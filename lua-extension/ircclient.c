#include <string.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "ircclient.h"

#define CALL(f, L, ...) \
  ircclient_##f(*(void**)lua_getextraspace(L), ##__VA_ARGS__)

static char module_key;

static command_cb command_hook_entry;
static message_cb message_hook_entry;

struct callback_node {
        lua_State *L;
        int myref;
        int funref;
};

int lib_writeline(lua_State *L)
{
        size_t len;
        const char *msg = luaL_checklstring(L, 1, &len);

        CALL(print, L, msg, len);
        return 0;
}

int lib_send(lua_State *L)
{
        size_t netlen, msglen;
        const char *net = luaL_checklstring(L, 1, &netlen);
        const char *msg = luaL_checklstring(L, 2, &msglen);
        luaL_checktype(L, 3, LUA_TNONE);

        int res = CALL(send, L, net, netlen, msg, msglen);
        if (res) {
                return luaL_error(L, "send failed");
        }

        return 0;
}

static int lib_hook_command(lua_State *L)
{
        size_t namelen, helplen;
        const char *name = luaL_checklstring(L, 1, &namelen);
        int priority = luaL_checkinteger(L, 2);
        // callback function #3
        const char *help = luaL_checklstring(L, 4, &helplen);

        lua_pushvalue(L, 3);
        int funref = luaL_ref(L, LUA_REGISTRYINDEX);

        struct callback_node *cb = lua_newuserdata(L, sizeof (struct callback_node));
        int ref = luaL_ref(L, LUA_REGISTRYINDEX);

        cb->myref = ref;
        cb->funref = funref;
        cb->L = L;

        hook_id hid = CALL(hook_command, L, name, namelen, priority, command_hook_entry, help, helplen, cb);
        lua_pushinteger(L, hid);

        return 1;
}

static int lib_hook_message(lua_State *L)
{
        size_t namelen;
        const char *name = luaL_checklstring(L, 1, &namelen);
        int priority = luaL_checkinteger(L, 2);
        // callback function #3

        lua_pushvalue(L, 3);
        int funref = luaL_ref(L, LUA_REGISTRYINDEX);
        struct callback_node *cb = lua_newuserdata(L, sizeof (struct callback_node));
        int ref = luaL_ref(L, LUA_REGISTRYINDEX);

        cb->myref = ref;
        cb->funref = funref;
        cb->L = L;

        hook_id hid = CALL(hook_message, L, name, namelen, priority, message_hook_entry, cb);
        lua_pushinteger(L, hid);

        return 1;
}

static int lib_unhook(lua_State *L) {
        hook_id hid = luaL_checkinteger(L, 1);

        struct callback_node *cb = CALL(unhook, L, hid);

        if (cb) {
                int ref = cb->myref;
                int funref = cb->funref;
                luaL_unref(L, LUA_REGISTRYINDEX, ref);
                luaL_unref(L, LUA_REGISTRYINDEX, funref);
        }

        return 0;
}

/***
Replacement for Lua's print function
@function print
@tparam string message Message to print to console
@usage print('This shows up on the * window')
*/
static int lib_print(lua_State *L)
{
        int n = lua_gettop(L);  /* number of arguments */

        luaL_Buffer b;
        luaL_buffinit(L, &b);

        lua_getglobal(L, "tostring");
        for (int i = 1; i <= n; i++) {
                lua_pushvalue(L, -1);  /* tostring */
                lua_pushvalue(L, i);   /* value to print */
                lua_call(L, 1, 1);

                if (!lua_isstring(L, -1)) {
                        return luaL_error(L, "'tostring' must return a string to 'print'");
                }
                if (i > 1) {
                        luaL_addchar(&b, '\t');
                }
                luaL_addvalue(&b);
        }

        luaL_pushresult(&b);
        size_t msglen;
        const char *msg = luaL_tolstring(L, -1, &msglen);
        CALL(print, L, msg, msglen);

        return 0;
}

static luaL_Reg irc_lib[] =
  { { "writeline", lib_writeline}
  , { "send"     , lib_send }
  , { "hook_command", lib_hook_command }
  , { "hook_message", lib_hook_message }
  , { "unhook", lib_unhook }
  , {}
  };

void setup_library(lua_State *L)
{
        luaL_newlib(L, irc_lib);
        lua_setglobal(L, "irc");

        lua_pushcfunction(L, lib_print);
        lua_setglobal(L, "print");
}

lua_State *startup_entry(void *X)
{
        lua_State *L = luaL_newstate();
        luaL_checkversion(L);
        luaL_openlibs(L);
        memcpy(lua_getextraspace(L), &X, sizeof(X));

        setup_library(L);

        int res = luaL_loadfile(L, "irc.lua") || lua_pcall(L, 0, 1, 0);
        if (res) {
                size_t errlen;
                const char *errmsg = lua_tolstring(L, -1, &errlen);
                CALL(print, L, errmsg, strlen(errmsg));
                lua_close(L);
                return NULL;
        }

        lua_rawsetp(L, LUA_REGISTRYINDEX, &module_key);

        return L;
}


void shutdown_entry(lua_State *L)
{
        lua_rawgetp(L, LUA_REGISTRYINDEX, &module_key);
        lua_getfield(L, -1, "shutdown");
        lua_remove(L, -2);

        lua_pcall(L, 0, 0, 0);

        lua_close(L);
}

static int message_hook_entry
  (const char *net, size_t net_len,
   const char *pfx, size_t pfx_len,
   const char *cmd, size_t cmd_len,
   const char **args, size_t *args_len,
   size_t args_n,
   void *user_data)
{
        struct callback_node * const cb = user_data;
        lua_State * const L = cb->L;

        // function
        lua_rawgeti(L, LUA_REGISTRYINDEX, cb->funref);

        lua_pushlstring(L, net, net_len); // argument 1
        lua_pushlstring(L, pfx, pfx_len); // argument 2
        lua_pushlstring(L, cmd, cmd_len); // argument 3

        lua_createtable(L, args_n, 0); // argument 4
        for (int i = 0; i < args_n; i++) {
                lua_pushlstring(L, args[i], args_len[i]);
                lua_rawseti(L, -2, i+1);
        }

        int ret = 0;

        // Call function
        if (lua_pcall(L, 4, 1, 0)) {
                // Error remains on stack
                size_t errlen;
                const char *errmsg = lua_tolstring(L, -1, &errlen);
                CALL(print, L, errmsg, errlen);
        } else {
                // Return value remains on stack
                ret = lua_tointeger(L, -1);
        }

        lua_pop(L, 1);
        return ret;
}

static int command_hook_entry
  (const char *word[],
   size_t *word_lens,
   const char *word_eol[],
   size_t *word_eol_lens,
   size_t args,
   void *user_data)
{
        struct callback_node * const cb = user_data;
        lua_State * const L = cb->L;

        // Function
        lua_rawgeti(L, LUA_REGISTRYINDEX, cb->funref);

        // Argument 1
        lua_createtable(L, args, 0);
        for (int i = 0; i < args; i++) {
                lua_pushlstring(L, word[i], word_lens[i]);
                lua_rawseti(L, -2, i+1);
        }

        // Argument 2
        lua_createtable(L, args, 0);
        for (int i = 0; i < args; i++) {
                lua_pushlstring(L, word_eol[i], word_eol_lens[i]);
                lua_rawseti(L, -2, i+1);
        }

        int ret = 0;

        // Call function with two arguments
        if (lua_pcall(L, 2, 1, 0)) {
                // Error remains on stack
                size_t errlen;
                const char *errmsg = lua_tolstring(L, -1, &errlen);
                CALL(print, L, errmsg, errlen);
        } else {
                // Return value remains on stack
                ret = lua_tointeger(L, -1);
                return ret;
        }

        lua_pop(L, 1);
        return ret;
}
