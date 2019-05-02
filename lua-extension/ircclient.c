#include <string.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "ircclient.h"

#define CALL(f, L, ...) \
  ircclient_##f(*(void**)lua_getextraspace(L), ##__VA_ARGS__)

static char module_key;

static command_cb command_hook_entry;

struct callback_node {
        lua_State *L;
        int myref;
        int funref;
};

int lib_print(lua_State *L)
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

static luaL_Reg irc_lib[] =
  { { "writeline", lib_print}
  , { "send"     , lib_send }
  , { "hook_command", lib_hook_command }
  , { "unhook", lib_unhook }
  , {}
  };

void setup_library(lua_State *L)
{
        luaL_newlib(L, irc_lib);
        lua_setglobal(L, "irc");
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
        lua_close(L);
}

int message_entry(lua_State *L, const char *msg, size_t msg_len)
{
        void *X;
        memcpy(&X, lua_getextraspace(L), sizeof(X));

        CALL(print, L, msg, msg_len);

        return 0;
}

static void command_hook_entry
  (const char *word[],
   size_t *word_lens,
   const char *word_eol[],
   size_t *word_eol_lens,
   size_t args,
   void *user_data)
{
        struct callback_node * const cb = user_data;
        lua_State * const L = cb->L;

        lua_rawgeti(L, LUA_REGISTRYINDEX, cb->funref);

        lua_createtable(L, args, 0);
        for (int i = 0; i < args; i++) {
                lua_pushlstring(L, word[i], word_lens[i]);
                lua_rawseti(L, -2, i+1);
        }

        lua_createtable(L, args, 0);
        for (int i = 0; i < args; i++) {
                lua_pushlstring(L, word_eol[i], word_eol_lens[i]);
                lua_rawseti(L, -2, i+1);
        }

        if (lua_pcall(L, 2, 0, 0)) {
                lua_pop(L, 1);
        }
}
