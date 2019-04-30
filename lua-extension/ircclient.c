#include <string.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "ircclient.h"

#define CALL(f, L, ...) \
  ircclient_##f(*(void**)lua_getextraspace(L), ##__VA_ARGS__)

static char module_key;

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

static luaL_Reg irc_lib[] =
  { { "writeline", lib_print}
  , { "send"     , lib_send }
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

void command_entry(lua_State *L, const char *cmd, size_t cmd_len)
{
        void *X;
        memcpy(&X, lua_getextraspace(L), sizeof(X));
}

int message_entry(lua_State *L, const char *msg, size_t msg_len)
{
        void *X;
        memcpy(&X, lua_getextraspace(L), sizeof(X));

        CALL(print, L, msg, msg_len);

        return 0;
}
