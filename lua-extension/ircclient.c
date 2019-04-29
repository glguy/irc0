#include <string.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "ircclient.h"

int lib_add_message(lua_State *L) {
        size_t len;
        const char *msg = luaL_checklstring(L, 1, &len);

        void *X;
        memcpy(&X, lua_getextraspace(L), sizeof(X));

        ircclient_add_message(X, msg, len);
        return 0;
}


lua_State *startup_entry(void *X) {
        lua_State *L = luaL_newstate();
        luaL_checkversion(L);
        luaL_openlibs(L);
        memcpy(lua_getextraspace(L), &X, sizeof(X));

        lua_pushcfunction(L, lib_add_message);
        lua_setglobal(L, "add_message");

        const char *msg = "Lua bundle loaded";
        ircclient_add_message(X, msg, strlen(msg));

        return L;
}


void shutdown_entry(lua_State *L) {
        lua_close(L);
}

void command_entry(lua_State *L, const char *cmd, size_t cmd_len) {
        void *X;
        memcpy(&X, lua_getextraspace(L), sizeof(X));
}

int message_entry(lua_State *L, const char *msg, size_t msg_len) {
        void *X;
        memcpy(&X, lua_getextraspace(L), sizeof(X));
        
        ircclient_add_message(X, msg, msg_len);

        return 0;
}