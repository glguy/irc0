
local function eval(args, args_eol)
        local chunk, err = load(args_eol[1], '=(eval)', 't')
        if chunk then
                local results = {chunk()}
                if #results ~= 0 then
                        print(table.unpack(results))
                end
        else
                irc.writeline('Chunk failed: ' .. err)
                error(err, 0)
        end
end
irc.hook_command('eval', 0, eval, 'evaluate some Lua')

local function shout(args)
    irc.writeline(table.concat(args, ':') .. '!')
end

local shout_id = irc.hook_command('shout', 0, shout, 'get excited')

local function msg(args, args_eol)
    irc.writeline('PRIVMSG ' .. args[1] .. ' :' .. args_eol[2])
end

irc.hook_command('msg', 0, msg, 'send message')

local function noshout(args)
    if shout_id then
            irc.unhook(shout_id)
            shout_id = nil
    end
end

irc.hook_command('noshout', 0, noshout, 'no more get excited')

local function onNotice(net, prefix, cmd, args)
        irc.writeline(net .. '> ' .. prefix .. ': ' .. cmd .. ' ' .. table.concat(args, '/'))
        return 1
end

irc.hook_message('NOTICE', 0, onNotice)

local M = {}

function M.shutdown() end

irc.writeline('File works')

return M
