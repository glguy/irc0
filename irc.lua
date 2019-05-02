irc.writeline('File works')

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
