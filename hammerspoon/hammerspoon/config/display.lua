function taskCallback(exitCode, stdOut, stdErr)
    print(exitCode, stdOut, stdErr)
end

function taskStreamCallback(task, stdOut, stdErr)
    return true
end

function runBrightnessControl(brightness)
    local task = hs.task.new("~/.local/bin/ddcctl", taskCallback, taskStreamCallback, {"-d", "1", "-b", brightness})
    if not task:start() then
        print("Task failed")
        return
    end
    task:waitUntilExit()
end


hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'Up', function()
    runBrightnessControl("20+")
end)

hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'Down', function()
    runBrightnessControl("20-")
end)
