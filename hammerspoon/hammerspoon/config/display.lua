displays = {1, 2}

function taskCallback(exitCode, stdOut, stdErr)
    print(exitCode, stdOut, stdErr)
end

function taskStreamCallback(task, stdOut, stdErr)
    return true
end

function runBrightnessControl(brightness)
    local tasks = {}
    for i, display in pairs(displays) do
        local task = hs.task.new("~/.local/bin/ddcctl", taskCallback, taskStreamCallback, {"-d", tostring(display), "-b", brightness})
        if not task:start() then
            print("Task failed")
            return
        end
        tasks[i] = task
    end

    for _, task in pairs(tasks) do
        task:waitUntilExit()
    end
end


hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'Up', function()
    runBrightnessControl("20+")
end)

hs.hotkey.bind({'cmd', 'alt', 'ctrl'}, 'Down', function()
    runBrightnessControl("20-")
end)
