-- local intr = require('config/introspection')

local browser_name = 'Helium'
-- if intr.hostname() == 'mba' or intr.hostname() == 'Simon’s MacBook Air' then
--     -- Helium has some performance problems with Amazon on macOS 27
--     -- browser_name = 'Safari'
-- end

local applications = {
    terminal = {
        name = "Ghostty",
    },
    browser = {
        name = browser_name,
    },
    chat = {
        name = "Slack",
    },
    linear = {
        name = "Linear",
    },
    todo = {
        name = "Reminders",
    },
    notes = {
        name = "Emacs",
    },
    editor = {
        name = "Zed",
    },
    agent = {
        name = "t3 code (nightly)",
    },
}

return applications
