local intr = require('config/introspection')

local applications = {
    terminal = {
        name = "Ghostty",
    },
    browser = {
        name = "Helium",
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
        name = "Obsidian",
    },
    editor = {
        name = "Zed",
    },
}

if intr.is_work() then
    applications.notes.name = "Notes"
end

return applications
