local function is_dark_mode()
    local cmd = "defaults read -g AppleInterfaceStyle >/dev/null 2>&1"
    return os.execute(cmd) == 0
end

vim.g.get_is_dark_mode = is_dark_mode
vim.g.is_dark_mode = is_dark_mode()
