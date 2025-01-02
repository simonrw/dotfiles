return {
    {
        "vim-test/vim-test",
        config = function()

            vim.g["test#python#runner"] = "pytest"
            vim.g["test#javascript#reactscripts#options"] = "--watchAll=false"
            vim.g["test#strategy"] = "basic"

            vim.keymap.set("n", "tf", function()
                vim.cmd("update")
                vim.cmd("TestFile")
            end, {noremap = true, silent = true})
            vim.keymap.set("n", "tl", function()
                vim.cmd("update")
                vim.cmd("TestLast")
            end, {noremap = true, silent = true})
            vim.keymap.set("n", "tn", function()
                vim.cmd("update")
                vim.cmd("TestNearest")
            end, {noremap = true, silent = true})
            vim.keymap.set("n", "ta", function()
                vim.cmd("update")
                vim.cmd("TestSuite")
            end, {noremap = true, silent = true})
            vim.keymap.set("n", "ts", function()
                vim.cmd("update")
                vim.cmd("TestSuite")
            end, {noremap = true, silent = true})
        end
    }
}
