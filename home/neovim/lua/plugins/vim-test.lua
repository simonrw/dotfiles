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
            end, {noremap = true, silent = true, desc = "Test the current file"})
            vim.keymap.set("n", "tl", function()
                vim.cmd("update")
                vim.cmd("TestLast")
            end, {noremap = true, silent = true, desc = "Run the last executed test"})
            vim.keymap.set("n", "tn", function()
                vim.cmd("update")
                vim.cmd("TestNearest")
            end, {noremap = true, silent = true, desc = "Run the nearest test"})
            vim.keymap.set("n", "ta", function()
                vim.cmd("update")
                vim.cmd("TestSuite")
            end, {noremap = true, silent = true, desc = "Run the whole test suite"})

            -- custom command to run test on AWS (LocalStack test)
            vim.api.nvim_create_user_command("AwsTestNearest", function() 
              vim.cmd([[TestNearest TEST_TARGET=AWS_CLOUD AWS_PROFILE=ls-sandbox SNAPSHOT_UPDATE=1]])
            end, {})
        end
    }
}
