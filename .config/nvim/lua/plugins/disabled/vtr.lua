return {
  {
    "christoomey/vim-tmux-runner",
    config = function()
      vim.keymap.set("n", "<leader>V", function() vim.cmd([[ VtrAttachToPane ]]) end,
        { noremap = true, silent = true, desc = "Choose which tmux pane to attach to" })

      vim.api.nvim_create_user_command("V", "VtrSendCommandToRunner <args>", {
        complete = "shellcmd",
        force = true,
        nargs = "*",
      })
    end,
  }
}
