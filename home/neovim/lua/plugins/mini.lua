return {
  {
    'echasnovski/mini.files',
    opts = {},
    keys = {
      { "-", function() require("mini.files").open() end, desc = "Show file browser" },
    },
  }
}
