--- @param s string
--- @param workspace_path string
--- @return string
local function expand_path(s, workspace_path)
  if string.find(s, "${workspaceFolder:") then
    local startIndex, endIndex = string.find(s, "${workspaceFolder:")
    local endReplaceIndex = string.find(s, '}', endIndex)
    local subPath = string.sub(s, endIndex + 1, endReplaceIndex - 1)
    local replacee = workspace_path .. "/" .. subPath
    local pattern = string.sub(s, startIndex, endReplaceIndex)
    return string.sub(s, 0, startIndex - 1) .. replacee .. string.sub(s, endReplaceIndex + 1)
  elseif string.find(s, "${workspaceFolder}") then
    -- should not really happen but straightforward
    local replaced = string.gsub(s, "${workspaceFolder}", workspace_path)
    return replaced
  else
    -- no transform needed
    return s
  end
end

--- @param m table
--- @param workspace_path string
--- @return table
local function expand_path_mapping(m, workspace_path)
  local out = {}
  for key, value in pairs(m) do
    out[key] = expand_path(value, workspace_path)
  end
  return out
end

--- @param mappings table
--- @param workspace_path string
--- @return table
local function expand_path_mappings(mappings, workspace_path)
  local expanded_mappings = {}
  for _, mapping in ipairs(mappings) do
    table.insert(expanded_mappings, expand_path_mapping(mapping, workspace_path))
  end
  return expanded_mappings
end

--- @param env table
--- @param workspace_path string
--- @return table
local function expand_env(env, workspace_path)
  local out = {}
  for name, value in pairs(env) do
    local res = expand_path(value, workspace_path)
    out[name] = res
  end
  return out
end

--- @param config table
--- @param workspace_path string
--- @return table
local function expand_paths(config, workspace_path)
  local cwd = config.cwd
  if cwd then
    config.cwd = expand_path(cwd, workspace_path)
  end

  local pathMappings = config.pathMappings
  if pathMappings then
    config.pathMappings = expand_path_mappings(pathMappings, workspace_path)
  end

  local env = config.env
  if env then
    local res = expand_env(env, workspace_path)
    config.env = res
  end

  return config
end

--- @param jsonstr string
--- @param workspace_path string
--- @return table
local function load_workspace_json(jsonstr, workspace_path)
  local ok, obj = pcall(vim.json.decode, jsonstr, { luanil = { object = true } })
  if not ok then
    error("error decoding launch json")
  end

  assert(type(obj) == "table", "workspace must contain an object")

  local workspace_dir = vim.fs.normalize(vim.fs.dirname(workspace_path))

  local configs = {}
  for _, config in ipairs(obj.launch.configurations or {}) do
    local transformed = expand_paths(config, workspace_dir)
    table.insert(configs, transformed)
  end
  return configs
end

return {
  {
    'mfussenegger/nvim-dap-python',
    dependencies = {
      {
        'theHamsta/nvim-dap-virtual-text',
        opts = {},
        dependencies = {
          {
            'mfussenegger/nvim-dap',
            config = function()
              local dap = require("dap")
              local opts = function(desc)
                return { noremap = true, silent = true, desc = desc }
              end

              vim.keymap.set("n", "<leader>bb", function() dap.toggle_breakpoint() end, opts("Toggle breakpoint"))
              vim.keymap.set("n", "<leader>br", function() dap.repl.toggle() end, opts("Toggle repl"))
              vim.keymap.set("n", "<leader>bl", function() dap.run_last() end, opts("Run last debugging session"))
              vim.keymap.set("n", "<leader>bf", function()
                local widgets = require('dap.ui.widgets')
                widgets.centered_float(widgets.frames)
              end, opts("Show frames"))
              vim.keymap.set("n", "<leader>bs", function()
                local widgets = require('dap.ui.widgets')
                widgets.centered_float(widgets.scopes)
              end, opts("Show scopes"))

              -- map pycharm bindings
              vim.keymap.set({ "i", "n" }, "<F7>", function() dap.step_into() end, opts("Step into"))
              vim.keymap.set({ "i", "n" }, "<F8>", function() dap.step_over() end, opts("Step over"))
              vim.keymap.set({ "i", "n" }, "<S-F8>", function() dap.step_out() end, opts("Step out"))
              vim.keymap.set({ "i", "n" }, "<F9>", function() dap.continue() end, opts("Continue"))

              dap.adapters.codelldb = {
                type = "server",
                port = "${port}",
                executable = {
                  command = "codelldb",
                  args = { "--port", "${port}" },
                },
              }

              -- support parsing LocalStack code workspace file
              dap.providers.configs["localstack-workspace"] = function(bufnr)
                local resolved_path = vim.fn.getcwd() .. '/../localstack.code-workspace'
                if not vim.loop.fs_stat(resolved_path) then
                  return {}
                end

                local lines = {}
                for line in io.lines(resolved_path) do
                  if not vim.startswith(vim.trim(line), '//') then
                    table.insert(lines, line)
                  end
                end
                local contents = table.concat(lines, '\n')
                return load_workspace_json(contents, resolved_path)
              end
            end,
          },
        },
      },
    },
    config = function()
      require("dap-python").setup("python")

      -- add mapping to debug the current test
      vim.api.nvim_create_autocmd('FileType', {
        group = vim.api.nvim_create_augroup('python-debug-test', { clear = true }),
        pattern = { 'python' },
        callback = function(event)
          vim.keymap.set('n', '<leader>bm', function() require('dap-python').test_method() end,
            { noremap = true, silent = true, buffer = event.buf, desc = "Debug python test" })
        end,
      })
    end,
  },
}
