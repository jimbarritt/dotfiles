-- Set working directory to git root on startup
local function set_git_root()
  local git_root = vim.fn.systemlist("git rev-parse --show-toplevel")[1]
  if vim.v.shell_error == 0 and git_root ~= "" then
    vim.cmd("cd " .. git_root)
  end
end
set_git_root()
