return {
  cmd = { "p4spectec-lsp" },
  filetypes = { "watsup" },
  root_dir = function(bufnr, on_dir)
    local root = vim.fs.root(bufnr, function(name, path)
      if not name:match("%.spec$") then
        return false
      end
      local stat = vim.uv.fs_stat(vim.fs.joinpath(path, name))
      return stat ~= nil and stat.type == "file"
    end)
    on_dir(root or vim.fs.root(bufnr, ".git") or vim.fs.dirname(vim.api.nvim_buf_get_name(bufnr)))
  end,
}
