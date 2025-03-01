
function msgbytes(method, params, id)
  local msg = {jsonrpc = "2.0", method = method, params = params, id = id}
  local bytes = a.nvim_call_function('json_encode', {msg})
  return 'Content-Length: ' .. bytes:len() ..'\r\n\r\n' ..bytes
end

capabilities = {
  textDocument = {
    publishDiagnostics={relatedInformation=true},
  },
  positionEncoding = {'utf-8', 'utf-16'},
}

init_p = {
  processId = 1337,
  rootUri = 'file:///home/lain/',
  capabilities = capabilities,
}


open_p = { textDocument = {
  uri='file:///home/lain/notes.html',
  text='<html>foooo</hml>' ,
  version=1,
  languageId='html'
} }

fil = io.open("test/lsp_init.json_rpc", "wb")
fil:write(msgbytes("initialize", init_p, 0))
fil:write(msgbytes("initialized", vim.empty_dict(), nil))  -- do not ask
fil:write(msgbytes("textDocument/didOpen", open_p, nil))
fil:close()
