
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

p = {
  processId = 1337,
  rootUri = 'file:///home/lain/',
  capabilities = capabilities,
}

bytes = msgbytes("initialize", p, 0)
fil = io.open("/tmp/filen", "wb")
fil:write(bytes)
fil:close()
