import * as encode from './encode.js'
import * as decode from './decode.js'
import * as udp from './udp.js'

export function GetAllControllers () {
  const request = encode.GetControllerRequest(0)

  return udp.broadcast(request)
    .then(replies => {
      const list = []

      for (const reply of replies) {
        list.push(decode.GetControllerResponse(reply))
      }

      return list
    })
}
{{range .model.functions}}
{{- template "function" . -}}
{{end}}

{{define "function"}}
export function {{CamelCase .name}}({{template "args" .args}}) { {{if .response}}
  const request = encode.{{CamelCase .request.name}}({{template "params" .args}})

  return udp.send(request, '0s')
    .then(replies => {
      if (replies.length > 0) {
        return decode.{{CamelCase .response.name}}(replies[0])
      }

      return null
    }){{else}}
  const request = encode.{{CamelCase .request.name}}({{template "params" .args}})

  return udp.send(request, '0.1ms')
    .then(replies => {
      return true
    }){{end}}
}
{{end}}
