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

  return udp.send(request)
    .then(reply => {
      return reply ? decode.{{CamelCase .response.name}}(reply) : null        
    }){{else}}
  const request = encode.{{CamelCase .request.name}}({{template "params" .args}})

  return udp.send(request, true)
    .then(() => {
      return true
    }){{end}}
}
{{end}}
