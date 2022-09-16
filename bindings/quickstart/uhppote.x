function get_all_controllers() {
    request = encode::get_controller_request(0)
    replies, err := udp::broadcast(request)

    return replies.map(reply => decoder::get_controller_response(reply))
}

function listen(events function, errors function, interrupt signal) error {
    onEvent = function(msg) {
        events(decoder::event(msg))
    }

    onError = function(error) {
        errors(error)
    }

    udp::listen(onEvent, onError, interrupt)
}

{{- range .model.functions }}
{{ template "function" . -}}
{{end}}

{{define "function"}}
func {{snakeCase .name}}({{template "args" .args}}) {
    request  = encoder::{{snakeCase .request.name}}({{template "params" .args}})
    reply    = udp::send(request)
    {{if .response}}
    return decoder::{{snakeCase .response.name}}(reply)
    {{- else}}
    return None
    {{- end}}
}{{end}}
