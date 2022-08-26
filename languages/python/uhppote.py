import encode
import decode
import udp


class Uhppote:
    def __init__(self, bind='0.0.0.0', broadcast='255.255.255.255:60000', debug=False):
        self._udp = udp.UDP(bind, broadcast, debug)

    def get_all_controllers(self):
        request = encode.get_controller_request(0)
        replies = self._udp.send(request, udp.read_all)

        list = []
        for reply in replies:
            list.append(decode.get_controller_response(reply))

        return list

{{range .model.functions}}{{template "function" .}}
{{end}}

{{define "function"}}
    def {{snakeCase .name}}(self, {{template "args" .args}}):
        {{if .response}}request = encode.{{snakeCase .request.name}}({{template "params" .args}})
        replies = self._udp.send(request, udp.read)

        for reply in replies:
            return decode.{{snakeCase .response.name}}(reply)

        return None
        {{else}}request = encode.{{snakeCase .request.name}}({{template "params" .args}})
        self._udp.send(request, udp.read_none)

        return True
        {{end}}
{{end}}
