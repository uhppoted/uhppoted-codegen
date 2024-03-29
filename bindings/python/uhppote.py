import encode
import decode
import udp


class Uhppote:
    def __init__(self, bind='0.0.0.0', broadcast='255.255.255.255:60000', listen="0.0.0.0:60001", debug=False):
        self._udp = udp.UDP(bind, broadcast, listen, debug)

    def get_all_controllers(self):
        request = encode.get_controller_request(0)
        replies = self._udp.broadcast(request)

        list = []
        for reply in replies:
            list.append(decode.get_controller_response(reply))

        return list

    def listen(self, onEvent):
        self._udp.listen(lambda packet: self.events(packet, onEvent))
        return None


    def events(self, packet, onEvent):
        onEvent(decode.event(packet))
    
{{range .model.functions}}
{{- template "function" . -}}
{{end}}

{{define "function"}}
    def {{snakeCase .name}}(self, {{template "args" .args}}):
        {{if .response}}request = encode.{{snakeCase .request.name}}({{template "params" .args}})
        reply = self._udp.send(request)

        if reply != None:
            return decode.{{snakeCase .response.name}}(reply)
            
        return None
        {{- else}}
        request = encode.{{snakeCase .request.name}}({{template "params" .args}})
        self._udp.send(request)

        return True
        {{end}}
{{end}}
