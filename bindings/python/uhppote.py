from collections import namedtuple

import encode
import decode
import net

Controller = namedtuple('Controller', 'controller address transport')

class Uhppote:
    def __init__(self, bind='0.0.0.0', broadcast='255.255.255.255:60000', listen="0.0.0.0:60001", debug=False):
        self._net = net.Net(bind, broadcast, listen, debug)

    def get_all_controllers(self):
        request = encode.get_controller_request(0)
        replies = self._net.broadcast(request)

        list = []
        for reply in replies:
            list.append(decode.get_controller_response(reply))

        return list

    def listen(self, onEvent):
        self._net.listen(lambda packet: self.events(packet, onEvent))
        return None


    def events(self, packet, onEvent):
        onEvent(decode.event(packet))
    
{{range .model.functions}}
{{- template "function" . -}}
{{end}}

{{define "function"}}
    def {{snakeCase .name}}(self, {{template "args" .args}}):
        {{if .response}}request = encode.{{snakeCase .request.name}}(controller.controller{{template "params" slice .args 1}})
        reply = self._net.send(controller, request)

        if reply != None:
            return decode.{{snakeCase .response.name}}(reply)
            
        return None
        {{- else}}
        request = encode.{{snakeCase .request.name}}(controller.controller{{template "params" slice .args 1}})
        self._net.send(controller, request)

        return True
        {{end}}
{{end}}
