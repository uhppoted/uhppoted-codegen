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
        c = resolve(controller)

        {{if .response}}request = encode.{{snakeCase .request.name}}(c.controller{{template "params" slice .args 1}})
        reply = self._net.send(c, request)

        if reply != None:
            return decode.{{snakeCase .response.name}}(reply)
            
        return None
        {{- else}}
        request = encode.{{snakeCase .request.name}}(c.controller, {{template "params" slice .args 1}})
        self._net.send(c, request)

        return True
        {{end}}
{{end}}

def resolve(v):
    if isinstance(v, int):
        return Controller(v, None, 'udp')

    if isinstance(v, tuple):
        controller = v[0]
        address = None
        protocol = 'udp'

        if len(v) > 1 and isinstance(v[1], str):
            address = v[1]

        if len(v) > 2 and (v[2] == 'tcp' or v[2] == 'TCP'):
            protocol = 'tcp'

        return Controller(controller, address, protocol)

    return Controller(None, None, 'udp')
