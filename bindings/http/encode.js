{{range .model.requests}}
{{- template "request" . -}}
{{end}}

{{define "request"}}
export function {{CamelCase .name}}({{template "args" .fields}}) {
  const request = new Uint8Array(64)
  const view = new DataView(request.buffer)

  request[0] = 0x17
  request[1] = {{byte2hex .msgtype}}
  {{range .fields -}}
  {{- if ne .type "magic"}}
  pack{{CamelCase .type}}({{camelCase .name}}, view, {{.offset}})
  {{- else}}
  packUint32(0x55aaaa55, view, {{.offset}})
  {{- end}}{{end}}

  return request
}
{{end}}

function packUint8(v, packet, offset) {
  packet.setUint8(offset, v)
}

function packUint16(v, packet, offset) {
  packet.setUint16(offset, v, true)
}

function packUint32(v, packet, offset) {
  packet.setUint32(offset, v, true)
}

function packBool(v, packet, offset) {
  if (v) {
    packet.setUint8(offset, 0x01)    
  } else {
    packet.setUint8(offset, 0x00)        
  }
}

function packIPv4(v, packet, offset) {
  const re = /([0-9]{0,3})\.([0-9]{0,3})\.([0-9]{0,3})\.([0-9]{0,3})/
  const match = v.match(re)

  if (!match || match.length !== 5) {
    throw new Error(`invalid IP address ${v}`)
  }

  for (let i = 0; i < 4; i++) {
    const b = Number(match[i + 1])
    if (Number.isNaN(b) || b > 255) {
      throw new Error(`invalid IP address ${v}`)
    } else {
      packet.setUint8(offset+i, b)
    }
  }
}

function packDate (v, packet, offset) {
  const match = /([0-9]{4})-([0-9]{2})-([0-9]{2})/.exec(v)

  if (match) {
    const year = match[1]
    const month = match[2]
    const day = match[3]

    const date = `${year}${month}${day}`
    const bytes = bcd2bin(`${date}`)

    packet.setUint8(offset+0, bytes[0])
    packet.setUint8(offset+1, bytes[1])
    packet.setUint8(offset+2, bytes[2])
    packet.setUint8(offset+3, bytes[3])
  }
}

function packDatetime (v, packet, offset) {
  const year = String(v.getFullYear()).padStart(4, '0')
  const month = String(v.getMonth() + 1).padStart(2, '0')
  const day = String(v.getDate()).padStart(2, '0')
  const hour = String(v.getHours()).padStart(2, '0')
  const minute = String(v.getMinutes()).padStart(2, '0')
  const second = String(v.getSeconds()).padStart(2, '0')

  const date = `${year}${month}${day}`
  const time = `${hour}${minute}${second}`
  const bytes = bcd2bin(`${date}${time}`)

  for (let i = 0; i < 7; i++) {
      packet.setUint8(offset+i, bytes[i])
  }
}

function packHHmm (v, packet, offset) {
  const match = /([0-9]{2}):([0-9]{2})/.exec(v)

  if (match) {
    const hour = match[1]
    const minute = match[2]

    const time = `${hour}${minute}`
    const bytes = bcd2bin(`${time}`)

    packet.setUint8(offset+0, bytes[0])
    packet.setUint8(offset+1, bytes[1])
  }
}

function packPin(v, packet, offset) {
  packet.setUint8(offset+0, (v >> 0) & 0x00ff)
  packet.setUint8(offset+1, (v >> 8) & 0x00ff)
  packet.setUint8(offset+2, (v >> 16) & 0x00ff)
}

function packMode(v, packet, offset) {
  packet.setUint8(offset, v)
}

function packTask(v, packet, offset) {
  packet.setUint8(offset, v)
}

function packInterlock(v, packet, offset) {
  packet.setUint8(offset, v)
}

function packAntiPassback(v, packet, offset) {
  packet.setUint8(offset, v)
}

function bcd2bin (bcd) {
  const bytes = []
  const matches = [...bcd.matchAll(/([0-9]{2})/g)]

  for (const m of matches) {
    const b = parseInt(m[0], 10)
    const byte = ((b / 10) << 4) | (b % 10)

    bytes.push(byte)
  }

  return bytes
}
