{{range .model.responses}}{{template "decode" .}}
{{end}}

{{define "decode"}}
export function {{CamelCase .name}}(packet) {
  const buffer = new Uint8Array(packet)
  const view = new DataView(buffer.buffer)

  if (buffer.length !== 64) {
    throw new Error(`invalid reply packet length (${buffer.length})`)
  }

  if (buffer[1] !== {{byte2hex .msgtype}}) {
    throw new Error(`invalid reply function code (${buffer[1].toString(16).padStart(2,'0')})`)
  }

  return { {{range .fields}}
    {{camelCase .name}}: unpack{{CamelCase .type}}(view, {{.offset}}),{{end}}
  }
}
{{end}}

function unpackUint8(packet, offset) {
  return packet.getUint8(offset)
}

function unpackUint16(packet, offset) {
  return packet.getUint16(offset, true)
}

function unpackUint32(packet, offset) {
  return packet.getUint32(offset, true)
}

function unpackBool(packet, offset) {
  return packet.getUint8(offset) !== 0x00
}

function unpackIPv4(packet, offset) {
  const bytes = new Uint8Array(packet.buffer.slice(offset, offset + 4))

  return [...bytes].map(x => x.toString(10)).join('.')
}

function unpackMAC(packet, offset) {
  const bytes = new Uint8Array(packet.buffer.slice(offset, offset + 6))
  
  return [...bytes].map(x => x.toString(16).padStart(2, '0')).join(':')
}

function unpackVersion(packet, offset) {
  const major = packet.getUint8(offset).toString(16)
  const minor = packet.getUint8(offset+1).toString(16).padStart(2, '0')

  return `v${major}.${minor}`
}

function unpackDate(packet, offset) {
  const bytes = new Uint8Array(packet.buffer.slice(offset, offset + 4))
  const datetime = bcd(bytes)

  if (datetime === '00000000') {
    return ''
  }

  const date = `${datetime.substr(0, 4)}-${datetime.substr(4, 2)}-${datetime.substr(6, 2)}`

  return `${date}`
}

function unpackShortdate(packet, offset) {
  const bytes = new Uint8Array(packet.buffer.slice(offset, offset + 3))
  const datetime = bcd('20' + bytes)

  if (datetime === '20000000') {
    return ''
  }

  const date = `${datetime.substr(0, 4)}-${datetime.substr(4, 2)}-${datetime.substr(6, 2)}`

  return `${date}`
}

function unpackDatetime(packet, offset) {
  const bytes = new Uint8Array(packet.buffer.slice(offset, offset + 7))
  const datetime = bcd(bytes)

  if (datetime === '00000000000000') {
    return ''
  }

  const date = `${datetime.substr(0, 4)}-${datetime.substr(4, 2)}-${datetime.substr(6, 2)}`
  const time = `${datetime.substr(8, 2)}:${datetime.substr(10, 2)}:${datetime.substr(12, 2)}`

  return `${date} ${time}`
}

function unpackTime(packet, offset) {
  const bytes = new Uint8Array(packet.buffer.slice(offset, offset + 3))
  const datetime = bcd(bytes)

  if (datetime === '000000') {
    return ''
  }

  const time = `${datetime.substr(8, 2)}:${datetime.substr(10, 2)}:${datetime.substr(12, 2)}`

  return `${time}`
}

function unpackHHmm(packet, offset) {
  const bytes = new Uint8Array(packet.buffer.slice(offset, offset + 2))
  const datetime = bcd(bytes)

  if (datetime === '0000') {
    return ''
  }

  const time = `${datetime.substr(8, 2)}:${datetime.substr(10, 2)}}`

  return `${time}`
}

function bcd (bytes) {
  return [...bytes].map(x => [(x >>> 4) & 0x0f, (x >>> 0) & 0x0f]).flat().join('')
}
