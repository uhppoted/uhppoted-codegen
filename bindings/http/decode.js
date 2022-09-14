{{- range .model.responses}}
{{- template "decode" . -}}
{{end -}}

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

  return parseYYYYMMDD(datetime)
}

function unpackShortdate(packet, offset) {
  const bytes = new Uint8Array(packet.buffer.slice(offset, offset + 3))
  const datetime = '20' + bcd(bytes)

  return parseYYYYMMDD(datetime)
}

function unpackOptionalDate(packet, offset) {
  const bytes = new Uint8Array(packet.buffer.slice(offset, offset + 4))
  const datetime = bcd(bytes)

  try {
    return parseYYYYMMDD(datetime)
  } catch {
    return null
  }
}

function unpackDatetime(packet, offset) {
  const bytes = new Uint8Array(packet.buffer.slice(offset, offset + 7))
  const datetime = bcd(bytes)

  return parseYYYYMMDDHHmmss(datetime)
}

function unpackOptionalDatetime(packet, offset) {
  const bytes = new Uint8Array(packet.buffer.slice(offset, offset + 7))
  const datetime = bcd(bytes)

  try {
    return parseYYYYMMDDHHmmss(datetime)
  } catch {
    return null
  }
}

function unpackTime(packet, offset) {
  const bytes = new Uint8Array(packet.buffer.slice(offset, offset + 3))
  const time = bcd(bytes)

  return `${time.substr(0, 2)}:${time.substr(2, 2)}:${time.substr(4, 2)}`
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

function parseYYYYMMDD(s) {
  if (!/[0-9]{8}/.test(s)) {
    throw new Error(`invalid date value ${s}`)
  }

  const year = parseInt(s.substr(0,4))
  const month = parseInt(s.substr(4,2))
  const day = parseInt(s.substr(6,2))

  if ((year < 2000 || year > 3000) || (month < 1 || month > 12) || (day < 1 || day > 31)) {
    throw new Error(`invalid date value ${s}`)   
  }

  const date = new Date()
  date.setFullYear(year)
  date.setMonth(month - 1)
  date.setDate(day)
  date.setHours(0)
  date.setMinutes(0)
  date.setSeconds(0)
  date.setMilliseconds(0)

  return date
}

function parseYYYYMMDDHHmmss(s) {
  if (!/[0-9]{14}/.test(s)) {
    throw new Error(`invalid datetime value ${s}`)
  }

  const year = parseInt(s.substr(0,4))
  const month = parseInt(s.substr(4,2))
  const day = parseInt(s.substr(6,2))
  const hours = parseInt(s.substr(8,2))
  const minutes = parseInt(s.substr(10,2))
  const seconds = parseInt(s.substr(12,2))

  if ((year < 2000 || year > 3000) || (month < 1 || month > 12) || (day < 1 || day > 31)) {
    throw new Error(`invalid datetime value ${s}`)   
  }

  if (hours > 24 || minutes > 60 || seconds > 60) {
    throw new Error(`invalid datetime value ${s}`)
  }

  const date = new Date()
  date.setFullYear(year)
  date.setMonth(month - 1)
  date.setDate(day)
  date.setHours(hours)
  date.setMinutes(minutes)
  date.setSeconds(seconds)
  date.setMilliseconds(0)

  return date
}
