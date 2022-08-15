export function GetController(deviceID) {
  const request = new Uint8Array(64)
  const view = new DataView(request.buffer)

  request[0] = 0x17
  request[1] = 0x94

  view.setUint32(4, deviceID, true)

  return request
}