import { describe, it } from 'mocha'
import { expect } from 'chai'
import * as decode from '../decode.js'

describe('decoder', function () {
  it('should decode get-status response', function () {
    const packet = [
      0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
      0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ]

    const expected = {
      controller: 405419896,
      eventIndex: 78,
      eventType: 2,
      eventAccessGranted: true,
      eventDoor: 3,
      eventDirection: 1,
      eventCard: 8165537,
      eventTimestamp: '2022-08-23 09:47:06',
      eventReason: 44,
      door1Open: false,
      door2Open: true,
      door3Open: false,
      door4Open: false,
      door1Button: false,
      door2Button: false,
      door3Button: false,
      door4Button: true,
      systemError: 3,
      systemDate: '2022-08-23',
      systemTime: '09:49:39',
      specialInfo: 39,
      relays: 7,
      inputs: 9,
      sequenceNo: 0
    }

    const response = decode.GetStatusResponse(packet)

    expect(response).to.deep.equal(expected)
  })

  it('should decode get-status response without an event', function () {
    const packet = [
      0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ]

    const expected = {
      controller: 405419896,
      eventIndex: 0,
      eventType: 0,
      eventAccessGranted: false,
      eventDoor: 0,
      eventDirection: 0,
      eventCard: 0,
      eventTimestamp: null,
      eventReason: 0,
      door1Open: false,
      door2Open: true,
      door3Open: false,
      door4Open: false,
      door1Button: false,
      door2Button: false,
      door3Button: false,
      door4Button: true,
      systemError: 3,
      systemDate: '2022-08-23',
      systemTime: '09:49:39',
      specialInfo: 39,
      relays: 7,
      inputs: 9,
      sequenceNo: 0
    }

    const response = decode.GetStatusResponse(packet)

    expect(response).to.deep.equal(expected)
  })
})
