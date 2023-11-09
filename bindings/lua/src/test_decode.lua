luaunit = require('luaunit')

local decode = require("src/decode")

function testDecodeGetStatus()
    -- stylua: ignore start    
    local packet = { 
            0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
            0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    }
    -- stylua: ignore end

    local reply = string.char(table.unpack(packet))
    local response = decode.get_status_response(reply)

    luaunit.assertEquals(response['controller'], 405419896)
    luaunit.assertEquals(response['system_date'], '2022-08-23')
    luaunit.assertEquals(response['system_time'], '09:49:39')
    luaunit.assertEquals(response['door_1_open'], false)
    luaunit.assertEquals(response['door_2_open'], true)
    luaunit.assertEquals(response['door_3_open'], false)
    luaunit.assertEquals(response['door_4_open'], false)
    luaunit.assertEquals(response['door_1_button'], false)
    luaunit.assertEquals(response['door_2_button'], false)
    luaunit.assertEquals(response['door_3_button'], false)
    luaunit.assertEquals(response['door_4_button'], true)
    luaunit.assertEquals(response['relays'], 7)
    luaunit.assertEquals(response['inputs'], 9)
    luaunit.assertEquals(response['system_error'], 3)
    luaunit.assertEquals(response['special_info'], 39)
    luaunit.assertEquals(response['event_index'], 78)
    luaunit.assertEquals(response['event_type'], 2)
    luaunit.assertEquals(response['event_access_granted'], true)
    luaunit.assertEquals(response['event_door'], 3)
    luaunit.assertEquals(response['event_direction'], 1)
    luaunit.assertEquals(response['event_card'], 8165537)
    luaunit.assertEquals(response['event_timestamp'], '2022-08-23 09:47:06')
    luaunit.assertEquals(response['event_reason'], 44)
    luaunit.assertEquals(response['sequence_no'], 0)
end

function testDecodeGetStatusWithNoEvent()
    -- stylua: ignore start    
    local packet = { 
            0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    }
    -- stylua: ignore end

    local reply = string.char(table.unpack(packet))
    local response = decode.get_status_response(reply)

    luaunit.assertEquals(response['controller'], 405419896)
    luaunit.assertEquals(response['system_date'], '2022-08-23')
    luaunit.assertEquals(response['system_time'], '09:49:39')
    luaunit.assertEquals(response['door_1_open'], false)
    luaunit.assertEquals(response['door_2_open'], true)
    luaunit.assertEquals(response['door_3_open'], false)
    luaunit.assertEquals(response['door_4_open'], false)
    luaunit.assertEquals(response['door_1_button'], false)
    luaunit.assertEquals(response['door_2_button'], false)
    luaunit.assertEquals(response['door_3_button'], false)
    luaunit.assertEquals(response['door_4_button'], true)
    luaunit.assertEquals(response['relays'], 7)
    luaunit.assertEquals(response['inputs'], 9)
    luaunit.assertEquals(response['system_error'], 3)
    luaunit.assertEquals(response['special_info'], 39)
    luaunit.assertEquals(response['event_index'], 0)
    luaunit.assertEquals(response['event_type'], 0)
    luaunit.assertEquals(response['event_access_granted'], false)
    luaunit.assertEquals(response['event_door'], 0)
    luaunit.assertEquals(response['event_direction'], 0)
    luaunit.assertEquals(response['event_card'], 0)
    luaunit.assertEquals(response['event_timestamp'], '')
    luaunit.assertEquals(response['event_reason'], 0)
    luaunit.assertEquals(response['sequence_no'], 0)
end

os.exit( luaunit.LuaUnit.run() )