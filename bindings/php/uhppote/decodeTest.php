<?php

declare(strict_types=1);

include "decode.php";

use PHPUnit\Framework\TestCase;

final class DecodeTest extends TestCase
{
    public function testDecodeGetStatus(): void
    {
        $packet = array(
            0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
            0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        );

        $response = get_status_response($packet);

        $this->assertSame($response->controller, 405419896);
        $this->assertSame($response->system_date, "2022-08-23");
        $this->assertSame($response->system_time, "09:49:39");
        $this->assertSame($response->door_1_open, false);
        $this->assertSame($response->door_2_open, true);
        $this->assertSame($response->door_3_open, false);
        $this->assertSame($response->door_4_open, false);
        $this->assertSame($response->door_1_button, false);
        $this->assertSame($response->door_2_button, false);
        $this->assertSame($response->door_3_button, false);
        $this->assertSame($response->door_4_button, true);
        $this->assertSame($response->relays, 7);
        $this->assertSame($response->inputs, 9);
        $this->assertSame($response->system_error, 3);
        $this->assertSame($response->special_info, 39);
        $this->assertSame($response->event_index, 78);
        $this->assertSame($response->event_type, 2);
        $this->assertSame($response->event_access_granted, true);
        $this->assertSame($response->event_door, 3);
        $this->assertSame($response->event_direction, 1);
        $this->assertSame($response->event_card, 8165537);
        $this->assertSame($response->event_timestamp, "2022-08-23 09:47:06");
        $this->assertSame($response->event_reason, 44);
        $this->assertSame($response->sequence_no, 0);
    }

    public function testDecodeGetStatusWithNoEvent(): void
    {
        $packet = array(
            0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        );

        $response = get_status_response($packet);

        $this->assertSame($response->controller, 405419896);
        $this->assertSame($response->system_date, "2022-08-23");
        $this->assertSame($response->system_time, "09:49:39");
        $this->assertSame($response->door_1_open, false);
        $this->assertSame($response->door_2_open, true);
        $this->assertSame($response->door_3_open, false);
        $this->assertSame($response->door_4_open, false);
        $this->assertSame($response->door_1_button, false);
        $this->assertSame($response->door_2_button, false);
        $this->assertSame($response->door_3_button, false);
        $this->assertSame($response->door_4_button, true);
        $this->assertSame($response->relays, 7);
        $this->assertSame($response->inputs, 9);
        $this->assertSame($response->system_error, 3);
        $this->assertSame($response->special_info, 39);
        $this->assertSame($response->event_index, 0);
        $this->assertSame($response->event_type, 0);
        $this->assertSame($response->event_access_granted, false);
        $this->assertSame($response->event_door, 0);
        $this->assertSame($response->event_direction, 0);
        $this->assertSame($response->event_card, 0);
        $this->assertSame($response->event_timestamp, "");
        $this->assertSame($response->event_reason, 0);
        $this->assertSame($response->sequence_no, 0);
    }
}
