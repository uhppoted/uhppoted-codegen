<?php

{{range .model.requests}}
{{- template "request" . -}}
{{end}}

{{define "request"}}
function {{snakeCase .name}}({{template "args" .fields}})
{
    $packet = array_fill(0, 64, 0x00);

    $packet[0] = 0x17;
    $packet[1] = {{byte2hex .msgtype}};
    
    {{range .fields }}
    {{- if ne .type "magic"}}$packet = pack_{{.type}}(${{snakeCase .name}}, $packet, {{.offset}});
    {{ else -}}
    $packet = pack_uint32(0x55aaaa55, $packet, {{.offset}});
    {{end}}{{end}}
    return $packet;
}
{{end}}

function pack_uint8($v, $packet, $offset)
{
    $packet[$offset] = ($v >> 0)  & 0x00ff;

    return $packet;
}

function pack_uint16($v, $packet, $offset)
{
    $packet[$offset]   = ($v >> 0)  & 0x00ff;
    $packet[$offset+1] = ($v >> 8)  & 0x00ff;

    return $packet;
}

function pack_uint32($v, $packet, $offset)
{
    $packet[$offset]   = ($v >> 0)  & 0x00ff;
    $packet[$offset+1] = ($v >> 8)  & 0x00ff;
    $packet[$offset+2] = ($v >> 16) & 0x00ff;
    $packet[$offset+3] = ($v >> 24) & 0x00ff;

    return $packet;
}

function pack_bool($v, $packet, $offset)
{
    $packet[$offset] = $v ? 0x01 : 0x00;

    return $packet;
}

function pack_IPv4($address, $packet, $offset)
{
    $addr = ip2long($address);

    if (!$addr) {
        throw new Exception(sprintf('invalid IPv4 address (%)', $address));
    }

    $packet[$offset]   = ($addr >> 24) & 0x00ff;
    $packet[$offset+1] = ($addr >> 16) & 0x00ff;
    $packet[$offset+2] = ($addr >> 8)  & 0x00ff;
    $packet[$offset+3] = ($addr >> 0)  & 0x00ff;

    return $packet;
}

function pack_date($v, $packet, $offset)
{
    $date = date_format($v,'Ymd');
    $bcd = string2bcd($date);

    $packet[$offset]   = $bcd[0];
    $packet[$offset+1] = $bcd[1];
    $packet[$offset+2] = $bcd[2];
    $packet[$offset+3] = $bcd[3];

    return $packet;
}

function pack_HHmm($v, $packet, $offset)
{
    $time = date_format($v,'Hi');
    $bcd = string2bcd($time);

    $packet[$offset]   = $bcd[0];
    $packet[$offset+1] = $bcd[1];

    return $packet;
}

function pack_datetime($v, $packet, $offset)
{
    $datetime = date_format($v,'YmdHis');
    $bcd = string2bcd($datetime);

    $packet[$offset]   = $bcd[0];
    $packet[$offset+1] = $bcd[1];
    $packet[$offset+2] = $bcd[2];
    $packet[$offset+3] = $bcd[3];
    $packet[$offset+4] = $bcd[4];
    $packet[$offset+5] = $bcd[5];
    $packet[$offset+6] = $bcd[6];

    return $packet;
}

function pack_pin($v, $packet, $offset)
{
    $packet[$offset]   = ($v >> 0)  & 0x00ff;
    $packet[$offset+1] = ($v >> 8)  & 0x00ff;
    $packet[$offset+2] = ($v >> 16) & 0x00ff;

    return $packet;
}

function pack_mode($v, $packet, $offset)
{
    $packet[$offset] = ($v >> 0)  & 0x00ff;

    return $packet;
}

function pack_task($v, $packet, $offset)
{
    $packet[$offset] = ($v >> 0)  & 0x00ff;

    return $packet;
}

function pack_interlock($v, $packet, $offset)
{
    $packet[$offset] = ($v >> 0)  & 0x00ff;

    return $packet;
}

function string2bcd($s)
{
    return array_values(unpack('C*',pack("H*", $s)));
}


