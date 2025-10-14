<?php

{{- range .model.responses}}
{{- template "response" . -}}
{{end}}

{{range .model.responses}}
{{- template "decode" . -}}
{{end}}

{{- with .model.event}}
{{- template "decode" . -}}
{{end}}

{{- define "response"}}
class {{CamelCase .name}}
{   {{range .fields}}
    public ${{snakeCase .name}};{{end}}
}
{{end}}

{{- with .model.event -}}
class {{CamelCase .name}}
{   {{range .fields}}
    public ${{snakeCase .name}};{{end}}
}
{{end}}

{{define "decode"}}
function {{snakeCase .name}}($packet)
{
    if (count($packet) != 64) {
        throw new Exception(sprintf('invalid reply packet length (%d)', count($packet)));
    }

    // Ref. v6.62 firmware event
    if ($packet[0] != 0x17 && ($packet[0] != 0x19 || $packet[1] != 0x20)) {
        throw new Exception(sprintf('invalid reply start of message byte (%02x)', $packet[0]));
    }

    if ($packet[1] != {{byte2hex .msgtype}}) {
        throw new Exception(sprintf('invalid reply function code (%02x)', $packet[1]));
    }

    $response = new {{CamelCase .name}}();
 
    {{range .fields}}
    $response->{{snakeCase .name}} = unpack_{{snakeCase .type}}($packet, {{.offset}});
    {{- end}}

    return $response;
}
{{end}}

function unpack_uint8($packet, $offset)
{
    return $packet[$offset] & 0x00ff;
}

function unpack_uint16($packet, $offset)
{
    $v = 0;
    $v |= $packet[$offset+1] & 0x00ff;
    $v <<= 8;
    $v |= $packet[$offset] & 0x00ff;

    return $v;
}

function unpack_uint32($packet, $offset)
{
    $v = 0;
    $v = $packet[$offset+3] & 0x00ff;
    $v <<= 8;
    $v |= $packet[$offset+2] & 0x00ff;
    $v <<= 8;
    $v |= $packet[$offset+1] & 0x00ff;
    $v <<= 8;
    $v |= $packet[$offset] & 0x00ff;

    return $v;
}

function unpack_bool($packet, $offset)
{
    if (($packet[$offset] & 0x00ff) == 1) {
        return true;
    } else {
        return false;
    }
}

function unpack_IPv4($packet, $offset)
{
    return sprintf(
        '%d.%d.%d.%d',
        $packet[$offset],
        $packet[$offset+1],
        $packet[$offset+2],
        $packet[$offset+3]
    );
}

function unpack_MAC($packet, $offset)
{
    return sprintf(
        '%02x:%02x:%02x:%02x:%02x:%02x',
        $packet[$offset],
        $packet[$offset+1],
        $packet[$offset+2],
        $packet[$offset+3],
        $packet[$offset+4],
        $packet[$offset+5]
    );
}

function unpack_version($packet, $offset)
{
    $major = $packet[$offset];
    $minor = $packet[$offset+1];

    return sprintf("v%x.%'.02x", $major, $minor);
}

function unpack_date($packet, $offset)
{
    $bcd = bcd2string(array_slice($packet, $offset, 4));
    $year = substr($bcd, 0, 4);
    $month = substr($bcd, 4, 2);
    $day = substr($bcd, 6, 2);

    return sprintf('%s-%s-%s', $year, $month, $day);
}

function unpack_shortdate($packet, $offset)
{
    $bcd = bcd2string(array_slice($packet, $offset, 3));
    $year = substr($bcd, 0, 2);
    $month = substr($bcd, 2, 2);
    $day = substr($bcd, 4, 2);

    return sprintf('20%s-%s-%s', $year, $month, $day);
}

function unpack_optional_date($packet, $offset)
{
    $bcd = bcd2string(array_slice($packet, $offset, 4));
    $year = substr($bcd, 0, 4);
    $month = substr($bcd, 4, 2);
    $day = substr($bcd, 6, 2);

    if ($bcd == '00000000000000') {
        return '';
    } else {
        return sprintf('%s-%s-%s', $year, $month, $day);
    }
}

function unpack_time($packet, $offset)
{
    $bcd = bcd2string(array_slice($packet, $offset, 3));
    $hour = substr($bcd, 0, 2);
    $minute = substr($bcd, 2, 2);
    $second = substr($bcd, 4, 2);

    return sprintf('%s:%s:%s', $hour, $minute, $second);
}

function unpack_hhmm($packet, $offset)
{
    $bcd = bcd2string(array_slice($packet, $offset, 2));
    $hour = substr($bcd, 0, 2);
    $minute = substr($bcd, 2, 2);

    return sprintf('%s:%s', $hour, $minute);
}

function unpack_datetime($packet, $offset)
{
    $bcd = bcd2string(array_slice($packet, $offset, 7));
    $year = substr($bcd, 0, 4);
    $month = substr($bcd, 4, 2);
    $day = substr($bcd, 6, 2);
    $hour = substr($bcd, 8, 2);
    $minute = substr($bcd, 10, 2);
    $second = substr($bcd, 12, 2);

    return sprintf('%s-%s-%s %s:%s:%s', $year, $month, $day, $hour, $minute, $second);
}

function unpack_optional_datetime($packet, $offset)
{
    $bcd = bcd2string(array_slice($packet, $offset, 7));
    $year = substr($bcd, 0, 4);
    $month = substr($bcd, 4, 2);
    $day = substr($bcd, 6, 2);
    $hour = substr($bcd, 8, 2);
    $minute = substr($bcd, 10, 2);
    $second = substr($bcd, 12, 2);

    if ($bcd == '00000000000000') {
        return '';
    } else {
        return sprintf('%s-%s-%s %s:%s:%s', $year, $month, $day, $hour, $minute, $second);        
    }
}

function unpack_pin($packet, $offset) 
{
    $v = 0;
    $v |= $packet[$offset+2] & 0x00ff;
    $v <<= 8;
    $v |= $packet[$offset+1] & 0x00ff;
    $v <<= 8;
    $v |= $packet[$offset] & 0x00ff;

    return $v;
}

function unpack_mode($packet, $offset)
{
    return $packet[$offset] & 0x00ff;
}

function unpack_event_type($packet, $offset)
{
    return $packet[$offset] & 0x00ff;
}

function unpack_direction($packet, $offset)
{
    return $packet[$offset] & 0x00ff;
}

function unpack_reason($packet, $offset)
{
    return $packet[$offset] & 0x00ff;
}

function bcd2string($bytes)
{
    return join('', array_map(fn ($v) => sprintf('%02x', $v), $bytes));
}
