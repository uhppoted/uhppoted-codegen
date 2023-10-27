'''
UHPPOTE request packet decoder unit tests.

Tests the packet decoding functions.
'''

import unittest
import datetime

from uhppote import decode

from tests.test_codec import to_uint8
from tests.test_codec import to_uint16
from tests.test_codec import to_uint32
from tests.test_codec import to_bool
from tests.test_codec import to_datetime
from tests.test_codec import to_date
from tests.test_codec import to_short_date
from tests.test_codec import to_optional_date
from tests.test_codec import to_optional_datetime
from tests.test_codec import to_time
from tests.test_codec import to_hhmm
from tests.test_codec import to_pin
from tests.test_codec import to_ipv4
from tests.test_codec import to_string

class TestDecode(unittest.TestCase):
{{range .testdata.tests}}
{{- if .response}}{{template "test" .}}{{end -}}
{{end}}

{{define "test"}}
    def test_{{snakeCase .name}}(self):
        # yapf: disable
        packet = bytearray([
{{dump .response.message "                  "}},
        ])
        # yapf: enable

        response = decode.{{snakeCase .response.name}}(packet)

        {{range .response.values -}}
        self.assertEqual(response.{{snakeCase .name}}, {{template "var" .}})
        {{end}}
{{end}}


if __name__ == '__main__':
    unittest.main()
