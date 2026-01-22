const std = @import("std");
const datelib = @import("datetime.zig");
const decode = @import("decode.zig");
const errors = @import("errors.zig");
const network = @import("network.zig");

const ZERO_DATETIME = [_]u8{ '0', '0', '0', '0', '0','0', '0', '0', '0', '0', '0', '0', '0', '0' };

{{range .testdata.tests}}
{{- if .response}}{{template "test" .}}{{end -}}
{{end}}

{{define "test"}}
test "decode {{ .name }} response" {
     const reply = [_]u8{
        {{dump .response.message "        "}},
     };

    const response = try decode.{{ snakeCase .response.name }}(reply);

    {{range .response.values -}}
    {{- if eq .type "string"}}
    try std.testing.expectFmt("{{ .value }}", "{s}", .{ response.{{ snakeCase .name }} });
    {{- else if eq .type "IPv4"}}
    try std.testing.expectFmt("{{ .value }}", "{f}", .{ response.{{ snakeCase .name }} });
    {{- else if eq .type "date" "short date" }}
    try std.testing.expectFmt("{{ .value }}", "{d:0>4}-{d:0>2}-{d:0>2}", 
                                              .{ 
                                                 response.{{ snakeCase .name }}.year,
                                                 response.{{ snakeCase .name }}.month,
                                                 response.{{ snakeCase .name }}.day
                                               });
    {{- else if eq .type "date" "optional date" }}
    try std.testing.expectFmt("{{ .value }}", "{d:0>4}-{d:0>2}-{d:0>2}", 
                                              .{ 
                                                 response.{{ snakeCase .name }}.?.year,
                                                 response.{{ snakeCase .name }}.?.month,
                                                 response.{{ snakeCase .name }}.?.day
                                               });
    {{- else if eq .type "time" }}
    try std.testing.expectFmt("{{ .value }}", "{d:0>2}:{d:0>2}:{d:0>2}", 
                                              .{ 
                                                 response.{{ snakeCase .name }}.hour,
                                                 response.{{ snakeCase .name }}.minute,
                                                 response.{{ snakeCase .name }}.second
                                               });
    {{- else if eq .type "datetime" }}
    try std.testing.expectFmt("{{ .value }}", "{d:0>4}-{d:0>2}-{d:0>2} {d:0>2}:{d:0>2}:{d:0>2}", 
                                              .{ 
                                                 response.{{ snakeCase .name }}.year,
                                                 response.{{ snakeCase .name }}.month,
                                                 response.{{ snakeCase .name }}.day,
                                                 response.{{ snakeCase .name }}.hour,
                                                 response.{{ snakeCase .name }}.minute,
                                                 response.{{ snakeCase .name }}.second
                                               });
    {{- else if eq .type "optional datetime"}}
    if (std.mem.eql(u8, "{{ .value }}","")) {
       try std.testing.expectEqual(response.{{ snakeCase .name }}, null);
    } else {
       try std.testing.expectFmt("{{ .value }}", "{d:0>4}-{d:0>2}-{d:0>2} {d:0>2}:{d:0>2}:{d:0>2}", 
                                                 .{ 
                                                    response.{{ snakeCase .name }}.?.year,
                                                    response.{{ snakeCase .name }}.?.month,
                                                    response.{{ snakeCase .name }}.?.day,
                                                    response.{{ snakeCase .name }}.?.hour,
                                                    response.{{ snakeCase .name }}.?.minute,
                                                    response.{{ snakeCase .name }}.?.second
                                                  });
    }
    {{- else if eq .type "HHmm" }}
    try std.testing.expectFmt("{{ .value }}", "{d:0>2}:{d:0>2}", 
                                              .{ 
                                                 response.{{ snakeCase .name }}.hour,
                                                 response.{{ snakeCase .name }}.minute
                                               });
    {{- else -}}
    try std.testing.expectEqual(response.{{ snakeCase .name }},{{template "var" .}});
    {{- end}}
    {{- end}}
}
{{end}}
