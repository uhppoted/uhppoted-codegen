package uhppote

import(
    "fmt"
    "net/netip"
    "strconv"
    "strings"
    "time"
)

func toUint8(s string) uint8 {
  v, _ := strconv.ParseUint(s, 10, 8)

  return uint8(v)
}

func toUint16(s string) uint16 {
  v, _ := strconv.ParseUint(s, 10, 16)

  return uint16(v)
}

func toUint32(s string) uint32 {
  v, _ := strconv.ParseUint(s, 10, 32)

  return uint32(v)
}

func toBool(s string) bool {
  v, _ := strconv.ParseBool(s)

  return v
}

func toString(s string) string {
  return s
}

func toIPv4(s string) netip.Addr {
  return netip.MustParseAddr(s)
}

func toDate(s string) Date {
  v,_ := time.ParseInLocation("2006-01-02", s, time.Local)

  return Date(v)
}

func toShortDate(s string) Date {
  v,_ := time.ParseInLocation("2006-01-02", s, time.Local)

  return Date(v)
}

func toOptionalDate(s string) *Date {
  v, _ := time.ParseInLocation("2006-01-02", s, time.Local)
  date := Date(v)

  return &date
}

func toDatetime(s string) DateTime {
  v,_ := time.ParseInLocation("2006-01-02 15:04:05", s, time.Local)

  return DateTime(v)
}

func toOptionalDatetime(s string) *DateTime {
  v,_ := time.ParseInLocation("2006-01-02 15:04:05", s, time.Local)
  datetime :=  DateTime(v)

  return &datetime
}

func toTime(s string) Time {
  v,_ := time.ParseInLocation("15:04:05", s, time.Local)

  return Time(v)
}

func toHHmm(s string) HHmm {
  v,_ := time.ParseInLocation("15:04", s, time.Local)

  return HHmm(v)
}

func toPin(s string) PIN {
  v, _ := strconv.ParseUint(s, 10, 32)

  return PIN(v)
}

func format(packet []byte) string {
    var s strings.Builder
    
    hex := "%02x %02x %02x %02x %02x %02x %02x %02x"
    prefix := ""

    for i := 0; i < 4; i++ {
        offset := i * 16
        u := packet[offset : offset+8]
        v := packet[offset+8 : offset+16]

        p := fmt.Sprintf(hex, u[0], u[1], u[2], u[3], u[4], u[5], u[6], u[7])
        q := fmt.Sprintf(hex, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7])

        fmt.Fprintf(&s,"%v   %08x  %v  %v\n", prefix, offset, p, q)
        prefix = "            "
    }

    return s.String()
}
