package uhppote

import (
    "encoding/json"
    "net"
    "time"
)

type MAC net.HardwareAddr

func (m MAC) MarshalJSON() ([]byte, error) {
    return json.Marshal(net.HardwareAddr(m).String())
}

type Date time.Time

func (d Date) String() string {
    return time.Time(d).Format("2006-01-02")
}

func (d Date) MarshalJSON() ([]byte, error) {
    s := time.Time(d).Format("2006-01-02")
    
    return json.Marshal(s)
}
