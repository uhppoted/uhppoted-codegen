package uhppote

import (
    "encoding/json"
    "time"
)

type Date time.Time

func (d Date) String() string {
    return time.Time(d).Format("2006-01-02")
}

func (d Date) Format(format string) string {
    return time.Time(d).Format(format)
}

func (d Date) MarshalJSON() ([]byte, error) {
    s := time.Time(d).Format("2006-01-02")
    
    return json.Marshal(s)
}


type Time time.Time

func (t Time) String() string {
    return time.Time(t).Format("15:04:05")
}

func (t Time) MarshalJSON() ([]byte, error) {
    s := time.Time(t).Format("15:04:05")
    
    return json.Marshal(s)
}


type DateTime time.Time

func (d DateTime) String() string {
    return time.Time(d).Format("2006-01-02 15:04:05")
}

func (d DateTime) Format(format string) string {
    return time.Time(d).Format(format)
}

func (d DateTime) MarshalJSON() ([]byte, error) {
    s := time.Time(d).Format("2006-01-02 15:04:05")
    
    return json.Marshal(s)
}


type HHmm time.Time

func (t HHmm) String() string {
    return time.Time(t).Format("15:04")
}

func (t HHmm) Format(format string) string {
    return time.Time(t).Format(format)
}

func (t HHmm) MarshalJSON() ([]byte, error) {
    s := time.Time(t).Format("15:04")
    
    return json.Marshal(s)
}

type PIN uint32

// func (p PIN) String() string {
//     return fmt.Sprintf("%v",p)
// }

// func (p PIN) Format(format string) string {
//     return fmt.Sprintf("%v",p)
// }

// func (p PIN) MarshalJSON() ([]byte, error) {
//     return json.Marshal(p)
// }
