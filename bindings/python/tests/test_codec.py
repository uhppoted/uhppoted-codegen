import datetime

from ipaddress import IPv4Address


def to_uint8(v):
    return int(v)

def to_uint16(v):
    return int(v)

def to_uint32(v):
    return int(v)

def to_bool(v):
    return True if v == 'true' else False

def to_string(v):
    return f'{v}'

def to_ipv4(v):
    return IPv4Address(v)

def to_date(v):
    return datetime.datetime.strptime(v, '%Y-%m-%d').date()

def to_short_date(v):
    return datetime.datetime.strptime(v, '%Y-%m-%d').date()

def to_optional_date(v):
    try:
       return datetime.datetime.strptime(v, '%Y-%m-%d').date()
    except ValueError as x:
       return None

def to_datetime(v):
    return datetime.datetime.strptime(v, '%Y-%m-%d %H:%M:%S')

def to_optional_datetime(v):
    try:
       return datetime.datetime.strptime(v, '%Y-%m-%d %H:%M:%S')
    except ValueError as x:
       return None

def to_time(v):
    return datetime.datetime.strptime(v, '%H:%M:%S').time()

def to_hhmm(v):
    return datetime.datetime.strptime(v, '%H:%M').time()

def to_pin(v):
    return int(v)
