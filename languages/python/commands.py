import pprint
import ipaddress
import datetime

import uhppote

# const ANY = "0.0.0.0:0"
# const BROADCAST = "255.255.255.255:60000"
CONTROLLER = 405419896
DOOR = 3
MODE = 2
DELAY = 10
CARD = 8165538
CARD_INDEX = 3
EVENT_INDEX = 37
TIME_PROFILE_ID = 29

ADDRESS = ipaddress.IPv4Address('192.168.1.100')
NETMASK = ipaddress.IPv4Address('255.255.255.0')
GATEWAY = ipaddress.IPv4Address('192.168.1.1')
LISTENER = (ipaddress.IPv4Address('192.168.1.100'), 60001)

def commands():
    return {
        'get-all-controllers': get_all_controllers,
        'get-controller': get_controller,
        'set-ip': set_ip,
        'get-time': get_time,
        'set-time': set_time,
        'get-listener': get_listener,
        'set-listener': set_listener,
        # 'get-door-control': get_door_control,
        # 'set-door-control': set_door_control,
        'get-status': get_status,
        # 'open-door': open_door,
        # 'get-cards': get_cards,
        # 'get-card': get_card,
        # 'get-card-by-index': get_card_by_index,
        # 'put-card': put_card,
        # 'delete-card': delete_card,
        # 'delete-cards': delete_cards,
        # 'get-event-index': get_event_index,
        # 'set-event-index': set_event_index,
        # 'get-event': get_event,
        'record-special-events': record_special_events,
        'get-time-profile': get_time_profile,
        # 'set-time-profile': set_time_profile,
        # 'clear-time-profiles': clear_time_profiles,
        # 'add-task': add_task,
        # 'refresh-tasklist': refresh_tasklist,
        # 'clear-tasklist': clear_tasklist,
    }


def exec(f, bind, broadcast, debug):
    u = uhppote.Uhppote(bind, broadcast, debug)
    response = f(u)

    pprint.pprint(response, indent=2, width=-1)

def get_all_controllers(u):
    return u.get_all_controllers()

def get_controller(u):
    return u.get_controller(CONTROLLER)

def set_ip(u):
    return u.set_ip(CONTROLLER, ADDRESS, NETMASK, GATEWAY)

def get_time(u):
    return u.get_time(CONTROLLER)

def set_time(u):
    return u.set_time(CONTROLLER, datetime.datetime.now())

def get_listener(u):
    return u.get_listener(CONTROLLER)

def set_listener(u):
    (address,port) = LISTENER
    return u.set_listener(CONTROLLER, address, port)

def get_status(u):
    return u.get_status(CONTROLLER)

def record_special_events(u):
    return u.record_special_events(CONTROLLER, True)

def get_time_profile(u):
    return u.get_time_profile(CONTROLLER, TIME_PROFILE_ID)
