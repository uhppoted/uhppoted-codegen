import pprint
import ipaddress
import datetime

import uhppote

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
        'get-door-control': get_door_control,
        'set-door-control': set_door_control,
        'get-status': get_status,
        'open-door': open_door,
        'get-cards': get_cards,
        'get-card': get_card,
        'get-card-by-index': get_card_by_index,
        'put-card': put_card,
        'delete-card': delete_card,
        'delete-all-cards': delete_all_cards,
        'get-event': get_event,
        'get-event-index': get_event_index,
        'set-event-index': set_event_index,
        'record-special-events': record_special_events,
        'get-time-profile': get_time_profile,
        'set-time-profile': set_time_profile,
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
    (address, port) = LISTENER

    return u.set_listener(CONTROLLER, address, port)


def get_door_control(u):
    return u.get_door_control(CONTROLLER, DOOR)


def set_door_control(u):
    return u.set_door_control(CONTROLLER, DOOR, MODE, DELAY)


def get_status(u):
    return u.get_status(CONTROLLER)


def open_door(u):
    return u.open_door(CONTROLLER, DOOR)


def get_cards(u):
    return u.get_cards(CONTROLLER)


def get_card(u):
    return u.get_card(CONTROLLER, CARD)


def get_card_by_index(u):
    return u.get_card_by_index(CONTROLLER, CARD_INDEX)


def put_card(u):
    start = datetime.datetime.strptime("2022-01-01", '%Y-%m-%d').date()
    end = datetime.datetime.strptime("2022-12-31", '%Y-%m-%d').date()

    return u.put_card(CONTROLLER, CARD, start, end, 0, 1, 29, 0)


def delete_card(u):
    return u.delete_card(CONTROLLER, CARD)


def delete_all_cards(u):
    return u.delete_all_cards(CONTROLLER)


def get_event(u):
    return u.get_event(CONTROLLER, EVENT_INDEX)


def get_event_index(u):
    return u.get_event_index(CONTROLLER)


def set_event_index(u):
    return u.set_event_index(CONTROLLER, EVENT_INDEX)


def record_special_events(u):
    return u.record_special_events(CONTROLLER, True)


def get_time_profile(u):
    return u.get_time_profile(CONTROLLER, TIME_PROFILE_ID)


def set_time_profile(u):
    start = datetime.datetime.strptime("2022-01-01", '%Y-%m-%d').date()
    end = datetime.datetime.strptime("2022-12-31", '%Y-%m-%d').date()
    monday = True
    tuesday = False
    wednesday = True
    thursday = True
    friday = False
    saturday = False
    sunday = True
    segment1start = datetime.datetime.strptime("08:15", '%H:%M').time()
    segment1end = datetime.datetime.strptime("11:45", '%H:%M').time()
    segment2start = datetime.datetime.strptime("12:45", '%H:%M').time()
    segment2end = datetime.datetime.strptime("17:15", '%H:%M').time()
    segment3start = datetime.datetime.strptime("19:30", '%H:%M').time()
    segment3end = datetime.datetime.strptime("22:00", '%H:%M').time()
    linked_profile_ID = 23

    return u.set_time_profile(CONTROLLER, TIME_PROFILE_ID,
        start, end,
        monday, tuesday, wednesday, thursday, friday, saturday, sunday,
        segment1start, segment1end,
        segment2start, segment2end,
        segment3start, segment3end,
        linked_profile_ID)
