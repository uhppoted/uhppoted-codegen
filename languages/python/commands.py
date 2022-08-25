import uhppote

def commands():
    return {
        'get-all-controllers': get_all_controllers,
        # 'get-controller': get_device,
        # 'set-address': set_address,
        # 'get-status': get_status,
        # 'get-time': {get_time,
        # 'set-time': set_time,
        # 'get-listener': get_listener,
        # 'set-listener': set_listener,
        # 'get-door-control': get_door_control,
        # 'set-door-control': set_door_control,
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
        # 'record-special-events': record_special_events,
        # 'get-time-profile': get_time_profile,
        # 'set-time-profile': set_time_profile,
        # 'clear-time-profiles': clear_time_profiles,
        # 'add-task': add_task,
        # 'refresh-tasklist': refresh_tasklist,
        # 'clear-tasklist': clear_tasklist,
    }

def exec(f):
    response = f()
    print(response)

def get_all_controllers():
    print(uhppote.get_all_controllers())

