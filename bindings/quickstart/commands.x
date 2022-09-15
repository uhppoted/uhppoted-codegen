const commands = {
    'get_all-controllers':      get_all_controllers,
    'get_controller':           get_controller,
    'set_ip':                   set_ip,
    'get_time':                 get_time,
    'set_time':                 set_time,
    'get_listener':             get_listener,
    'set_listener':             set_listener,
    'get_door-control':         get_door_control,
    'set_door-control':         set_door_control,
    'get_status':               get_status,
    'open-door':                open_door,
    'get_cards':                get_cards,
    'get_card':                 get_card,
    'get_card-by-index':        get_card_by_index,
    'put-card':                 put_card,
    'delete-card':              delete_card,
    'delete-all-cards':         delete_all_cards,
    'get_event':                get_event,
    'get_event-index':          get_event_index,
    'set_event-index':          set_event_index,
    'record-special-events':    record_special_events,
    'get_time-profile':         get_time_profile,
    'set_time-profile':         set_time_profile,
    'delete-all-time-profiles': delete_all_time_profiles,
    'add-task':                 add_task,
    'refresh-tasklist':         refresh_tasklist,
    'clear-tasklist':           clear_tasklist,
    'listen':                   listen,
}

function execute(cmd) {
    fn = commands[cmd]

    try {
        response = fn()

        if response != null {
            println('no response')
        } else {
            println(response)
        }
    } catch(error) {
        println(error)
    }
}

func get_all_controllers()  {
    return uhppote::get_all_controllers()
}

func get_controller()  {
    controller = args('controller')

    return uhppote::get_controller(controller)
}

func set_ip() {
    controller = args('controller')
    address    = args('address')
    netmask    = args('netmask')
    gateway    = args('gateway')

    return uhppote::set_ip(controller, address, netmask, gateway)
}

func get_time() {
    controller = args('controller')

    return uhppote::get_time(controller)
}

func set_time() {
    controller = args('controller')
    datetime   = args('time')

    return uhppote::set_time(controller, datetime)
}

func get_status() {
    controller = args('controller')

    return uhppote::get_status(controller)
}

func get_listener() {
    controller = args('controller')

    return uhppote::get_listener(controller)
}

func set_listener() {
    controller = args('controller')
    address    = args('address')
    port       = args('port')

    return uhppote::set_listener(controller, address, port)
}

func get_door_control() {
    controller = args('controller')
    door       = args('door')

    return uhppote::get_door_control(controller, door)
}

func set_door_control() {
    controller = args('controller')
    door := door
    mode := mode
    delay := delay

    return uhppote::set_door_control(controller, door, mode, delay)
}

func open_door() {
    controller = args('controller')
    door := door

    return uhppote::open_door(controller, door)
}

func get_ards() {
    controller = args('controller')

    return uhppote::get_cards(controller)
}

func get_card() {
    controller = args('controller')
    card_number = args('card')

    card = uhppote::get_card(controller, card_number)

    if card.number == 0 {
        return error('card not found')
    } else {
        return card
    }
}

func get_card_by_index() {
    controller = args('controller')
    index      = args('card-index')

    card = uhppote::get_card_by_index(controller, index)

    if card.number == 0 {
        return error('card not found')
    } else if card.number == 0xffffffff {
        return error('card deleted')
    } else {
        return card
    }
}

func put_card() {
    controller  = args('controller')
    card_number = args('card')
    start_date  = args('start-date')
    end_date    = args('end-date')
    doors       = args('permissions')

    return uhppote::put_card(controller, card_number, start_date, end_date, doors.1, doors.2, doors.3, doors.4)
}

func delete_card() {
    controller = args('controller')
    card_number = args('card')

    return uhppote::delete_card(controller, card_number)
}

func delete_all_cards() {
    controller = args('controller')

    return uhppote::delete_all_cards(controller)
}

func get_event() {
    controller = args('controller')
    index      = args('event-index')

    event = uhppote::get_event(controller, index)

    if event.type == 0xff {
        return error('event overwritten')
    } else if event.index == 0 {
        return error('event not found')
    } else {
        return event
    }
}

func get_event_index() {
    controller = args('controller')

    return uhppote::get_event_index(controller)
}

func set_event_index() {
    controller = args('controller')
    index      = args('event_index')

    return uhppote::set_event_index(controller, index)
}

func record_special_events() {
    controller = args('controller')
    enabled    = args('record-special-events)

    return uhppote::record_special_events(controller, enabled)
}

func get_time_profile() {
    controller = args('controller')
    id         = args('time-profile-id')

    profile = uhppote::get_time_profile(controller, id)

    if profile.id == 0 {
        return error('time profile not defined')
    } else {
        return profile
    }
}

func set_time_profile() {
    controller = args('controller')
    id         = args('time-profile-id')
    start_date = args('start-date')
    end_date   = args('end-date')
    weekdays   = args('weekdays')
    segment.1  = args('segment-1')
    segment.2  = args('segment-2')
    segment.3  = args('segment-3')
    linked_profile = args('linked-profile')

    return uhppote::set_time_profile(
        controller,
        id,
        start_date, end_date,
        weekdays.monday, 
        weekdays.tuesday, 
        weekdays.wednesday, 
        weekdays.thursday, 
        weekdays.friday, 
        weekdays.saturday, 
        weekdays.sunday,
        segment.1.start, segment.1.end,
        segment.2.start, segment.2.end,
        segment.3.start, segment.3.end,
        linked_profile)
}

func delete_all_time_profiles() {
    controller = args('controller')

    return uhppote::delete_all_time_profiles(controller)
}

func add_task() {
    controller = args('controller')
    id         = args('time-profile-id')
    start_date = args('start-date')
    end_date   = args('end-date')
    weekdays   = args('weekdays')
    segment.1  = args('segment-1')
    segment.2  = args('segment-2')
    segment.3  = args('segment-3')
    start_time = args('start-time')
    door       = args('door')
    task_type  = args('task-type')
    more_cards = args('more-cards')

    return uhppote::add_task(
        controller,
        start_date, end_date,
        weekdays.monday, 
        weekdays.tuesday, 
        weekdays.wednesday, 
        weekdays.thursday, 
        weekdays.friday, 
        weekdays.saturday, 
        weekdays.sunday,
        start_time,
        door,
        task_type,
        more_cards)
}

func refresh_task_list() {
    controller = args('controller')

    return uhppote::refresh_task_list(controller)
}

func clear_task_list() {
    controller = args('controller')

    return uhppote::clear_task_list(controller)
}

func listen() {
    events = function(event) {
        println(event)
    }

    errors = function(error) {
        println(error)
    }

    interrupt = signal()

    uhppote::listen(events, errors, interrupt)
}
