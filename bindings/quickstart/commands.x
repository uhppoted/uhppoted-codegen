commands = {
    'get_all-controllers':      get_all_controllers,
    'get_controller':           get_controller,
    'set_ip':                   set_ip,
    'get_time':                 get_time,
    'set_time':                 set_time,
    'get_listener':             get_listener,
    'set_listener':             set_listener,
    'get_door':                 get_door,
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
    'set-pc-control':           set_pc_control,
    'set-interlock':            set_interlock,
    'listen':                   listen,
}

function execute(cmd) {
    fn = commands[cmd]
    response = fn()

    if response {
        println(response)
    } else {
        println('no response')
    }
}

function get_all_controllers()  {
    return uhppote::get_all_controllers()
}

function get_controller()  {
    controller = arg('controller')

    return uhppote::get_controller(controller)
}

function set_ip() {
    controller = arg('controller')
    address    = arg('address')
    netmask    = arg('netmask')
    gateway    = arg('gateway')

    return uhppote::set_ip(controller, address, netmask, gateway)
}

function get_time() {
    controller = arg('controller')

    return uhppote::get_time(controller)
}

function set_time() {
    controller = arg('controller')
    datetime   = arg('time')

    return uhppote::set_time(controller, datetime)
}

function get_status() {
    controller = arg('controller')

    return uhppote::get_status(controller)
}

function get_listener() {
    controller = arg('controller')

    return uhppote::get_listener(controller)
}

function set_listener() {
    controller = arg('controller')
    address    = arg('address')
    port       = arg('port')

    return uhppote::set_listener(controller, address, port)
}

function get_door() {
    controller = arg('controller')
    door       = arg('door')

    return uhppote::get_door(controller, door)
}

function set_door_control() {
    controller = arg('controller')
    door := door
    mode := mode
    delay := delay

    return uhppote::set_door_control(controller, door, mode, delay)
}

function open_door() {
    controller = arg('controller')
    door := door

    return uhppote::open_door(controller, door)
}

function get_ards() {
    controller = arg('controller')

    return uhppote::get_cards(controller)
}

function get_card() {
    controller = arg('controller')
    card_number = arg('card')

    card = uhppote::get_card(controller, card_number)

    if card.number == 0 {
        return error('card not found')
    } else {
        return card
    }
}

function get_card_by_index() {
    controller = arg('controller')
    index      = arg('card-index')

    card = uhppote::get_card_by_index(controller, index)

    if card.number == 0 {
        return error('card not found')
    } else if card.number == 0xffffffff {
        return error('card deleted')
    } else {
        return card
    }
}

function put_card() {
    controller  = arg('controller')
    card_number = arg('card')
    start_date  = arg('start-date')
    end_date    = arg('end-date')
    doors       = arg('permissions')

    return uhppote::put_card(controller, card_number, start_date, end_date, doors.1, doors.2, doors.3, doors.4)
}

function delete_card() {
    controller = arg('controller')
    card_number = arg('card')

    return uhppote::delete_card(controller, card_number)
}

function delete_all_cards() {
    controller = arg('controller')

    return uhppote::delete_all_cards(controller)
}

function get_event() {
    controller = arg('controller')
    index      = arg('event-index')

    event = uhppote::get_event(controller, index)

    if event.type == 0xff {
        return error('event overwritten')
    } else if event.index == 0 {
        return error('event not found')
    } else {
        return event
    }
}

function get_event_index() {
    controller = arg('controller')

    return uhppote::get_event_index(controller)
}

function set_event_index() {
    controller = arg('controller')
    index      = arg('event_index')

    return uhppote::set_event_index(controller, index)
}

function record_special_events() {
    controller = arg('controller')
    enabled    = arg('record-special-events)

    return uhppote::record_special_events(controller, enabled)
}

function get_time_profile() {
    controller = arg('controller')
    id         = arg('time-profile-id')

    profile = uhppote::get_time_profile(controller, id)

    if profile.id == 0 {
        return error('time profile not defined')
    } else {
        return profile
    }
}

function set_time_profile() {
    controller = arg('controller')
    id         = arg('time-profile-id')
    start_date = arg('start-date')
    end_date   = arg('end-date')
    weekdays   = arg('weekdays')
    segment.1  = arg('segment-1')
    segment.2  = arg('segment-2')
    segment.3  = arg('segment-3')
    linked_profile = arg('linked-profile')

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

function delete_all_time_profiles() {
    controller = arg('controller')

    return uhppote::delete_all_time_profiles(controller)
}

function add_task() {
    controller = arg('controller')
    id         = arg('time-profile-id')
    start_date = arg('start-date')
    end_date   = arg('end-date')
    weekdays   = arg('weekdays')
    segment.1  = arg('segment-1')
    segment.2  = arg('segment-2')
    segment.3  = arg('segment-3')
    start_time = arg('start-time')
    door       = arg('door')
    task_type  = arg('task-type')
    more_cards = arg('more-cards')

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

function refresh_task_list() {
    controller = arg('controller')

    return uhppote::refresh_task_list(controller)
}

function clear_task_list() {
    controller = arg('controller')

    return uhppote::clear_task_list(controller)
}

function set_pc_control() {
    controller = arg('controller')
    enabled = arg('enabled')

    return uhppote::set_pc_control(controller, enabled)
}

function set_interlock() {
    controller = arg('controller')
    interlock = arg('interlock')

    return uhppote::set_interlock(controller, interlock)
}

function listen() {
    events = function(event) {
        println(event)
    }

    errors = function(error) {
        println(error)
    }

    interrupt = signal()

    uhppote::listen(events, errors, interrupt)
}
