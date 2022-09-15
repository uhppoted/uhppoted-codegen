package main

function main() {
    println("uhppoted-codegen: quickstart sample application")

    bind      = option('bind-address', '0.0.0.0')
    broadcast = option('broadcast-address', '255.255.255.255:60000')
    listen    = option('listen-address', '0.0.0.0:60001')
    debug     = option('debug', false)
    cmd       = option('command')

    udp::set_bind_addres(bind)
    udp::set_broadcast_address(broadcast)
    udp::set_listen_address(listen)
    udp::set_debug(debug)

    try {
        if commands.has(cmd) {
            commands::execute(cmd)
        } else {
            println('invalid command')
        }
    } catch(error) {
        println(error)
    }
}
