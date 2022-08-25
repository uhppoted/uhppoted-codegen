import * as encode from './encode.js'
import * as decode from './decode.js'

let REQUESTID = 0

export function get_all_controllers () {
  const request = encode.GetController(0)
  const replies= post(request, 5000)

  const list = []
  for (const reply of replies) {
    list.push(decode.GetControllerResponse(reply))
  }

  return list
}

function post (bytes, timeout) {
  const hex = bin2hex(bytes)
  const debug = document.querySelector('#request textarea')

  debug.value = hex

  const rq = {
    ID: nextID(),
    wait: timeout,
    request: [...bytes]
  }

  const request = {
    method: 'POST',
    mode: 'cors',
    cache: 'no-cache',
    credentials: 'same-origin',
    headers: { 'Content-Type': 'application/json' },
    redirect: 'follow',
    referrerPolicy: 'no-referrer',
    body: JSON.stringify(rq)
  }

  fetch('/udp', request)
    .then(response => {
      switch (response.status) {
        case 200:
          return response.json()

        default:
          response.text().then(w => {
            warn(new Error(w))
          })
      }
    })
    .then(reply => {
      return reply.replies
    })
    .catch(function (err) {
      warn(`${err.message.toLowerCase()}`)
    })
    .finally(() => {
    })
}

function nextID () {
  REQUESTID++

  return REQUESTID
}

function warn (err) {
  console.error(err)
}

function bin2hex (bytes) {
  const chunks = [...bytes]
    .map(x => x.toString(16).padStart(2, '0'))
    .join('')
    .match(/.{1,16}/g)
    .map(l => l.match(/.{1,2}/g).join(' '))

  const lines = []
  while (chunks.length > 0) {
    lines.push(chunks.splice(0, 2).join('  '))
  }

  return lines.join('\n')

  // const f = function* chunks(array,N) {
  //    for (let i=0; i < array.length; i += N) {
  //        yield array.slice(i, i + N);
  //    }
  // }
}
