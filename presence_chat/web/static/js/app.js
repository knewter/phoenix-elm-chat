// Brunch automatically concatenates all files in your
// watched paths. Those paths can be configured at
// config.paths.watched in "brunch-config.js".
//
// However, those files will only be executed if
// explicitly imported. The only exception are files
// in vendor, which are never wrapped in imports and
// therefore are always executed.

// Import dependencies
//
// If you no longer want to use a dependency, remember
// to also remove its path from "config.paths.watched".
import "phoenix_html"

// Import local files
//
// Local files can be imported directly using relative
// paths "./socket" or full ones "web/static/js/socket".

// import socket from "./socket"

import {Socket, Presence} from "phoenix"

let $status = $("#status")
let $messages = $("#messages")
let $input = $("#message-input")
let $username = $("#username")
let $usernameContainer = $("#username-container")
let $userList = $("#user-list")

$status.hide()
$messages.hide()
$input.hide()

$username.off("keypress").on("keypress", e => {
  if (e.keyCode == 13) {
    initiateChat($username.val())
  }
})

let initiateChat = (username) => {
  let socket = new Socket("/socket", { params: {username: username} })

  socket.connect()

  $usernameContainer.hide()
  $status.show()
  $messages.show()
  $input.show()

  let sanitize = (html) => { return $("<div/>").text(html).html() }

  let messageTemplate = (msg) => {
    let body = sanitize(msg.body)
    let user = sanitize(msg.user || "anonymous")

    return (`<p><a href='#'>[${user}]</a> ${body}</p>`)
  }

  let chan = socket.channel("room:lobby", {})

  let presences = {}

  let listBy = (id, {metas: [first, ...rest]}) => {
    first.name = id
    first.count = rest.length + 1
    return first
  }

  let render = (presences) => {
    $userList.html(
      Presence.list(presences, listBy)
        .map(user => `<li>${user.name} (${user.count}) [${user.device}]</li>`)
        .join("")
    )
  }

  chan.on("presence_state", state => {
    Presence.syncState(presences, state)
    render(presences)
  })

  chan.on("presence_diff", diff => {
    Presence.syncDiff(presences, diff)
    render(presences)
  })

  chan.join().receive("ok", () => console.log("join ok"))
             .receive("timeout", () => console.log("Connection interruption"))
  chan.onError(e => console.log("something went wrong", e))
  chan.onClose(e => console.log("channel closed", e))

  $input.off("keypress").on("keypress", e => {
    if (e.keyCode == 13) {
      chan.push("new:msg", {body: $input.val()})
      $input.val("")
    }
  })

  chan.on("new:msg", msg => {
    $messages.append(messageTemplate(msg))
    scrollTo(0, document.body.scrollHeight)
  })

  chan.on("user:entered", msg => {
    let user = sanitize(msg.user || "anonymous")
    $messages.append(`<br/><i>[${user} entered]</i>`)
  })
}
