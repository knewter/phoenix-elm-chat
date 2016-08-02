defmodule PresenceChat.RoomChannel do
  use PresenceChat.Web, :channel
  alias PresenceChat.Presence
  require Logger
  @two_way_regex ~r/<->/

  def join("room:" <> channel, msg, socket) do
    send self, {:after_join, msg}
    handle_two_way_messaging(channel, socket)

    {:ok, socket}
  end

  def handle_in("new:msg", msg, socket) do
    broadcast socket, "new:msg", %{user: socket.assigns[:user], body: msg["body"]}
    {:reply, {:ok, %{msg: msg["body"]}}, socket}
  end

  def handle_info({:after_join, msg}, socket) do
    Presence.track(socket, socket.assigns[:user], %{
      device: "browser",
      online_at: inspect(:os.timestamp())
    })
    push socket, "presence_state", Presence.list(socket)

    broadcast! socket, "user:entered", %{user: socket.assigns[:user]}
    push socket, "join", %{status: "connected"}
    {:noreply, socket}
  end

  def terminate(reason, _socket) do
    Logger.debug "> leave #{inspect reason}"
    :ok
  end

  def handle_two_way_messaging(channel, socket) do
    if channel |> String.match?(@two_way_regex) do
      users = channel |> String.split(@two_way_regex)
      for user <- users do
        if(user != socket.assigns[:user]) do
          # FIXME: This should go over a user-specific control channel, not the lobby
          IO.puts "broadcasting to control channel: control:#{user}"
          PresenceChat.Endpoint.broadcast!("control:"<>user, "chat:join", %{channel: "room:"<>channel})
        end
      end
    end
  end
end
