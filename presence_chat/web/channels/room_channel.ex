defmodule PresenceChat.RoomChannel do
  use PresenceChat.Web, :channel
  alias PresenceChat.Presence
  require Logger

  def join("room:"<>_, msg, socket) do
    send self, {:after_join, msg}
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
end
