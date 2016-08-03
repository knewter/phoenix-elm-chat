defmodule PresenceChat.ControlChannel do
  use PresenceChat.Web, :channel
  alias PresenceChat.Presence

  def join("control:" <> username, msg, socket) do
    {:ok, socket}
  end
end
