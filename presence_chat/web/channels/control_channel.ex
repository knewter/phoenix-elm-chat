defmodule PresenceChat.ControlChannel do
  use PresenceChat.Web, :channel
  alias PresenceChat.Presence
  require Logger
  @two_way_regex ~r/<->/

  def join("control:" <> username, msg, socket) do
    {:ok, socket}
  end
end
