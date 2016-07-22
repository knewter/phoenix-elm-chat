defmodule PresenceChat.PageController do
  use PresenceChat.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
