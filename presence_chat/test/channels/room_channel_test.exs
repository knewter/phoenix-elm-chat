defmodule PresenceChat.RoomChannelTest do
  use PresenceChat.ChannelCase

  alias PresenceChat.RoomChannel

  def new_socket_on_lobby(username) do
    {:ok, _, socket} =
      socket("user_id", %{user: username})
      |> subscribe_and_join(RoomChannel, "room:lobby")
    {:ok, socket}
  end

  setup do
    {:ok, alice} = new_socket_on_lobby("alice")
    {:ok, socket: alice}
  end

  test "new:msg broadcasts to room:lobby", %{socket: socket} do
    push socket, "new:msg", %{body: "foo"}
    assert_broadcast "new:msg", %{body: "foo", user: "alice"}
  end

  test "new users receive chat history", %{socket: socket} do
    ref = push socket, "new:msg", %{body: "foo"}
    assert_reply ref, :ok # Wait til we get the reply before running the next bit of the test
    {:ok, bob} = new_socket_on_lobby("bob")
    flush
    push bob, "history:fetch", %{}
    assert_push "history:list", %{history: [%{body: "foo", user: "alice"}] }
  end

  test "broadcasts are pushed to the client", %{socket: socket} do
    broadcast_from! socket, "broadcast", %{"some" => "data"}
    assert_push "broadcast", %{"some" => "data"}
  end

  def flush do
    receive do
      _ -> flush
    after 0 -> :ok
    end
  end
end
