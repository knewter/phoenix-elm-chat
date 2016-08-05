defmodule PresenceChat.ChatMessageTest do
  use PresenceChat.ModelCase

  alias PresenceChat.ChatMessage

  @valid_attrs %{body: "some content", channel: "some content", username: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = ChatMessage.changeset(%ChatMessage{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = ChatMessage.changeset(%ChatMessage{}, @invalid_attrs)
    refute changeset.valid?
  end
end
