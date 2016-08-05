defmodule PresenceChat.ChatMessage do
  use PresenceChat.Web, :model

  schema "chat_messages" do
    field :channel, :string
    field :username, :string
    field :body, :string

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:channel, :username, :body])
    |> validate_required([:channel, :username, :body])
  end
end
