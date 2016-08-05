defmodule PresenceChat.Repo.Migrations.CreateChatMessage do
  use Ecto.Migration

  def change do
    create table(:chat_messages) do
      add :channel, :string
      add :username, :string
      add :body, :text

      timestamps()
    end

  end
end
