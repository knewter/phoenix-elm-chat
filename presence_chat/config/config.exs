# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :presence_chat,
  ecto_repos: [PresenceChat.Repo]

# Configures the endpoint
config :presence_chat, PresenceChat.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "PL+tifD7TGnSmrKNoQMoG4j9BTO/aiUGNwl2WpnzFnyX3K+pKMWLIgOWOS805xD+",
  render_errors: [view: PresenceChat.ErrorView, accepts: ~w(html json)],
  pubsub: [name: PresenceChat.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
