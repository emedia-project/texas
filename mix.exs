defmodule Texas.Mixfile do
  use Mix.Project

  def project do
    [
      app: :texas,
      version: "0.0.1",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps
    ]
  end

  def application do
    [
       applications: [:lager],
       env: []
    ]
  end

  defp deps do
    [
      {:lager, "~> 3.2"},
      {:pt_helpers, git: "https://github.com/emedia-project/pt_helpers.git", branch: "master"},
      {:doteki, "~> 0.1.0"},
      {:bucs, "~> 0.1.0"},
      {:texas_adapter, git: "https://github.com/emedia-project/texas_adapter.git", branch: "master"}    
    ]
  end
end