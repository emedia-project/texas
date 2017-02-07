defmodule Texas.Mixfile do
  use Mix.Project

  def project do
    [
      app: :texas,
      version: "0.0.1",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      aliases: aliases()
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
      {:lager, "~> 3.2.0"},
      {:pt_helpers, git: "https://github.com/emedia-project/pt_helpers.git", branch: "master"},
      {:doteki, "~> 1.0.5"},
      {:bucs, "~> 1.0.6"},
      {:texas_adapter, git: "https://github.com/emedia-project/texas_adapter.git", branch: "master"}    
    ]
  end

  defp aliases do
    [compile: &compile_with_hooks/1]
  end

  defp compile_with_hooks(args) do
    pre_compile_hooks()
    result = Mix.Task.run("compile", args)
    post_compile_hooks()
    result
  end

  defp pre_compile_hooks() do
    run_hook_cmd [
    ]
  end

  defp post_compile_hooks() do
    run_hook_cmd [
    ]
  end

  defp run_hook_cmd(commands) do
    {_, os} = :os.type
    for command <- commands, do: (fn
      ({regex, cmd}) ->
         if Regex.match?(Regex.compile!(regex), Atom.to_string(os)) do
           Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(String.strip(x)) end
         end
      (cmd) ->
        Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(String.strip(x)) end
      end).(command)
  end    
end