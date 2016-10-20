-module(idl_prv_clean).
-behaviour(provider).
-author('Sebastian Weddmark Olsson <sebastian.weddmark.olsson@ericsson.com>').

%%% == General ==
%%% This is a plugin for compiling Erlang IDL files using Rebar3.
%%%
%%% == How to use ==
%%% There are two options that can now be specified in your rebar.config:
%%%
%%% {idl_paths, [Paths]}.
%%%         Path = {file, FilePath, Options} | {file, FilePath} |
%%%                {dir, Directory, Options} | {dir, Directory}
%%% {idl_opts, [Options]}.
%%%         Options: Options specified by the ic module, check
%%%                  http://www.erlang.org/doc/man/ic.html for more info.
%%%
%%% If the options is not given with the path tuple, then the idl_opts option
%%% is used.
%%%
%%% Example:
%%%     {idl_opts, [{'Wall', true}]}.
%%%     {idl_files, [
%%%                  {file, "idl/system.idl", [{outdir, "generated"}]},
%%%                  {dir, "idlfiles"}
%%%                  ]}.
%%%
%%% In the given example, all *.idl-files found under the path
%%% "app_root/idlfiles" would be compiled with the 'Wall' option, while the
%%% file "app_root/idl/system.idl" would be compiled into the directory
%%% "generated".
%%%

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, clean).
-define(NAMESPACE, idl).
-define(DEPS, [{default, app_discovery}]).

%%% ===================================================================
%%% Public API
%%% ===================================================================
%%%
%%% These three functions is almost fully generated from the `rebar3' script.
%%%

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, ?NAMESPACE},      % To be able to run 'idl cmd'
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 idl clean"}, % How to use the plugin
            {opts, [                      % list of command-line options
                   ]},                    % understood by the plugin
            {short_desc, "Rebar3 IDL Compiler"},
            {desc, "This is a plugin for compiling Erlang IDL files using Rebar3."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Cwd = rebar_state:dir(State),
    Providers = rebar_state:providers(State),
    rebar_hooks:run_all_hooks(Cwd, pre, {?NAMESPACE, ?PROVIDER}, Providers, State),

    %% Do clean

    rebar_hooks:run_all_hooks(Cwd, post, {?NAMESPACE, ?PROVIDER}, Providers, State),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("Did not expect ~p~n", [Reason]).

%%% ===================================================================
%%% Private API
%%% ===================================================================
