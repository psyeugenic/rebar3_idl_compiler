-module(idl_prv_clean).
-behaviour(provider).
-author('Sebastian Weddmark Olsson <sebastian.weddmark.olsson@ericsson.com>').

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
            {bare, true},                 % The task can be run by the user
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 idl clean"}, % How to use the plugin
            {opts, [                      % list of command-line options
                   ]},                    % understood by the plugin
            {short_desc, "IDL Compiler"},
            {desc, "This is a plugin for cleaning Erlang IDL files."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Cwd = rebar_state:dir(State),
    Providers = rebar_state:providers(State),
    rebar_hooks:run_all_hooks(Cwd, pre, {?NAMESPACE, ?PROVIDER}, Providers, State),

    %% Do clean
    remove_idl_dir(State),

    rebar_hooks:run_all_hooks(Cwd, post, {?NAMESPACE, ?PROVIDER}, Providers, State),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("Did not expect ~p~n", [Reason]).

%%% ===================================================================
%%% Private API
%%% ===================================================================

%%% ===================================================================
-spec remove_idl_dir(rebar_state:t()) -> string().
%%% @doc
%%%  Removes the cache directory "idl".
%%% @end
%%% ===================================================================
remove_idl_dir(State) ->
    CurrentApp = rebar_state:current_app(State),
    OutDir = case CurrentApp /= undefined of
                 true ->
                     rebar_app_info:out_dir(CurrentApp);
                 false ->
                     [ProjectApp1|_] = rebar_state:project_apps(State),
                     rebar_app_info:out_dir(ProjectApp1)
             end,
    RebarCache = rebar_dir:local_cache_dir(OutDir),
    CacheDir = filename:join([RebarCache, "idl"]),
    case file:list_dir_all(CacheDir) of
        {ok, Files} ->
            LeftFiles = lists:foldl(
                          fun (File, Acc) ->
                                  CacheFile = filename:join([CacheDir, File]),
                                  case file:delete(CacheFile) of
                                      ok ->
                                          Acc;
                                      {error, Reason} ->
                                          [{error, File, Reason}|Acc]
                                  end
                          end, [], Files),
            case LeftFiles of
                [] ->
                    ok = file:del_dir(CacheDir);
                _ ->
                    rebar_log:log(error,
                                  "IDL cleaner could not remove "
                                  "some files:~n~p~n",
                                  [LeftFiles])
            end;
        {error, enoent} ->
            %% Directory does not exist
            ok
    end.
