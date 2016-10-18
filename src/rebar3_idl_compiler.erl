-module(rebar3_idl_compiler).
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

%% Internal
-export([compile_idl_file/2]).

-define(PROVIDER, compile).
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
            {example, "rebar3 idl compile"}, % How to use the plugin
            {opts, [                      % list of command-line options
                   ]},                    % understood by the plugin
            {short_desc, "Rebar3 IDL Compiler"},
            {desc, "This is a plugin for compiling Erlang IDL files using Rebar3."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    IdlFiles = get_idl_files(State),

    Cwd = rebar_state:dir(State),
    Providers = rebar_state:providers(State),
    rebar_hooks:run_all_hooks(Cwd, pre, {?NAMESPACE, ?PROVIDER}, Providers, State),

    lists:foreach(fun(Idl) -> spawn_link(?MODULE, compile_idl_file, [self(), Idl]) end, IdlFiles),
    wait_until_finished(length(IdlFiles)),

    rebar_hooks:run_all_hooks(Cwd, post, {?NAMESPACE, ?PROVIDER}, Providers, State),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(error) ->
    io_lib:format("Error: undefined error.");
format_error({warning, Warning}) ->
    io_lib:format("Warning: ~p", [Warning]);
format_error({error, Warning, Error}) ->
    io_lib:format("Error: ~p,~nWarning: ~p", [Error, Warning]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%% ===================================================================
%%% Private API
%%% ===================================================================

%%% ===================================================================
-spec get_idl_files(rebar_state:t()) -> [{file, string(), list()}].
%%% @doc
%%%   Returns a list of {file, Path, Opts} for each file specified.
%%% @end
%%% ===================================================================
get_idl_files(State) ->
    GeneralOpts = rebar_state:get(State, idl_opts, []),
    PathOptions = rebar_state:get(State, idl_paths, []),
    io:format("Got options: Path: ~p,~n          General: ~p~n", [PathOptions, GeneralOpts]),
    ListOfFiles = [normalize_file(PathOption, GeneralOpts) || PathOption <- PathOptions],
    lists:flatten(ListOfFiles).

normalize_file({file, FilePath, Opts}, _GeneralOpts) ->
    [{file, FilePath, Opts}];
normalize_file({file, FilePath}, GeneralOpts) ->
    [{file, FilePath, GeneralOpts}];
normalize_file({dir, DirPath, Opts}, _GeneralOpts) ->
    [{file, FilePath, Opts} || FilePath <- filelib:wildcard(DirPath ++ "/*.idl")];
normalize_file({dir, DirPath}, GeneralOpts) ->
    [{file, FilePath, GeneralOpts} || FilePath <- filelib:wildcard(DirPath ++ "/*.idl")].


%%% ===================================================================
-spec compile_idl_file(pid(), {file, Path::string(), Options::list()}) -> ok | error | {warning, string()} | {error, list(string()), list(string())}.
%%% @doc
%%%   Compile a IDL file with the `ic' module.
%%%   Check documentation http://www.erlang.org/doc/man/ic.html
%%% @end
%%% ===================================================================
compile_idl_file(Pid, {file, Path, Opts}) ->
    case ic:gen(Path, Opts) of
        ok ->
            Pid ! ok;
        error ->
            Pid ! error;
        {ok, [Warning]} ->
            Pid ! {warning, Warning};
        {error, [Warning], [Error]} ->
            Pid ! {error, Warning, Error}
    end.

%%% ===================================================================
-spec wait_until_finished(non_neg_integer()) -> ok.
%%% @doc
%%%  It's the finalcount down! Dududu, DUDUDUDU-DU.
%%% @end
%%% ===================================================================
wait_until_finished(0) ->
    ok;
wait_until_finished(Left) ->
    receive
        ok ->
            ok;
        Error ->
            format_error(Error)
    end,
    wait_until_finished(Left-1).
