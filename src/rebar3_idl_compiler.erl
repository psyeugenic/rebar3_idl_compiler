-module('rebar3_idl_compiler').

%%% ===================================================================
%%% General
%%% ===================================================================
%%% This is a plugin for compiling Erlang IDL files using Rebar3.
%%%
%%%
%%% ===================================================================
%%% How to use
%%% ===================================================================
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

-define(PROVIDER, 'rebar3_idl_compiler').
-define(DEPS, [app_discovery]).

%%% ===================================================================
%%% Public API
%%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 rebar3_idl_compiler"}, % How to use the plugin
            {opts, [                      % list of command-line options
                   ]},                    % understood by the plugin
            {short_desc, "Rebar3 IDL Compiler"},
            {desc, "This is a plugin for compiling Erlang IDL files using Rebar3."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    lists:foreach(fun compile_idl_file/1, get_idl_files(State)),
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
%%% get_idl_files/1
-spec get_idl_files(rebar_state:t()) -> [{file, string(), list()}].
%%% Documentation:
%%%       Returns a list of {file, Path, Opts} for each file specified.
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
%%% compile_idl_file/1
-spec compile_idl_file({file, Path::string(), Options::list()}) -> ok | warning | error.
%%% Documentation:
%%%       Compile a IDL file with the `ic' module.
%%%       Check documentation http://www.erlang.org/doc/man/ic.html
%%% ===================================================================
compile_idl_file({file, Path, Opts}) ->
    case ic:gen(Path, Opts) of
        ok ->
            ok;
        error ->
            format_error(error),
            error;
        {ok, [Warning]} ->
            format_error({warning, Warning}),
            warning;
        {error, [Warning], [Error]} ->
            format_error({error, Warning, Error}),
            error
    end.

%%% ===================================================================
%%% Help functions
%%% ===================================================================