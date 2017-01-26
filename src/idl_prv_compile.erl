-module(idl_prv_compile).
-behaviour(provider).
-author('Sebastian Weddmark Olsson <sebastian.weddmark.olsson@ericsson.com>').

-export([init/1, do/1, format_error/1]).

%% Internal
-export([compile_idl_file/3]).

-define(PROVIDER, compile).
-define(NAMESPACE, idl).
-define(DEPS, [{default, app_discovery}]).

-define(TIMEOUT, 30000).

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
            {example, "rebar3 idl compile"}, % How to use the plugin
            {opts, [                      % list of command-line options
                   ]},                    % understood by the plugin
            {short_desc, "IDL Compiler"},
            {desc, "This is a plugin for compiling Erlang IDL files."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    IdlFiles = get_idl_files(State),
    CacheDir = create_cache_dir(State),

    Cwd = rebar_state:dir(State),
    Providers = rebar_state:providers(State),
    rebar_hooks:run_all_hooks(Cwd, pre,
                              {?NAMESPACE, ?PROVIDER},
                              Providers, State),

%    NewIdlFiles = lists:filter(fun ({file, Idl, _}) ->
%                                       is_changed(CacheDir, Idl)
%                               end, IdlFiles),
    lists:foreach(fun(Idl) ->
                          compile_idl_file(CacheDir, self(), Idl)
                  end, IdlFiles),

%    lists:foreach(fun(Idl) ->
%                          spawn_link(?MODULE, compile_idl_file,
%                                     [CacheDir, self(), Idl])
%                  end, NewIdlFiles),
    wait_until_finished(length(IdlFiles)),

    rebar_hooks:run_all_hooks(Cwd, post,
                              {?NAMESPACE, ?PROVIDER},
                              Providers, State),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%% ===================================================================
%%% Private API
%%% ===================================================================

%%% ===================================================================
-spec get_idl_files(rebar_state:t()) -> [{file, string(), list()}].
%%% @doc
%%%  Returns a list of {file, Path, Opts} for each file specified.
%%% @end
%%% ===================================================================
get_idl_files(State) ->
    GeneralOpts = rebar_state:get(State, idl_opts, []),
    PathOptions = rebar_state:get(State, idl_paths, []),
    rebar_log:log(debug,
                  "IDL got options:~n  Path: ~p,~n  General: ~p~n",
                  [PathOptions, GeneralOpts]),
    ListOfFiles = [normalize_file(PathOption, GeneralOpts)
                   || PathOption <- PathOptions],
    lists:flatten(ListOfFiles).

normalize_file({file, FilePath, Opts}, _GeneralOpts) ->
    [{file, FilePath, Opts}];
normalize_file({file, FilePath}, GeneralOpts) ->
    [{file, FilePath, GeneralOpts}];
normalize_file({dir, DirPath, Opts}, _GeneralOpts) ->
    [{file, FilePath, Opts}
     || FilePath <- filelib:wildcard(DirPath ++ "/*.idl")];
normalize_file({dir, DirPath}, GeneralOpts) ->
    [{file, FilePath, GeneralOpts}
     || FilePath <- filelib:wildcard(DirPath ++ "/*.idl")].

%%% ===================================================================
-spec create_cache_dir(rebar_state:t()) -> string().
%%% @doc
%%%  Checks if the cache directory "idl" exists. If it does not exist
%%%  it creates it. Returns the path.
%%% @end
%%% ===================================================================
create_cache_dir(State) ->
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
    ok = filelib:ensure_dir(filename:join([CacheDir, "dummy.file"])),
    rebar_log:log(debug, "IDL cache path ~p~n", [CacheDir]),
    CacheDir.

%%% ===================================================================
-spec compile_idl_file(CacheDir::string(),
                       pid(),
                       {file, Path::string(), Options::list()}) ->
                              ok | {warning, string()}
                                  | error
                                  | {error, list(string()), list(string())}.
%%% @doc
%%%  Compile a IDL file with the `ic' module.
%%%  Check documentation http://www.erlang.org/doc/man/ic.html
%%% @end
%%% ===================================================================
compile_idl_file(CacheDir, Pid, {file, Path, Opts}) ->
    rebar_log:log(debug, "Compiling ~p with options ~p~n", [Path, Opts]),
    case ic:gen(Path, Opts) of
        ok ->
            update_cache_file(CacheDir, Path),
            Pid ! ok;
        {ok, [Warning]} ->
            update_cache_file(CacheDir, Path),
            Pid ! {warning, Warning};
        error ->
            Pid ! error;
        {error, [Warning], [Error]} ->
            Pid ! {error, Warning, Error}
    end.

%%% ===================================================================
-spec update_cache_file(CacheDir::string(), Path::string()) ->
                               ok | {error, string()}.
%%% @doc
%%%  Creates a cache file to be able to determine if
%%%  we need to recompile the IDL file.
%%% @end
%%% ===================================================================
update_cache_file(CacheDir, IdlPath) ->
    IdlLastMod = filelib:last_modified(IdlPath),
    CacheFileName = get_cache_filename(CacheDir, IdlPath),
    case filelib:is_file(CacheFileName) of
        true ->
            %% Update modified time
            ok = file:change_time(CacheFileName, IdlLastMod, IdlLastMod);
        false ->
            %% Create
            ok = file:write_file(CacheFileName, ""),
            ok = file:change_time(CacheFileName, IdlLastMod, IdlLastMod)
    end.

%%% ===================================================================
%-spec is_changed(CacheDir::string(), Path::string()) -> boolean().
%%%% @doc
%%%%  Check if there exist a cache file that is newer than the IDL file.
%%%%  If it does, we don't need to compile this IDL file.
%%%% @end
%%%% ===================================================================
%is_changed(CacheDir, IdlPath) ->
%    erlang:display({is_changed, CacheDir, IdlPath}),
%%    IdlLastMod = filelib:last_modified(IdlPath),
%%    CacheFileName = get_cache_filename(CacheDir, IdlPath),
%%    CacheLastMod = filelib:last_modified(CacheFileName),
%%
%%    %% Is Cache older or newer than IDL file (and does it exist)
%%    CacheLastMod /= IdlLastMod,
%    true.

%%% ===================================================================
-spec get_cache_filename(CacheDir::string(), IdlPath::string()) -> string().
%%% @doc
%%%  Return the cache filename for a file.
%%% @end
%%% ===================================================================
get_cache_filename(CacheDir, IdlPath) ->
    BaseName = filename:basename(IdlPath),
    CacheFile = filename:join([CacheDir, BaseName]),
    ok = filelib:ensure_dir(CacheFile),
    CacheFile.

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
        {ok, Warnings} ->
            lists:foreach(fun (Warning) ->
                                  rebar_log:log(warning, "~p~n", [Warning])
                          end, Warnings);
        error ->
            throw(unknown_error);
        {error, Warnings, Errors} ->
            lists:foreach(fun (Warning) ->
                                  rebar_log:log(warning, "~p~n", [Warning])
                          end, Warnings),
            lists:foreach(fun (Error) ->
                                  rebar_log:log(error, "~p~n", [Error])
                          end, Errors)
    after ?TIMEOUT ->
            rebar_log:log(error, "IDL compiler timed out.", []),
            throw(idl_timed_out)
    end,
    wait_until_finished(Left-1).
