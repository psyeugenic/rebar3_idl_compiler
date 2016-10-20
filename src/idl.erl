-module(idl).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    %% initialize all commands here
    {ok, State1} = idl_prv_compile:init(State),
    {ok, State2} = idl_prv_clean:init(State1),
    {ok, State2}.
