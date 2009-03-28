%% A sampling gen server. 
%% Author: Robert Schonberger. 
%% Copyright 2009 Robert Schonberger
%% See the MIT-LICENSE file for licensing information.
%% 
%% Sample Usage: 
%% gen_server({local, sampler_20}, sample_server, [20], []).
%% [ gen_server:cast(sampler_20, {add, X}) || X <- lists:seq(1, 1000)].
%% Samples = gen_server:call(sampler_20, get_values).
%% gen_server:call(sampler_20, stop).
%% 
%% Lots of TODOs, including:
%%   - improving the argument system
%%   - Including a multiple add convenience call
%%   - Changing and controlling the randomness source.
%%   - Eunit testing. 
%%   - Weighted sampling.

-module(sample_server).
-behaviour(gen_server).
-include_lib("sample_record.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, get_values/3]).

% the record is: State, seen so far, total to sample.

init([]) -> init([100]);
init([X]) when X > 0 -> 
  Table = ets:new(table, [protected, set]),
  State = #sample{table = Table, seen = 0, total = X, seed = now()},
  {ok, State}.

% Internal function to extract all the values from the table.
get_values(_, 0, Acc) -> Acc;
get_values(Table, Num, Acc)  ->
  [{_, Value}] = ets:lookup(Table, Num),
  get_values(Table, Num - 1, [Value | Acc]).

% internal synchronous function to add a single value.
add_value(Value, State) when State#sample.seen < State#sample.total ->
  InsertVal = {State#sample.seen + 1, Value},
  ets:insert(State#sample.table, InsertVal),
  State#sample{seen = State#sample.seen + 1};

add_value(Value, State) ->
  {Random, NewSeed} = random:uniform_s(State#sample.seen + 1, State#sample.seed),
  NewState = State#sample{seen = State#sample.seen + 1, seed = NewSeed},
  if Random =< State#sample.total -> 
    NewVar = {Random, Value},
    ets:insert(State#sample.table, NewVar);
    true -> ok end,
  NewState.

handle_call(seen_count, _From, State) -> {reply, State#sample.seen, State};
handle_call(get_table, _From, State) -> {reply, State#sample.table, State};
handle_call(get_values, _From, State) -> 
  if State#sample.seen < State#sample.total -> {reply, get_values(State#sample.table, State#sample.seen, []), State};
     true -> {reply, get_values(State#sample.table, State#sample.total, []), State} end;
handle_call({add, Value}, _, State) -> NewState = add_value(Value, State), {reply, State#sample.seen + 1, NewState};
handle_call(stop, _From, State) -> {stop, stop, stop, State}.

terminate(_, State) -> ets:delete(State#sample.table).

handle_cast({add, Value}, State) -> {noreply, add_value(Value, State)}.
  
handle_info(_Info,  State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.