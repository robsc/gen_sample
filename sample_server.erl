-module(sample_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, get_values/3]).

% the record is: State, seen so far, total to sample.

init([]) -> init([100]);
init([X]) when X > 0 -> 
  {A,B,C} = now(), random:seed(A, B, C),
  Table = ets:new(table, [protected, ordered_set]),
  State = {Table, 0, X},
  {ok, State}.

% Internal function to extract all the values from the table.
get_values(_, -1, Acc) -> Acc;
get_values(Table, Num, Acc)  ->
  [{_, Value}] = ets:lookup(Table, Num),
  get_values(Table, Num - 1, [Value | Acc]).

% internal synchronous function to add a single value.
add_value(Value, {Table, Seen, Total}) when Seen < Total ->
  InsertVal = {Seen, Value},
  ets:insert(Table, InsertVal),
  NewState = {Table, Seen + 1, Total},
  NewState;

add_value(Value, {Table, Seen, Total}) ->
  NewState = {Table, Seen + 1, Total},
  Random = random:uniform(Seen + 1) - 1,
  if Random < Total -> 
    NewVar = {Random, Value},
    ets:insert(Table, NewVar);
     true -> ok end,
  NewState.

handle_call(seen_count, _From, State = {Table, Seen, Total}) -> {reply, Seen, State};
handle_call(get_table, _From, State = {Table, _, _}) -> {reply, Table, State};
handle_call(get_values, _From, State = {Table, Seen, Total}) ->
  if Seen < Total -> {reply, get_values(Table, Seen - 1, []), State};
     true -> {reply, get_values(Table, Total - 1, []), State} end;
handle_call({add, Value}, _, State = {_, Seen, _}) -> NewState = add_value(Value, State), {reply, Seen + 1, NewState};
handle_call(stop, _From, State) -> {stop, stop, stop, State}.
terminate(_, {Table, _, _}) -> ets:delete(Table).

handle_cast({add, Value}, State) -> {noreply, add_value(Value, State)}.
  
handle_info(Info, State) -> {noreply, State}.

code_change(_OldVsn, State, Extra) -> {ok, State}.