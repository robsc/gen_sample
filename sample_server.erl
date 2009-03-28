-module(sample_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3).

% the record is: State, seen so far, total to sample.

init([]) -> init([100]).
init([X]) when X > 0 -> 
  {A,B,C} = now(), random:seed(A, B, C),
  Table = ets:new(table, [protected, duplicate_bag]),
  State = {Table, 0, X},
  {ok, State}.

handle_call(seen_count, _From, State = {Table, Seen, Total}) -> {reply, Seen, State}.

terminate(_, {Table, _, _}) -> ets:delete(Table).