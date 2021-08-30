-module(totientrange).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
	 sumTotient/2
	]).

hcf(X,0) -> X;
hcf(X,Y) -> hcf(Y,X rem Y).

relprime(X,Y) ->
  V = hcf(X,Y),
  if
    V == 1
      -> true;
    true
      -> false
  end.

euler(N) ->
  RelprimeN = fun(Y) -> relprime(N,Y) end,
  length (lists:filter(RelprimeN,(lists:seq(1,N)))).

%% Take completion timestamp, and print elapsed time

printElapsed(S,US) ->
  {_, S2, US2} = os:timestamp(),
  %% Adjust Seconds if completion Microsecs > start Microsecs
  if
    US2-US < 0 ->
      S3 = S2-1,
      US3 = US2+1000000;
    true ->
      S3 = S2,
      US3 = US2
  end,
  io:format("Time taken in Secs, MicroSecs ~p ~p~n",[S3-S,US3-US]).

%%sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])

sumTotient(Lower,Upper) ->
  {_, S, US} = os:timestamp(),
  Res = lists:sum(lists:map(fun euler/1,lists:seq(Lower, Upper))),
  io:format("Sum of totients: ~p~n", [Res]),
  printElapsed(S,US).
