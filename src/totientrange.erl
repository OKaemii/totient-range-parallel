-module(totientrange2Workers).

-export([start_server/0, server/0, totientWorker/0, hcf/2, relprime/2, euler/1, sumTotient/2]).


start_server() ->
  register(server, spawn(totientrange2Workers, server, [])),
  true.

server() ->
  receive
    {range, Lower, Upper} ->
      %% Father = self(),

      %% start time
      {_, S, US} = os:timestamp(),

      %% spawns and registers two totientWorkers
      Worker_PID1 = spawn(totientrange2Workers, totientWorker, []),
      Worker_PID2 = spawn(totientrange2Workers, totientWorker, []),

      %% send messages that they compute half of the range each
      %% io:format("~p sends with ~p:(~p, ~p); ~p:(~p, ~p).~n",[Father, Worker_PID1, Lower, Upper/2,Worker_PID2,Upper/2 + 1, Upper]),
      Worker_PID1 ! {range, round(Lower), round(Upper/2)},
      Worker_PID2 ! {range, round(Upper/2 + 1), round(Upper)},

      %% receives the range sum from the two workers
      Result_1 = receive {range, Calc1} -> Calc1 end,
      io:format("Server: Received Sum ~p~n", [Result_1]),
      Result_2 = receive {range, Calc2} -> Calc2 end,
      io:format("Server: Received Sum ~p~n", [Result_2]),
      Total = Result_1 + Result_2,

      %% prints total, continue as server
      io:format("Server: Sum of totients: ~p~n",[Total]),

      Worker_PID1 ! finished,
      Worker_PID2 ! finished,
      printElapsed(S,US);

    finished ->
      io:format("Server has disconnected.~n", []),
      done
  end.

totientWorker() ->
  receive
    %% compute the sum of the totient range
  end.

hcf(X,0) ->
  X;
hcf(X,Y) ->
  hcf(Y,X rem Y).

%% relprime x y = hcf x y == 1
relprime(X,Y) ->
  V = hcf(X,Y),
  if
    V == 1
      -> true;
    true
      -> false
  end.

%%euler n = length (filter (relprime n) (mkList n))
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
  io:format("Server: Time taken in Secs, MicroSecs ~p ~p~n",[S3-S,US3-US]).

%%sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])
sumTotient(Lower,Upper) ->
  Res = lists:sum(lists:map(fun euler/1,lists:seq(Lower, Upper))),
  Res.
