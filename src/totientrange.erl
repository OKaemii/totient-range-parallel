-module(totientrange).

-export([start_server/0, server/0, totientWorker/0, watcher/2, testRobust/2, workerChaos/2, hcf/2, relprime/2, euler/1, sumTotient/2]).

%% KINKY STUFF --------------------------------------
testRobust(NWorkers,NVictims) ->
 ServerPid = whereis(server),
 if ServerPid == undefined ->
 start_server();
  true ->
    ok
  end,
 server ! {range, 1, 15000, NWorkers},
 workerChaos(NVictims,NWorkers).
%% --------------------------------------------------

%% HEART OF PROGRAM ---------------------------------
start_server() ->
  register(server, spawn(totientrange, server, [])),
  true.

server() ->
  receive
    {range, Lower, Upper, Num} ->
      %% Father = self(),
      %% start the clock! SPEEEEEEEEEEEEEEEEEEEEEED!
      {_, S, US} = os:timestamp(),

      %% of our range
      Quantity = round((Upper - Lower) + 1),

      %% a function that spawns watcher instances
      SpawnN = fun(I) -> spawn(totientrange, watcher, [I, {range, round(1 + I * Quantity/Num), round(1 + (I+1) * Quantity/Num - 1)}]) end,

      %% spawns and registers Num number of watcher instances
      lists:map(SpawnN, lists:seq(0, Num-1)),

      %% function to receive the range sum from the spawned workers
      Results = fun(I) -> receive {range, R} -> io:format("Server: Received Sum ~p~n", [R]), R end end,
      %% sum the distributed parts of workers to get back whole
      Total = lists:sum(lists:map(Results, lists:seq(0, Num-1))),

      %% prints total
      io:format("Server: Sum of totients: ~p~n",[Total]),

      %% print total execution time of instance
      printElapsed(S,US),

      %% continue as server
      server();

    finished ->
      io:format("Server has disconnected.~n", []),
      done
  end.
%% --------------------------------------------------

%% DEATH TO ALL -------------------------------------
workerChaos(NVictims,NWorkers) ->
 lists:map(
 fun(_) ->
  timer:sleep(500), %% Sleep for .5s
  %% Choose a random victim
  WorkerNum = rand:uniform(NWorkers),
  io:format("workerChaos killing ~p~n",
  [workerName(WorkerNum)]),
  WorkerPid = whereis(workerName(WorkerNum)),
  if %% Check if victim is alive
  WorkerPid == undefined ->
  io:format("workerChaos already dead: ~p~n",
  [workerName(WorkerNum)]);

 true -> %% Kill Kill Kill
  exit(whereis(workerName(WorkerNum)),chaos)
  end
 end,
 lists:seq(1, NVictims )).
%% --------------------------------------------------

%% WORKER GETS ID -----------------------------------
workerName(Num) ->
 list_to_atom("worker" ++ integer_to_list(Num)).
%% --------------------------------------------------

%% MANAGEMENT AND HIS TEAM --------------------------
watcher(I, Payload) ->
  %% worker picks up unique ID card
  Worker = workerName(I+1),
  io:format("Watching ClientWorker ~p~n", [Worker]),

  %% slap on that ID badge PERMANENTLY TILL DEATH for worker
  %% assign worker to be a totientWorker
  register(Worker, spawn_link(totientrange, totientWorker, [])),

  %% task Worker:totientWorker with payload to do for the day
  Worker ! Payload,
  receive
    %% not a crash
    {"EXIT", PID, normal} ->
      ok;

    %% crashed
    {"EXIT", PID, _} ->
      %% if Worker:totientWorker dies, restart it
      watcher(I, Payload);

    finished ->
      io:format("Watcher Finished~n" ,[])
  end.

totientWorker() ->
  %% trap all exits of for program.
  process_flag(trap_exit, true),

  receive
    %% compute the sum of the totient range
    {range, Lower, Upper} ->
      %% display work assigned
      io:format("Worker: Computing Range ~p ~p~n", [Lower, Upper]),
      Calc = sumTotient(round(Lower), round(Upper)),

      %% send sum to server
      server ! {range, Calc},

      %% let worker die after
      io:format("Worker: Finished~n", [])

  end.
%% --------------------------------------------------

%% MATHS LOGIC --------------------------------------
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

%% --------------------------------------------------
