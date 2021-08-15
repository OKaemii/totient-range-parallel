# A Reliable Totient Range Function in Parallel
A reliable implementation of the totient range function in Erlang.
This is a parallel and distributed implementation.
A `server` process is spawned, which watches over its worker processes that it will spawn `NWorker` number of (specified by the user).
Its worker: `totientWorker` process will compute a section of the totient range, all workers will be accumulated in the server during the final step.

This program simulates an environment where any of its important processes may be killed at anytime (a Chaos Monkey).
As such, a reliable implementation will restart them, and proceed as normal.

### Totient Function
In number theory, Euler's totient function counts the positive integers up to a given integer n that are relatively prime to n. It is written using the Greek letter phi as &varphi; or &phi;, and may also be called Euler's phi function.

### Expected Output
`> totientrange:testRobust(4, 3).`\
Watching ClientWorker worker1\
Watching ClientWorker worker2\
Watching ClientWorker worker3\
Watching ClientWorker worker4\
Worker : Computing Range 1 3750\
Worker : Computing Range 3751 7500\
Worker : Computing Range 7501 11250\
Worker : Computing Range 11251 15000\
workerChaos killing worker4\
workerChaos killing worker1\
workerChaos killing worker4
[ true , true , true ]\
Worker : Finished\
Server : Received Sum 4275174\
Worker : Finished\
Server : Received Sum 12824238\
Worker : Finished\
Server : Received Sum 21371600\
Worker : Finished\
Server : Received Sum 29923304\
Server : Sum of totients : 68394316\
Server : Time taken in Secs , MicroSecs 13 302830\

### Manual
compile from the shell: `>c(totientrange).`

`NWorkers` number of workers\
`ATotient` beginning integer for totient range\
`BTotient` ending integer for totient range\
run from the shell:     `>server ! {range, ATotient, BTotient, NWorkers}.`\
