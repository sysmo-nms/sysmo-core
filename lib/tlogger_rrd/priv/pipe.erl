-module(pipe).
-author('Sergiy Dzysyak ').

-compile(export_all).

start()->
    spawn(?MODULE, read, []).
   
read() ->
  Port = open_port({spawn,"/usr/bin/rrdtool - "},[use_stdio, stderr_to_stdout, {line, 255}]),
  do_read(Port).

do_read(Port) ->
  receive
    {Port,{data,Data}} ->
        io:format("Data: ~p~n",[Data]);
    {Port,eof} ->
        io:format("Data:"),
        read();
    {go, Cmd} ->
        io:format("herhe"),
        Port ! {self(), {command, Cmd}};
    Any ->
      io:format("No match fifo_client:do_read/1, ~p~n",[Any])
  end,
  do_read(Port).
