-module(errd).
-include("include/errd.hrl").

% API
-export([
    create/1,
    update/1,
    dump/2,
    dump_delayed/2
]).

% @doc
% rrdtool create
% @end
create(String) ->
    Cmd = lists:concat(["create ", String]),
    errd_server_call_queue:call_rrd(?HIGH_PRIO_CALL_QUEUE, Cmd).

% @doc
% rrdtool update
% @end
update(String) ->
    Cmd = lists:concat(["update ", String]),
    errd_server_call_queue:call_rrd(?HIGH_PRIO_CALL_QUEUE, Cmd).

% @doc
% rrdtool dump
% @end
dump(File, Dest) ->
    Cmd = lists:concat(["dump ", File, " ", Dest]),
    errd_server_call_queue:call_rrd(?LOW_PRIO_CALL_QUEUE, Cmd).

% @doc
% Given a directory, apply dump on all files exampleX.rrd to destination name 
% exampleX.xml.
%
% The function return as soon as the command is sent to the server.
%
% When complete, the function _Fun is called.
% @end
dump_delayed(Dir, Fun) ->
    spawn(fun() ->
        {ok, Filenames} = file:list_dir(Dir),
        lists:foreach(fun(File) ->
            Src = filename:join(Dir, File),
            Dst = lists:concat([Src, ".xml"]),
            Cmd = lists:concat(["dump ", Src, " ", Dst]),
            errd_server_call_queue:call_rrd(?LOW_PRIO_CALL_QUEUE, Cmd)
        end, Filenames),
        Fun()
    end).
