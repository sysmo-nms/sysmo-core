-module(tlogger_text).
-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/1,
    log/2,
    dump/1
]).

-record(state, {
    file,
    dump_size,
    return_re,
    space_re
}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

log(Pid, Message) ->
    gen_server:cast(Pid, {log, Message}).

dump(Pid) ->
    gen_server:call(Pid, dump).


init(FileName) ->
    {ok, DumpSize} = application:get_env(tlogger_text, dump_size),
    file:write_file(FileName, <<>>, [append]),
    {ok, Newline} = re:compile("\n+"),
    {ok, Spaces}  = re:compile(" +"),
    {ok, #state{
            file = FileName,
            dump_size = DumpSize,
            return_re = Newline,
            space_re  = Spaces
        }
    }.

handle_call(dump, _, #state{file = F, dump_size = DS} = S) ->
    {ok, FInfo} = file:read_file_info(F),
    case FInfo#file_info.size < DS of
        true    ->
            {ok, Data} = file:read_file(F),
            Bin = to_binary(Data);
        false   ->
            {ok, Fd}  = file:open(F, [read]),
            {ok, Data} = file:pread(Fd, {eof, -DS}, DS),
            file:close(Fd),
            Bin = to_binary(Data)
    end,
    {reply, Bin, S};
handle_call(_,_,S) ->
    {reply, ok, S}.

handle_cast({log, Message}, 
        #state{file = F, return_re = RRe, space_re = SRe} = S) ->
    A1 = re:replace(Message, RRe, " ", [{return, list}, global]),
    A0 = re:replace(A1,      SRe, " ", [{return, list}, global]),
    file:write_file(F, lists:append(A0, "\n"), [append]),
    {noreply, S};
handle_cast(_,S) ->
    {noreply, S}.

handle_info(_, S) ->
    {noreply, S}.

terminate(_,_) ->
    ok.

code_change(_,S,_) ->
    {ok, S}.


to_binary(R) when is_atom(R) ->
    <<>>;
to_binary(R) when is_list(R) ->
    list_to_binary(R);
to_binary(R) when is_binary(R) ->
    R.
