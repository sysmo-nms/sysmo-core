% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
%
% Enms is a Network Management System aimed to manage and monitor SNMP
% targets, monitor network hosts and services, provide a consistent
% documentation system and tools to help network professionals
% to have a wide perspective of the networks they manage.
%
% Enms is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% Enms is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with Enms.  If not, see <http://www.gnu.org/licenses/>.
% @private
-module(icmp_server).
-behaviour(gen_server).
-include_lib("kernel/include/inet.hrl").

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

% API
-export([
    start_link/0,
    ping/2,
    dump/0
]).


-record(icmp_server, {
        socket,         % socket
        icmp_requests   % icmp message waiting for responce
    }).

-record(icmp_request, {
    id,
    pid,            % pid of the caller
    ip,             % ip required
    timeout         % wait for reply
}).

-define(ICMP_ECHO_REPLY, 0).
-define(ICMP_ECHO, 8).

-type microseconds_delay()    :: integer().

-spec start_link() -> {ok, pid()}.
% @doc
% start the gen_server.
% @end
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

-spec ping(inet:ip4_address() | inet:hostname(), integer()) -> 
        {ok, microseconds_delay()} | {error, any()}.
% @doc 
% Do a call with noreply. handle_info should receive the responce and
% do a gen_server:reply/2 to reply the client. Client will allways have a
% responce, and call(_,_,Timeout+1) should prevent a gen_server:call timeout
% to occur.
% @end
ping(Ip, TimeOut) ->
    Id = crypto:rand_uniform(0, 16#FFFF),
    gen_server:call({global, ?MODULE}, 
            {ping, Id, Ip, TimeOut}, TimeOut + 1000).

%%--------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------
init([]) ->
    {ok, FD} = procket:open(0, 
        [
            {protocol, icmp}, 
            {type, raw}, 
            {family, inet}
        ]
    ),
    {ok, S} = gen_udp:open(0, [binary, {fd, FD}]),
    {ok, #icmp_server{socket = S, icmp_requests = []}}.

%%
%% handle_cast will respond
handle_call({ping, Id, Ip, TimeOut}, From, 
        #icmp_server{icmp_requests = IcmpReqs} = S) ->
    Request = #icmp_request{
        id = Id,
        ip = Ip,
        pid = From,
        timeout = TimeOut
    },
    gen_server:cast({global, ?MODULE}, {init_ping, Request}),
    {noreply, S#icmp_server{icmp_requests = [Request | IcmpReqs]}};

handle_call(dump, _F, S) ->
    {reply, S, S};

handle_call(_R, _F, S) ->
    {reply, ok, S}.

%%
%% start spawn(init_ping)
handle_cast({init_ping, #icmp_request{id = Id, ip = Ip} = Request}, 
                            #icmp_server{socket = Sock} = S) ->
    Packet = make_icmp_packet(Id, 0),
    ok = gen_udp:send(Sock, Ip, 0, Packet),
    spawn(fun() -> ping_timeout(Request) end),
    {noreply, S};

%% called from init_ping. When received, search if the request is in the 
%% #icmp_server.icmp_requests. If it is, remove it and gen_server:reply()
handle_cast({ping_timeout, #icmp_request{id = Id} = Request}, 
        #icmp_server{icmp_requests = ReqList} = S) ->
    case lists:keyfind(Id, 2, ReqList) of
        false ->
            {noreply, S};
        ReqRecord ->
            NewReqList = lists:delete(ReqRecord, ReqList),
            gen_server:reply(Request#icmp_request.pid, {error, noreply}),
            {noreply, S#icmp_server{icmp_requests = NewReqList}}
    end;

handle_cast(_R, S) ->
    {noreply, S}.

%% INFO
handle_info({udp, _Sock, _Ip, _Port, <<_:20/bytes, Data/binary>>}, 
                    #icmp_server{icmp_requests = ReqList} = S) ->
    % known icmp pdu?
    case decode_icmp_pdu(Data) of
        {ok, {Id, {Mega, Sec, Micro}}} ->
            % did cast(ping_timeout) removed it from the server_state?
            case lists:keyfind(Id, 2, ReqList) of
                false ->
                    {noreply, S};
                ReqRecord ->
                    NewReqList = lists:keydelete(Id, 2, ReqList),
                    From = ReqRecord#icmp_request.pid,
                    gen_server:reply(From, {ok, 
                        timer:now_diff(erlang:now(), {Mega, Sec, Micro})}),
                    {noreply, S#icmp_server{icmp_requests = NewReqList}}
            end;
        false ->
            {noreply, S};
        _ ->
            {noreply, S}
    end;

handle_info(_I, S) ->
    {noreply, S}.

terminate(_R, _S) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.


%% PRIVATE FUNCTIONS
%% when timer reched, send a ping_timeout cast.
ping_timeout(Request) ->
    timer:sleep(Request#icmp_request.timeout),
    gen_server:cast({global, ?MODULE}, {ping_timeout, Request}).

make_icmp_packet(Id, Seq) ->
    {Mega,Sec,USec} = erlang:now(),

    % Pad packet to 64 bytes
    Payload = list_to_binary(lists:seq($\s, $K)),

    CS = makesum(<<?ICMP_ECHO:8, 0:8, 0:16, Id:16, 
                    Seq:16, Mega:32, Sec:32, USec:32, Payload/binary>>),
    <<
    8:8,    % Type
    0:8,    % Code
    CS:16,  % Checksum
    Id:16,  % Id
    Seq:16, % Sequence

    Mega:32, Sec:32, USec:32,   % Payload: time
    Payload/binary
    >>.

makesum(Hdr) -> 16#FFFF - checksum(Hdr).

checksum(Hdr) ->
    lists:foldl(fun compl/2, 0, [ W || <<W:16>> <= Hdr ]).

compl(N) when N =< 16#FFFF -> N;

compl(N) -> (N band 16#FFFF) + (N bsr 16).

compl(N,S) -> compl(N+S).

decode_icmp_pdu(<<?ICMP_ECHO_REPLY:8, 0:8, _CheckSum:16, 
                    Id:16, _Sequence:16, Mega:32/integer, Sec:32/integer, 
                            Micro:32/integer, _Payload/binary>>) ->
    {ok, {Id, {Mega, Sec, Micro}}};
    

decode_icmp_pdu(_) ->
    false.


%% DBUG
dump() ->
    gen_server:call({global, ?MODULE}, dump).
