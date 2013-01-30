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
    start_link/0
]).

% -record(icmp, {
%         valid,
%         type, code, checksum,
%         id, sequence,
%         gateway,
%         un,
%         mtu
%     }).

-record(icmp_server_state, {
        socket,         % socket
        icmp_requests   % icmp message waiting for responce
    }).

%-record(icmp_request_state, {
    %ip
%}).

-define(ICMP_ECHO_REPLY, 0).
-define(ICMP_ECHO, 8).

-type microseconds()    :: integer().

-spec start_link() -> {ok, pid()}.
% @doc start the gen_server.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec ping(ip4_address | hostname()) -> {ok, microseconds()} | {error, any()}.
ping(Ip) ->
    gen_server:
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
    {ok, #icmp_server_state{socket = S, icmp_requests = []}}.

handle_call(_R, _F, S) ->
    {reply, ok, S}.

handle_cast(_R, S) ->
    {noreply, S}.

% OTHER
handle_info(_I, S) ->
    {noreply, S}.

terminate(_R, _S) ->
    normal.

code_change(_O, S, _E) ->
    {ok, S}.

% ping(IP) ->
%     Id = crypto:rand_uniform(0, 16#FFFF),
%     {ok, FD} = procket:open(0, [
%             {protocol, icmp}, {type, raw}, {family, inet}]),
%     {ok, S} = gen_udp:open(0, [binary, {fd, FD}]),
%     send(#state{
%             s = S,
%             id = Id,
%             ip = IP 
%         }).
% 
% 
% send(#state{s = S, id = Id, ip = IP} = _State) ->
%     Packet = make_packet(Id, 0),
%     ok = gen_udp:send(S, IP, 0, Packet),
%     receive
%         {udp, S, _IP, _Port, <<_:20/bytes, Data/binary>>} ->
%             case icmp(Data) of
%                 {_ICMP, <<Mega:32/integer, Sec:32/integer, 
%                         Micro:32/integer, _Payload/binary>>} ->
%                     {ok, timer:now_diff(erlang:now(), {Mega,Sec,Micro})};
%                 _ ->
%                     error_somewhere
%             end
%     after
%         5000 ->
%             {noresponse, Packet}
%     end.
% 
% make_packet(Id, Seq) ->
%     {Mega,Sec,USec} = erlang:now(),
% 
%     % Pad packet to 64 bytes
%     Payload = list_to_binary(lists:seq($\s, $K)),
% 
%     CS = makesum(<<?ICMP_ECHO:8, 0:8, 0:16, Id:16, 
%                     Seq:16, Mega:32, Sec:32, USec:32, Payload/binary>>),
%     <<
%     8:8,    % Type
%     0:8,    % Code
%     CS:16,  % Checksum
%     Id:16,  % Id
%     Seq:16, % Sequence
% 
%     Mega:32, Sec:32, USec:32,   % Payload: time
%     Payload/binary
%     >>.
% 
% 
% makesum(Hdr) -> 16#FFFF - checksum(Hdr).
% 
% checksum(Hdr) ->
%     lists:foldl(fun compl/2, 0, [ W || <<W:16>> <= Hdr ]).
% 
% compl(N) when N =< 16#FFFF -> N;
% 
% compl(N) -> (N band 16#FFFF) + (N bsr 16).
% 
% compl(N,S) -> compl(N+S).
% 
% icmp(<<?ICMP_ECHO_REPLY:8, 0:8, Checksum:16, Id:16, 
%                 Sequence:16, Payload/binary>>) ->
%     {#icmp{
%             type = ?ICMP_ECHO_REPLY, code = 0, checksum = Checksum, id = Id,
%             sequence = Sequence
%         }, Payload};
% icmp(_) ->
%     false.
