%% Copyright (c) 2010-2011, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(icmp).
-export([ping/1]).

-record(icmp, {
        valid,
        type, code, checksum,
        id, sequence,
        gateway,
        un,
        mtu
    }).

-record(state, {
        s,          % socket
        id,         % ping ID
        ip          % IP Address
    }).

-define(ICMP_ECHO_REPLY, 0).
-define(ICMP_ECHO, 8).

ping(IP) ->
    Id = crypto:rand_uniform(0, 16#FFFF),
    {ok, FD} = procket:open(0, [
            {protocol, icmp}, {type, raw}, {family, inet}]),
    {ok, S} = gen_udp:open(0, [binary, {fd, FD}]),
    send(#state{
            s = S,
            id = Id,
            ip = IP 
        }).


send(#state{s = S, id = Id, ip = IP} = _State) ->
    Packet = make_packet(Id, 0),
    ok = gen_udp:send(S, IP, 0, Packet),
    receive
        {udp, S, _IP, _Port, <<_:20/bytes, Data/binary>>} ->
            case icmp(Data) of
                {_ICMP, <<Mega:32/integer, Sec:32/integer, 
                        Micro:32/integer, _Payload/binary>>} ->
                    {ok, timer:now_diff(erlang:now(), {Mega,Sec,Micro})};
                _ ->
                    error_somewhere
            end
    after
        5000 ->
            {noresponse, Packet}
    end.

make_packet(Id, Seq) ->
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

icmp(<<?ICMP_ECHO_REPLY:8, 0:8, Checksum:16, Id:16, 
                Sequence:16, Payload/binary>>) ->
    {#icmp{
            type = ?ICMP_ECHO_REPLY, code = 0, checksum = Checksum, id = Id,
            sequence = Sequence
        }, Payload};
icmp(_) ->
    false.
