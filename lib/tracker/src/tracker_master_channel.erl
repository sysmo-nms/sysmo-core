% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% target, monitor network hosts and services, provide a consistent
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
-module(tracker_master_channel).
-behaviour(gen_server).
-behaviour(gen_channel).
-include("../include/tracker.hrl").

-export([
    init/1,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-export([
    start_link/0,
    chan_add/1,
    chan_del/1,
    chan_event/1
]).

-record(state, {
    chans,
    perm
}).

-define(MASTER_CHAN, 'target-MasterChan').
%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @private
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MASTER_CHAN}, ?MODULE, [], []).


%%-------------------------------------------------------------
%% API for the tracker_target_channel modules
%%-------------------------------------------------------------
-spec chan_add({target_id(), #perm_conf{}}) -> ok.
% @doc
% Called by a target_channel at initialisation stage.
% @end
chan_add({Id, Perm}) ->
    gen_server:call(?MASTER_CHAN, {chan_add, Id, Perm}).

-spec chan_event(any()) -> ok.
% @doc
%
% @end
chan_event(Any) ->
    gen_server:call(?MASTER_CHAN, {chan_event, Any}).
    
-spec chan_del(target_id()) -> ok.
% @doc
% Called by a target_channel at termination stage.
% @end
chan_del(Id) ->
    gen_server:call(?MASTER_CHAN, {chan_del, Id}).

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
% @private
init([]) ->
    {ok, #state{
            chans = [],
            perm = #perm_conf{
                read = ["admin", "wheel"],
                write = ["admin"]
            }
        }
    }.
    
handle_call({chan_event, Any}, _F, S) ->
    io:format("new event ~p~n",[Any]),
    {reply, ok, S};

handle_call({chan_add, Id, Perm}, _F, #state{chans = C} = S) ->
    case lists:keyfind(Id, 1, C) of
        false ->
            {reply, ok, S#state{
                    chans = [{Id, Perm} | C]
                }
            };
        _ ->
            {reply, ok, 
                S#state{
                    chans = lists:keyreplace(Id, 1, C, {Id, Perm})
                }
            }
    end;

handle_call({chan_del, Id}, _F, #state{chans = C} = S) ->
    {Id, Perm} = lists:keyfind(Id, 1, C),
    ifs_mpd:multicast_msg(?MASTER_CHAN, {Perm, pdu(targetDelete, Id)}),
    {reply, ok, S#state{chans = lists:keydelete(Id, 1, C)}};







%%-------------------------------------------------------------
%% These calls are used by the gen_channel behaviour module
%%-------------------------------------------------------------
% Called by ifs_mpd via gen_channel to allow or not a client to subscribe in 
% regard of the result after applying beha_ifs_ctrl:satisfy/3.
handle_call(get_perms, _F, #state{perm = P} = S) ->
    {reply, P, S};

handle_call({synchronize, CState}, _F, #state{chans = Chans} = S) ->
    dump_known_data(CState, Chans),
    ifs_mpd:subscribe_stage3(?MASTER_CHAN, CState),
    {reply, ok, S}.







handle_cast(_R, S) ->
    {noreply, S}.

% OTHER
% @private
handle_info(I, S) ->
    io:format("handle_info ~p ~p ~p~n", [?MODULE, I, S]),
    {noreply, S}.

% @private
terminate(_R, _S) ->
    normal.

% @private
code_change(_O, S, _E) ->
    {ok, S}.



%% PRIVATE FUNS
dump_known_data(ClientState, Chans) ->
    lists:foreach(fun({TargetId, Perm}) ->
        spawn(fun() ->
            ifs_mpd:unicast_msg(ClientState, 
                {Perm, pdu(targetInfo, TargetId)})
        end)
    end, Chans).

pdu(targetInfo, Id) ->
    {modTrackerPDU,
        {fromServer,
            {targetInfo,
                {'TargetInfo',
                    atom_to_list(Id),
                    create}}}};

pdu(targetDelete, Id) ->
    {modTrackerPDU,
        {fromServer,
            {targetInfo,
                {'TargetInfo',
                    atom_to_list(Id),
                    delete}}}}.

