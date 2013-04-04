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
    start_link/1,
    chan_add/1,
    chan_del/1,
    chan_event/2
]).

-record(state, {
    chans,
    perm,
    probe_modules
}).

-define(MASTER_CHAN, 'target-MasterChan').
%%-------------------------------------------------------------
%% API
%%-------------------------------------------------------------
% @private
-spec start_link([any()]) -> {ok, pid()}.
start_link(ProbeModules) ->
    gen_server:start_link({local, ?MASTER_CHAN}, ?MODULE, [ProbeModules], []).



%%-------------------------------------------------------------
%% API for the tracker_target_channel modules
%%-------------------------------------------------------------
-spec chan_add(#target{}) -> ok.
% @doc
% Called by a target_channel at initialisation stage.
% @end
chan_add(Target) ->
    gen_server:call(?MASTER_CHAN, {chan_add, Target}).

-spec chan_event(probe_status, {#target{}, #probe{}}) -> ok.
% @doc
% @end
chan_event(probe_status_move, {Target, Probe}) ->
    gen_server:call(?MASTER_CHAN, {probe_status_move, {Target, Probe}}).
    
-spec chan_del(#target{}) -> ok.
% @doc
% Called by a target_channel at termination stage.
% @end
chan_del(Target) ->
    gen_server:call(?MASTER_CHAN, {chan_del, Target}).

%%-------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%-------------------------------------------------------------
% @private
init([ProbeModules]) ->
    PMods = lists:foldl(fun(PMod, Acc) ->
        {ok, Info} = PMod:info(),
        [{PMod, Info} | Acc] 
    end, [], ProbeModules),
        
    {ok, #state{
            chans = [],
            perm = #perm_conf{
                read    = ["admin", "wheel"],
                write   = ["admin"]
            },
            probe_modules = PMods
        }
    }.
    
handle_call({probe_status_move, {
            #target{id = TargetId} = NewTarget, 
            #probe{permissions = Perm} = Probe
            }
        }, _F, #state{chans = Chans} = S) ->
    NewChans = lists:keyreplace(TargetId, 2, Chans, NewTarget),
    ifs_mpd:multicast_msg(?MASTER_CHAN, {Perm, 
        pdu(probeInfo, {update, TargetId, Probe})}),
    {reply, ok, S#state{chans = NewChans}};

handle_call({chan_add, #target{id = Id, properties = Prop} = Target}, _F, 
        #state{chans = C} = S) ->
    {global_perm, Perm} = get_property(global_perm, Prop),
    case lists:keyfind(Id, 2, C) of
        false ->    % did not exist insert
            ifs_mpd:multicast_msg(?MASTER_CHAN, {Perm,
                pdu(targetInfo, Target)}),
            {reply, ok, S#state{
                    chans = [Target | C]
                }
            };
        _ ->        % exist update
            ifs_mpd:multicast_msg(?MASTER_CHAN, {Perm,
                pdu(targetInfo, Target)}),
            {reply, ok, 
                S#state{
                    chans = lists:keyreplace(Id, 2, C, Target)
                }
            }
    end;

handle_call({chan_del, #target{id = Id, global_perm = Perm}}, _F, 
        #state{chans = C} = S) ->
    ifs_mpd:multicast_msg(?MASTER_CHAN, {Perm, pdu(targetDelete, Id)}),
    {reply, ok, S#state{chans = lists:keydelete(Id, 2, C)}};







%%-------------------------------------------------------------
%% These calls are used by the gen_channel behaviour module
%%-------------------------------------------------------------
% Called by ifs_mpd via gen_channel to allow or not a client to subscribe in 
% regard of the result after applying beha_ifs_ctrl:satisfy/3.
handle_call(get_perms, _F, #state{perm = P} = S) ->
    {reply, P, S};

handle_call({synchronize, CState}, _F, State) ->
    dump_known_data(CState, State),
    ifs_mpd:subscribe_stage3(?MASTER_CHAN, CState),
    {reply, ok, State}.







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
dump_known_data(#client_state{module = CMod} = ClientState, 
        #state{chans = Chans, probe_modules = PMods}) ->
    lists:foreach(fun(PMod) ->
        CMod:send(ClientState, pdu(probeModInfo, PMod))
    end, PMods),
    lists:foreach(fun(#target{global_perm = Perm} = Target) ->
        spawn(fun() ->
            ifs_mpd:unicast_msg(ClientState, 
                    {Perm, pdu(targetInfo, Target)}),
            lists:foreach(fun(Probe) -> 
                ifs_mpd:unicast_msg(ClientState,
                    {
                        Probe#probe.permissions,
                        pdu(probeInfo, {create, Target#target.id, Probe})
                    }
                )
            end, Target#target.probes)
        end)
    end, Chans).

pdu(targetInfo, #target{id = Id, properties = Prop}) ->
    AsnProp = lists:foldl(fun({X,Y}, Acc) ->
        [{'Property', atom_to_list(X), tuple_to_list(Y)} | Acc]
    end, [], Prop),
    {modTrackerPDU,
        {fromServer,
            {targetInfo,
                {'TargetInfo',
                    atom_to_list(Id),
                    AsnProp,
                    create}}}};

pdu(targetDelete, Id) ->
    {modTrackerPDU,
        {fromServer,
            {targetInfo,
                {'TargetInfo',
                    atom_to_list(Id),
                    delete}}}};

% pdu(probeInfo,  {InfoType, TargetId, Probe}) ->
%     {modTrackerPDU,
%         {fromServer,
%             {probeInfo,
%                 {'ProbeInfo',
%                     atom_to_list(TargetId),
%                     Probe#probe.id,
%                     Probe#probe.name,
%                     Probe#probe.type,
%                     atom_to_list(Probe#probe.tracker_probe_mod),
%                     atom_to_list(Probe#probe.status),
%                     Probe#probe.step,
%                     Probe#probe.timeout_max,
%                     Probe#probe.timeout_wait,
%                     lists:map(fun(X) -> 
%                         atom_to_list(X) 
%                     end, Probe#probe.inspectors),
%                     InfoType}}}};

pdu(probeModInfo,  {ProbeName, ProbeInfo}) ->
    {modTrackerPDU,
        {fromServer,
            {probeModInfo,
                {'ProbeModuleInfo',
                    atom_to_list(ProbeName),
                    ProbeInfo }}}}.

get_property(Atom, PropertyList) ->
    lists:keyfind(Atom, 1, PropertyList).
