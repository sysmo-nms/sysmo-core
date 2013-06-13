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
    chan_update/2
]).

-record(state, {
    chans,
    perm,
    probe_modules
}).

-define(MASTER_CHAN, 'target-MasterChan').


%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
% @private
-spec start_link([any()]) -> {ok, pid()}.
start_link(ProbeModules) ->
    gen_server:start_link({local, ?MASTER_CHAN}, ?MODULE, [ProbeModules], []).


%%----------------------------------------------------------------------------
%% API for the tracker_target_channel(s) modules
%%----------------------------------------------------------------------------

-spec chan_add(#target{}) -> ok.
% @doc
% Called by a target_channel at initialisation stage.
% @end
chan_add(Target) ->
    gen_server:call(?MASTER_CHAN, {chan_add, Target}).

    
-spec chan_del(#target{}) -> ok.
% @doc
% Called by a target_channel at termination stage.
% @end
chan_del(Target) ->
    gen_server:call(?MASTER_CHAN, {chan_del, Target}).

-spec chan_update(
        probe_create    |       % called from a channel
        probe_delete    |       % idem
        probe_update    |       % idem
        chan_update     |       % idem
        wide_warning,           % call from tracker_api or channel
        {#target{}, #probe{}}) -> ok.
% @doc
% Called by a tracker_target_channel when information must be forwarded
% to subscribers of 'target-MasterChan'. Also used from the tracker_api
% module to send arbitrary message to clients wich are subscribed.
% @end
chan_update(Type, {Target, Probe}) ->
    gen_server:call(?MASTER_CHAN, {chan_update, Type, {Target, Probe}}).



%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------


%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% INIT
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
init([ProbeModules]) ->
    P = extract_probes_info(ProbeModules),
    {ok, #state{
            chans = [],
            perm = #perm_conf{
                read    = ["admin", "wheel"],
                write   = ["admin"]
            },
            probe_modules = P
        }
    }.
    
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CALL
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------

%%----------------------------------------------------------------------------
%% SELF API CALLS
%%----------------------------------------------------------------------------
handle_call({probe_status_move, {
            #target{id = TargetId} = NewTarget, 
            #probe{permissions = Perm} = Probe
            }
        }, _F, #state{chans = Chans} = S) ->
    NewChans = lists:keyreplace(TargetId, 2, Chans, NewTarget),
    supercast_mpd:multicast_msg(?MASTER_CHAN, {Perm, 
        pdu(probeInfo, {update, TargetId, Probe})}),
    {reply, ok, S#state{chans = NewChans}};

handle_call({chan_add, #target{id = Id, properties = Prop} = Target}, _F, 
        #state{chans = C} = S) ->
    {global_perm, Perm} = get_property(global_perm, Prop),
    case lists:keyfind(Id, 2, C) of
        false ->    % did not exist insert
            supercast_mpd:multicast_msg(?MASTER_CHAN, {Perm,
                pdu(targetInfo, Target)}),
            {reply, ok, S#state{
                    chans = [Target | C]
                }
            };
        _ ->        % exist update
            supercast_mpd:multicast_msg(?MASTER_CHAN, {Perm,
                pdu(targetInfo, Target)}),
            {reply, ok, 
                S#state{
                    chans = lists:keyreplace(Id, 2, C, Target)
                }
            }
    end;

handle_call({chan_del, #target{id = Id, global_perm = Perm}}, _F, 
        #state{chans = C} = S) ->
    supercast_mpd:multicast_msg(?MASTER_CHAN, {Perm, pdu(targetDelete, Id)}),
    {reply, ok, S#state{chans = lists:keydelete(Id, 2, C)}};

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% CALLS VIA GEN_CHANNEL BEHAVIOUR
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
% These calls are used by the gen_channel behaviour module
% Called by supercast_mpd via gen_channel to allow or not a client to subscribe in 
% regard of the result after applying beha_supercast_ctrl:satisfy/3.
handle_call(get_perms, _F, #state{perm = P} = S) ->
    {reply, P, S};

handle_call({synchronize, IfsCState}, _F, State) ->
    dump_known_data(IfsCState, State),
    supercast_mpd:subscribe_stage3(?MASTER_CHAN, IfsCState),
    {reply, ok, State}.


%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_CAST
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_cast(_R, S) ->
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% HANDLE_INFO
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
handle_info(I, S) ->
    io:format("handle_info ~p ~p ~p~n", [?MODULE, I, S]),
    {noreply, S}.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% TERMINATE  
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
terminate(_R, _S) ->
    normal.

%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% CODE_CHANGE
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
code_change(_O, S, _E) ->
    {ok, S}.



%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%% PRIVATE FUNS
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------

dump_known_data(#client_state{module = CMod} = ClientState, 
        #state{chans = Chans, probe_modules = PMods}) ->
    lists:foreach(fun(PMod) ->
        CMod:send(ClientState, pdu(probeModInfo, PMod))
    end, PMods),
    lists:foreach(fun(#target{global_perm = Perm} = Target) ->
        spawn(fun() ->
            supercast_mpd:unicast_msg(ClientState, 
                    {Perm, pdu(targetInfo, Target)}),
            lists:foreach(fun(Probe) -> 
                supercast_mpd:unicast_msg(ClientState,
                    {
                        Probe#probe.permissions,
                        pdu(probeInfo, {create, Target#target.id, Probe})
                    }
                )
            end, Target#target.probes)
        end)
    end, Chans).

%%----------------------------------------------------------------------------
%% PDU BUILD
%%----------------------------------------------------------------------------
pdu(targetInfo, #target{id = Id, properties = Prop}) ->
    io:format("create pdu 1~n"),
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
    io:format("create pdu 2~n"),
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
    I = {modTrackerPDU,
        {fromServer,
            {probeModInfo,
                {'ProbeModuleInfo',
                    atom_to_list(ProbeName),
                    ProbeInfo }}}},
    io:format("create pdu 3: ~p~n", [I]),
    I.

%%----------------------------------------------------------------------------
%% UTILS    
%%----------------------------------------------------------------------------
get_property(Atom, PropertyList) ->
    lists:keyfind(Atom, 1, PropertyList).

extract_probes_info(ProbeModules) ->
    lists:foldl(
        fun(PMod, Acc) ->
            {ok, Info} = PMod:info(),
            [{PMod, Info} | Acc] 
        end, 
    [], ProbeModules).
