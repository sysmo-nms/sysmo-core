/*
 * Sysmo NMS Network Management and Monitoring solution (http://www.sysmo.io)
 *
 * Copyright (c) 2012-2015 Sebastien Serre <ssbx@sysmo.io>
 *
 * This file is part of Sysmo NMS.
 *
 * Sysmo NMS is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Sysmo NMS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Sysmo.  If not, see <http://www.gnu.org/licenses/>.
 */
package io.sysmo.nchecks;
import java.util.Map;
import java.util.HashMap;

/**
 * The Reply class contain all the values and data related to the execution of
 * a module implementing NCheckInterface (a check module).
 *
 * @see {@link Reply}
 */
public class Query
{
    private Map<String,Argument> arguments;
    private String               stateId;


    /**
     * Private use only.
     *
     * @param args a map of {@link Argument}
     * @param stateId a unique string identifying the probe
     */
    public Query(Map<String,Argument> args, final String stateId)
    {
        this.stateId    = stateId;
        this.arguments  = new HashMap<>(args);
    }


    /**
     * Private use only.
     *
     * No sate Query, used by HelperInterface
     *
     * @param args a map of Argument
     */
    public Query(Map<String,Argument> args)
    {
        this.arguments = new HashMap<>(args);
    }


    /**
     * Retrieve the object stored from previous call.
     *
     * example:
     *
     * ...
     * MyStateClass state = (MyStateClass) reply.getState();
     * if (state == null) {
     *    // initialize a new state then
     *    state = new MySateClass();
     * }
     * ...
     *
     * @see {@link Reply#setState(java.io.Serializable)}
     * @return an Object set from Reply.setState or null
     */
    public Object getState() {
        // Lazy getState. Modules not using states, will not use it.
        StateMessage msg = new StateMessage(StateMessage.GET);
        msg.setKey(this.stateId);
        // getState will get the data from the (possibly) distant state server.
        return StateClient.getState(msg);
    }


    /**
     *
     * @return the unique string identifying the probe
     */
    public String getStateId() {
        return this.stateId;
    }

    /**
     * Return the argument identified by key or null if the key
     * does not exist.
     * @param key the flag identifying the argument
     * @return the argument or null
     * @see {@link io.sysmo.nchecks.Reply}
     * @see {@link io.sysmo.nchecks.Argument}
     */
    public Argument get(String key) {
        if (this.arguments.containsKey(key)) {
            return this.arguments.get(key);
        } else {
            return null;
        }
    }
}
