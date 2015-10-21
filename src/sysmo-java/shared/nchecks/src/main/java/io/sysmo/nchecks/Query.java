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
* The Reply class contain all the values and informations related to the execution of 
* a module implementing NCheckInterface (a check module).
*/
public class Query
{
    private Map<String,Argument> arguments;
    private String               stateId;

    public Query(Map<String,Argument> args, final String stateId)
    {
        this.stateId    = stateId;
        this.arguments  = new HashMap<>(args);
    }

    public Query(Map<String,Argument> args)
    {
        this.arguments = new HashMap<>(args);
    }

    /**
     * This identifier is for use with Reply.setState(id, value) as the
     * unique identifier for the probe to store and get data later.
     *
     * @see Reply
     *
     * @return an unique probe identifier
     */
    public String getStateId() {
        return this.stateId;
    };

    /**
     * Retrieve the object stored from previous call.
     *
     * example:
     *
     * MyStateClass state = (MyStateClass) reply.getState();
     *
     * @return an Object set from Reply.setState
     */
    public Object getState() {
        StateMessage msg = new StateMessage(StateMessage.GET);
        msg.setKey(this.stateId);
        return StateClient.getState(msg);
    }

    /**
     * Return the argument identified by key or null if the key
     * does not exist.
     * @param key the flag identifying the argument
     * @return the argument or null
     * @see io.sysmo.nchecks.Reply
     * @see io.sysmo.nchecks.Argument
     */
    public Argument get(String key) {
        if (this.arguments.containsKey(key)) {
            return this.arguments.get(key);
        } else {
            return null;
        }
    }

}
