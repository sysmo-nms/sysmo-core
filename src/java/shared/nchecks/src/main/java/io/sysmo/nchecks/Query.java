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
 * along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
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
    private byte[]               state;

    public Query(Map<String,Argument> args, byte[] state)
    {
        this.state      = state.clone();
        this.arguments  = new HashMap<>(args);
    }

    public Query(Map<String,Argument> args)
    {
        this.arguments = new HashMap<>(args);
    }


    /*
    * Retrieve the state set by the previous check call.
    * (see Reply.setState)
    * Deserializtaion example:
    * DESERIALIZATION EXAMPLE
    *       ByteArrayInputStream b = new ByteArrayInputStream(opaqueData);
    *       ObjectInputStream o = new ObjectInputStream(b);
    *       Object myObject = o.readObject();
    *       or beter
    *       MyObjectClass = (MyObjectClass) o.readObject();
    */
    public byte[] getState() {return this.state.clone();}

    /*
    * Return the argument corresponding to the key.
    */
    public Argument get(String key) {return this.arguments.get(key);}

}
