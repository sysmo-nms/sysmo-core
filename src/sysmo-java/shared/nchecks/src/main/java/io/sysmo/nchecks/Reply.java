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

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import java.io.Serializable;
import java.util.Map;
import java.util.HashMap;

/**
 * The Reply class contain all the values and informations related to the execution of
 * a module implementing NCheckInterface (a check module).
 * @see io.sysmo.nchecks.Query
 */

public class Reply
{
    public static final String STATUS_OK = "OK";
    public static final String STATUS_WARNING = "WARNING";
    public static final String STATUS_CRITICAL = "CRITICAL";
    public static final String STATUS_DOWN = "DOWN";
    public static final String STATUS_ERROR = "ERROR";

    private static final OtpErlangAtom atomNchecksReply = new OtpErlangAtom("nchecks_reply");
    private String replyMsg;
    private String status;
    private long   timestamp;
    private Map<String,PerformanceGroup> perfValues;
    private int statusCode;


    public Reply() {
        this.statusCode = 0;
        this.replyMsg = "";
        this.status   = "DOWN";
        this.timestamp = System.currentTimeMillis() / 1000;
        this.perfValues = new HashMap<>();
    }

    /**
     * The status code is used to determine if the event should be logged to
     * the event database when two subsequent checks have the same status
     * (see setStatus()).
     *
     * The default status code is 0
     *
     * Here are the rules concerning "status" ans "statusCode":
     * - All status move (ie: from OK to DOWN...) are logged.
     * - If two (or more) consecutive returns have the same status
     * (CRITICAL for example) and the same statusCode, only the first event will
     * be logged.
     * - If two (or more) consecutive returns have the same status
     * (CRITICAL for example) and a different statusCode, every event where a
     * statusCode have changed will be logged.
     *
     * @param statusCode integer representing the status code
     */
    public void setStatusCode(int statusCode) {
        this.statusCode = statusCode;
    }

    /**
     * Set the status code for this reply (see setStatusCode). The default value
     * is 0.
     *
     * @return the actual status code.
     */
    public int getStatusCode() {
        return this.statusCode;
    }

    /**
    * Set the reply string of this check. It should be a human readable string.
    * @param str the string representing the reply.
    */
    public void setReply(String str) {
        replyMsg = str;
    }

    /**
     * Get the reply string.
     * @return the reply string
     */
    public String getReply() {
        return replyMsg;
    }


    /**
     * Store a state for this probe for later calls. 'key' must be taken from
     * Query.getStateId() to insure an unique entry.
     *
     * example:
     *
     * ...
     * Query query = new Query();
     * query.setState(reply.getStateId(), myState);
     *
     * @see Query.getStateId()
     *
     * @param key the key from Query.getStateId();
     * @param value the Object to be stored
     */
    public void setState(String key, Serializable value) {
        StateMessage msg = new StateMessage(StateMessage.SET);
        msg.setKey(key);
        msg.setObjectState(value);
        StateClient.setState(msg);
    }

    /**
     * Set the return status. (see setStatusCode)
     * @param str A return status (Reply.STATUS_{ERROR|OK|WARNING|CRITICAL})
     */
    public void setStatus(String str) {
        status = str;
    }

    /**
     * Get the return status.
     */
    public String getStatus() {
        return status;
    }

    /**
     * Store a performance value. These performances values will be interpreted
     * by rrd as defined in your module xml definition file. Assume there is
     * only one rrd file (with multiple databases) to update.
     * @param key the rrd database name
     * @param value the value to store
     */
    public void putPerformance(String key, long value)
    {
        putPerformance("simple", key, value);
    }

    /**
     * Store a performance value. These performances values will be interpreted
     * by rrd as defined in your module xml definition file.
     * The "group" String is used to identify one of the rrd files to update.
     * Assume a setup with multiple rrd files.
     * @param group the performance group key
     * @param key the key
     * @param value the value
     */
    public void putPerformance(String group, String key, long value)
    {
        PerformanceGroup groupObj = getPerformanceGroup(group);
        groupObj.putPerformance(key, value);
    }

    public void putPerformance(int group, String key, long value)
    {
        this.putPerformance(Integer.toString(group), key, value);
    }

    /**
     * Internal use.
     * Return the PerformanceGroup for key groupKey. Create it if it does not
     * exist.
     * @param groupKey  the performance group key
     * @return the performance group
     */
    private PerformanceGroup getPerformanceGroup(String groupKey)
    {
        PerformanceGroup group = perfValues.get(groupKey);
        if (group == null)
        {
            group = new PerformanceGroup();
            perfValues.put(groupKey, group);
            return group;
        }
        else
        {
            return group;
        }
    }

    /**
     * Internal use only for NChecksErlang.
     * Return the erlang representation of the Reply.
     */
    public OtpErlangTuple asTuple() {
        OtpErlangObject[] perfValuesObj = new OtpErlangObject[this.perfValues.size()];
        int i = 0;
        for (Map.Entry<String, PerformanceGroup> entry: perfValues.entrySet())
        {
            OtpErlangList value = entry.getValue().asList();
            OtpErlangObject[] objEntry = new OtpErlangObject[2];
            objEntry[0] = new OtpErlangString(entry.getKey());
            objEntry[1] = value;
            perfValuesObj[i] = new OtpErlangTuple(objEntry);
            i++;
        }
        OtpErlangList perfValueList = new OtpErlangList(perfValuesObj);

        OtpErlangObject[] replyRecord = new OtpErlangObject[6];
        replyRecord[0] = Reply.atomNchecksReply;
        replyRecord[1] = new OtpErlangString(this.status);
        replyRecord[2] = new OtpErlangLong(this.statusCode);
        replyRecord[3] = perfValueList;
        replyRecord[4] = new OtpErlangString(this.replyMsg);
        replyRecord[5] = new OtpErlangLong(this.timestamp);

        return new OtpErlangTuple(replyRecord);
    }

    // utility classes
    static class PerformanceGroup
    {
        private Map<String,Long> perfValues;

        public PerformanceGroup()
        {
            perfValues = new HashMap<>();
        }

        public void putPerformance(String key, long value)
        {
            perfValues.put(key, value);
        }

        public OtpErlangList asList() {
            OtpErlangObject[] perfValuesObj = new OtpErlangObject[perfValues.size()];
            int i = 0;
            for (Map.Entry<String, Long> entry: perfValues.entrySet())
            {
                OtpErlangObject[] objEntry = new OtpErlangObject[2];
                objEntry[0] = new OtpErlangString(entry.getKey());
                objEntry[1] = new OtpErlangLong(entry.getValue().longValue());
                perfValuesObj[i] = new OtpErlangTuple(objEntry);
                i++;
            }
            return new OtpErlangList(perfValuesObj);
        }
    }
}
