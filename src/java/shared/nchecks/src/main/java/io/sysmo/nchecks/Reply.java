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
import com.ericsson.otp.erlang.*;

/**
* The Reply class contain all the values and informations related to the execution of 
* a module implementing NCheckInterface (a check module).
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
    private byte[] opaque;
    private long   timestamp;
    private Map<String,PerformanceGroup> perfValues;


    public Reply() {
        replyMsg = "";
        status   = "DOWN";
        timestamp = System.currentTimeMillis() / 1000;
        perfValues = new HashMap<>();
        opaque = new byte[0];
    }

    /**
    * Set the reply string of this check. It should be a human readable string.
    * @param str the string to set.
    */
    public void setReply(String str) {
        replyMsg = str;
    }
    /**
    * Get the reply string.
    */
    public String getReply() {
        return replyMsg;
    }

    /**
    * Set an opaque object in the reply. This data will stored
    * and returned on the next call of the check. This mechanism is actualy used
    * to store and compare COUNTER type values, but can be used as your wish.
    * Here is a serialization example:
    *
    *       ByteArrayOutputStream b = new ByteArrayOutputStream();
    *       ObjectOutputStream    o = new ObjectOutputStream(b);
    *       o.writeObject(myObject);
    *       state = b.toByteArray();
    *       myreply.setState(state);
    */
    public void setState(byte[] value) {
        opaque = value.clone();
    }

    /**
    * Set the return status.
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
    * only one rrd file to update.
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
    */
    public void putPerformance(String group, String key, long value)
    {
        PerformanceGroup groupObj = getPerformanceGroup(group);
        groupObj.putPerformance(key, value);
    }

    /**
     * Return the PerformanceGroup for key groupKey. Create it if it does not
     * exist.
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
     * Return the erlang representation of the Reply.
     */
    public OtpErlangTuple asTuple() {
        OtpErlangObject[] perfValuesObj = new OtpErlangObject[perfValues.size()];
        int i = 0;
        for (Map.Entry<String, PerformanceGroup> entry: perfValues.entrySet())
        {
            String              key      = entry.getKey();
            OtpErlangList       value    = entry.getValue().asList();
            OtpErlangObject[]   objEntry = new OtpErlangObject[2];
            objEntry[0]      = new OtpErlangString(entry.getKey());
            objEntry[1]      = value;
            perfValuesObj[i] = new OtpErlangTuple(objEntry);
            i++;
        }
        OtpErlangList perfValueList = new OtpErlangList(perfValuesObj);

        OtpErlangObject[] replyRecord = new OtpErlangObject[6];
        replyRecord[0] = atomNchecksReply;
        replyRecord[1] = new OtpErlangString(status);
        replyRecord[2] = perfValueList;
        replyRecord[3] = new OtpErlangString(replyMsg);
        replyRecord[4] = new OtpErlangLong(timestamp);
        replyRecord[5] = new OtpErlangBinary(opaque);

        return new OtpErlangTuple(replyRecord);
    }
}

class PerformanceGroup
{
    private Map<String,Long> perfValues;

    public PerformanceGroup()
    {
        perfValues = new HashMap<String, Long>();
    }

    public void putPerformance(String key, long value)
    {
        perfValues.put(key, Long.valueOf(value));
    }

    public OtpErlangList asList() {
        OtpErlangObject[] perfValuesObj = new OtpErlangObject[perfValues.size()];
        int i = 0;
        for (Map.Entry<String, Long> entry: perfValues.entrySet())
        {
            String key      = entry.getKey();
            Long value      = entry.getValue();
            OtpErlangObject[] objEntry = new OtpErlangObject[2];
            objEntry[0] = new OtpErlangString(entry.getKey());
            objEntry[1] = new OtpErlangLong(entry.getValue().longValue());
            perfValuesObj[i] = new OtpErlangTuple(objEntry);
            i++;
        }
        return new OtpErlangList(perfValuesObj);
    }
}
