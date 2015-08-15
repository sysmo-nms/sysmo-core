/* Copyright (C) 2014, Sebastien Serre <sserre.bx@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package io.sysmo.nchecks;
import java.util.Map;
import java.util.HashMap;
import java.io.Serializable;
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
        perfValues = new HashMap<String,PerformanceGroup>();
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

    /*
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

    /*
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

        OtpErlangTuple replyTuple = new OtpErlangTuple(replyRecord);
        return replyTuple;
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
        OtpErlangList perfValueList = new OtpErlangList(perfValuesObj);
        return perfValueList;
    }
}
