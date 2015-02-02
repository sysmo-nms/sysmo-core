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
package io.noctopus.nchecks;
import java.util.Map;
import java.util.HashMap;
import com.ericsson.otp.erlang.*;

public class Reply
{
    private static OtpErlangAtom atomNchecksReply = new OtpErlangAtom("nchecks_reply");
    private String replyMsg;
    private String errorMsg;
    private String status;
    private long   timestamp;
    private Map<String,Long> perfValues;


    public Reply() {
        replyMsg = "";
        errorMsg = "";
        status   = "DOWN";
        timestamp = System.currentTimeMillis() / 1000;
        perfValues = new HashMap<String,Long>();
    }

    public void setReply(String str) {
        replyMsg = str;
    }
    public String getReply() {
        return replyMsg;
    }


    public void setStatus(String str) {
        status = str;
    }
    public String getStatus() {
        return status;
    }

    public void putPerformance(String key, long value) {
        perfValues.put(key, new Long(value));
    }

    public OtpErlangTuple asTuple() {
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

        OtpErlangObject[] replyRecord = new OtpErlangObject[5];
        replyRecord[0] = atomNchecksReply;
        replyRecord[1] = new OtpErlangString(status);
        replyRecord[2] = perfValueList;
        replyRecord[3] = new OtpErlangString(replyMsg);
        replyRecord[4] = new OtpErlangLong(timestamp);

        OtpErlangTuple replyTuple = new OtpErlangTuple(replyRecord);
        return replyTuple;
    }
}
