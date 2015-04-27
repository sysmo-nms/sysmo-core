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

package io.sysmo.nchecks.modules;

import io.sysmo.nchecks.NChecksInterface;
import io.sysmo.nchecks.Argument;
import io.sysmo.nchecks.Reply;
import io.sysmo.nchecks.Query;
import io.sysmo.nchecks.Const;

import java.util.Map;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.net.Socket;
import java.net.InetSocketAddress;
import java.net.InetAddress;
import java.net.Inet4Address;
import java.net.Inet6Address;


public class CheckTCP implements NChecksInterface
{
    private String  host        = "";
    private int     port        = 0;
    private int     msWarning   = 500;
    private int     msCritical  = 2500;
    private int     msTimeout   = 5000;
    private String  refuseState = Const.STATUS_CRITICAL;
    private String  acceptState = Const.STATUS_OK;

    public CheckTCP() {}

    public Reply execute(Query query)
    {
        Reply reply = new Reply();

        Argument hostArg        = query.get("host");
        Argument portArg        = query.get("port");
        Argument msWarningArg   = query.get("ms_warning");
        Argument msCriticalArg  = query.get("ms_critical");
        Argument msTimeoutArg   = query.get("ms_timeout");
        Argument refuseStateArg = query.get("refuse");
        Argument acceptStateArg = query.get("accept");
        /* TODO
        Argument useIpv6        = query.get("force_ipv6");
        Argument escapeChars    = query.get("escape_chars");
        Argument send_string    = query.get("send_string");
        Argument expect_string  = query.get("expect_string");
        */

        try {
            if (hostArg         != null) { host = hostArg.asString(); }
            if (portArg         != null) { port = portArg.asInteger(); }
            if (msWarningArg    != null) { msWarning = msWarningArg.asInteger(); }
            if (msCriticalArg   != null) { msCritical = msCriticalArg.asInteger(); }
            if (msTimeoutArg    != null) { msTimeout = msTimeoutArg.asInteger(); }
            if (refuseStateArg  != null) { refuseState = refuseStateArg.asString(); }
            if (acceptStateArg  != null) { acceptState = acceptStateArg.asString(); }
        } catch (Exception|Error e) {
            e.printStackTrace();
            reply.setStatus(Const.STATUS_ERROR);
            reply.setReply("Bad or wrong argumentis: " + e);
            return reply;
        }


        if (port == 0 || port > 65535) {
            reply.setStatus(Const.STATUS_ERROR);
            reply.setReply("CheckTCP ERROR: Bad port definition " + port);
            return reply;
        }

        InetAddress addr;
        try {
            addr = InetAddress.getByName(host);
        } catch (Exception e) {
            reply.setStatus(Const.STATUS_ERROR);
            reply.setReply("Host lookup fail for: " + host);
            return reply;
        }


        Socket  sock = new Socket();
        Instant start;
        Instant stop;
        try {
            start = Instant.now();
            sock.connect(new InetSocketAddress(addr, port), msTimeout);
            stop = Instant.now();
            sock.close();
        } catch (Exception e) {
            reply.setReply(e.getMessage());
            reply.setStatus(refuseState);
            return reply;
        }

        long elapsed = ChronoUnit.MILLIS.between(start,stop);
        reply.putPerformance("ReplyDuration", elapsed);
        String st = null;
        if (Const.STATUS_OK.equals(acceptState))
        {
            if (elapsed >= msCritical) {
                st = Const.STATUS_CRITICAL;
            } else if (elapsed >= msWarning) {
                st = Const.STATUS_WARNING;
            } else {
                st = Const.STATUS_OK;
            }
        } else {
            st = acceptState;
        }
        reply.setStatus(st);
        reply.setReply("CheckTCP " + st + "Time elapsed: "  + elapsed + " milliseconds");
        return reply;
    }
}
