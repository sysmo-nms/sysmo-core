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

import java.net.Socket;
import java.net.InetSocketAddress;
import java.net.InetAddress;

import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

public class CheckTCP implements NChecksInterface
{
    private static Logger logger = LoggerFactory.getLogger(CheckTCP.class);

    private String  host        = "";
    private int     port        = 0;
    private int     msWarning   = 500;
    private int     msCritical  = 2500;
    private int     msTimeout   = 5000;
    private String  refuseState = Reply.STATUS_CRITICAL;
    private String  acceptState = Reply.STATUS_OK;

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
            CheckTCP.logger.error(e.toString());
            reply.setStatus(Reply.STATUS_ERROR);
            reply.setReply("Bad or wrong arguments: " + e);
            return reply;
        }


        if (port == 0 || port > 65535) {
            reply.setStatus(Reply.STATUS_ERROR);
            reply.setReply("CheckTCP ERROR: Bad port definition " + port);
            return reply;
        }

        InetAddress addr;
        try {
            addr = InetAddress.getByName(host);
        } catch (Exception e) {
            reply.setStatus(Reply.STATUS_ERROR);
            reply.setReply("Host lookup fail for: " + host);
            return reply;
        }


        Socket  sock = new Socket();
        long start;
        long stop;
        try {
            start = System.nanoTime();
            sock.connect(new InetSocketAddress(addr, port), msTimeout);
            stop  = System.nanoTime();
            sock.close();
        } catch (Exception e) {
            reply.setReply(e.getMessage());
            reply.setStatus(refuseState);
            return reply;
        }

        long elapsed = (stop - start) / 1000000;
        reply.putPerformance("ReplyDuration", elapsed);
        String st;
        if (Reply.STATUS_OK.equals(acceptState))
        {
            if (elapsed >= msCritical) {
                st = Reply.STATUS_CRITICAL;
            } else if (elapsed >= msWarning) {
                st = Reply.STATUS_WARNING;
            } else {
                st = Reply.STATUS_OK;
            }
        } else {
            st = acceptState;
        }
        reply.setStatus(st);
        reply.setReply("CheckTCP " + st + "Time elapsed: "  + elapsed + " milliseconds");
        return reply;
    }
}
