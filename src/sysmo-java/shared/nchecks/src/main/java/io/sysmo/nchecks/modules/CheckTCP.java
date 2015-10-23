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

package io.sysmo.nchecks.modules;

import io.sysmo.nchecks.NChecksInterface;
import io.sysmo.nchecks.Argument;
import io.sysmo.nchecks.Reply;
import io.sysmo.nchecks.Query;

import java.net.Socket;
import java.net.InetSocketAddress;
import java.net.InetAddress;

import io.sysmo.nchecks.Status;
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
    private Status  refuseState = Status.CRITICAL;
    private Status  acceptState = Status.OK;

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
            if (refuseStateArg  != null) {
                refuseState = Status.fromString(refuseStateArg.asString());
            }
            if (acceptStateArg  != null) {
                acceptState = Status.fromString(acceptStateArg.asString());
            }
        } catch (Exception|Error e) {
            CheckTCP.logger.error(e.getMessage(), e);
            reply.setStatus(Status.ERROR);
            reply.setReply("Bad or wrong arguments: " + e);
            return reply;
        }


        if (port == 0 || port > 65535) {
            reply.setStatus(Status.ERROR);
            reply.setReply("CheckTCP ERROR: Bad port definition " + port);
            return reply;
        }

        InetAddress addr;
        try {
            addr = InetAddress.getByName(host);
        } catch (Exception e) {
            CheckTCP.logger.error(e.getMessage(), e);
            reply.setStatus(Status.ERROR);
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
            CheckTCP.logger.error(e.getMessage(), e);
            reply.setReply(e.getMessage());
            reply.setStatus(refuseState);
            return reply;
        }

        long elapsed = (stop - start) / 1000000;
        reply.putPerformance("ReplyDuration", elapsed);
        Status st;
        if (Status.OK.equals(acceptState))
        {
            if (elapsed >= msCritical) {
                st = Status.CRITICAL;
            } else if (elapsed >= msWarning) {
                st = Status.WARNING;
            } else {
                st = Status.OK;
            }
        } else {
            st = acceptState;
        }
        reply.setStatus(st);
        reply.setReply("CheckTCP " + st + "Time elapsed: "  + elapsed + " milliseconds");
        return reply;
    }
}
