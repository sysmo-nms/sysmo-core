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

package io.sysmo.jserver;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import io.sysmo.nchecks.NChecksErlang;

import org.eclipse.jetty.server.Server;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;

public class SysmoServer {

    private static final String selfNodeName = "sysmo-jserver";

    public static void main(final String[] args) {

        // init logger
        Logger logger = LoggerFactory.getLogger(SysmoServer.class);
        logger.info("Logger started");

        // Extract args
        String foreignNodeName;
        String erlangCookie;
        try {
            foreignNodeName = args[0];
            erlangCookie    = args[1];
        } catch (ArrayIndexOutOfBoundsException e) {
            logger.error(e.getMessage(), e);
            return;
        }

        // generate paths
        FileSystem fs = FileSystems.getDefault();
        String rubyDir    = fs.getPath("ruby").toString();
        String utilsDir   = fs.getPath("utils").toString();
        String etcDir     = fs.getPath("etc").toString();
        String docrootDir = fs.getPath("docroot").toString();
        String dataDir    = fs.getPath("data").toString();

        /*
         * Connect to erlang-main node
         */
        OtpNode node;
        try {
            node = new OtpNode(selfNodeName, erlangCookie);
            if (!node.ping(foreignNodeName, 2000)) {
                logger.error("Can t connect to main erlang node.");
                return;
            }
        } catch (IOException e) {
            logger.error(e.getMessage(), e);
            return;
        }

        OtpMbox mainMbox = node.createMbox();

        OtpMbox rrd4jMbox   = node.createMbox();
        OtpMbox snmp4jMbox  = node.createMbox();
        OtpMbox nchecksMbox = node.createMbox();
        OtpMbox derbyMbox   = node.createMbox();
        OtpMbox mailMbox    = node.createMbox();

        /*
         * Link to mainMbox.
         * this way exiting mainMbox will raise an OtpExitException on
         * other mailboxes, closing the threads.
         */
        try {
            mainMbox.link(rrd4jMbox.self());
            mainMbox.link(snmp4jMbox.self());
            mainMbox.link(nchecksMbox.self());
            mainMbox.link(derbyMbox.self());
            mainMbox.link(mailMbox.self());
        } catch (Exception e) {
            logger.error("Should not append here!" + e.getMessage(), e);
        }


        /*
         * Create derby thread
         */
        EventDb eventDb;
        try {
            eventDb = new EventDb(derbyMbox, foreignNodeName, dataDir);
        } catch (Exception e) {
            logger.error("EventDb failed to start" + e.getMessage(), e);
            System.err.println("EventDb failed to start" + e.getMessage());
            return;
        }
        Thread eventDbThread = new Thread(eventDb);
        eventDbThread.start();

        /*
         * Create NChecks thread
         */
        NChecksErlang nchecks;
        try {
            nchecks = new NChecksErlang(
                    nchecksMbox, foreignNodeName, rubyDir, utilsDir, etcDir);
        } catch (Exception e) {
            logger.error("NChecks failed to start" + e.getMessage(), e);
            System.err.println("NChecks failed to start" + e.getMessage());
            mainMbox.exit("error");
            try {
                eventDbThread.join();
            } catch (Exception inner) {
                logger.error("Fail to shutdown derby: ", inner);
            }
            return;
        }
        Thread nchecksThread = new Thread(nchecks);
        nchecksThread.start();


        /*
         * Create rrd4j thread
         */
        RrdLogger rrd4j = new RrdLogger(rrd4jMbox, foreignNodeName, etcDir);
        Thread rrd4jThread = new Thread(rrd4j);
        rrd4jThread.start();

        /*
         * Create mail thread
         */
        MailSender mail = new MailSender(mailMbox, etcDir);
        Thread mailThread = new Thread(mail);
        mailThread.start();

        /*
         * Create snmp4j thread
         */
        SnmpManager snmp4j = new SnmpManager(snmp4jMbox,
                                                foreignNodeName, etcDir);
        Thread snmp4jThread = new Thread(snmp4j);
        snmp4jThread.start();

        /*
         * Create simple http file server
         */
        Server jettyThread = JettyServer.startServer(docrootDir, etcDir);
        int jettyPort = JettyServer.getPort();

        /*
         * Send acknowledgement to the "sysmo" erlang process
         */
        OtpErlangObject[] ackObj = new OtpErlangObject[7];
        ackObj[0] = new OtpErlangAtom("java_connected");
        ackObj[1] = rrd4jMbox.self();
        ackObj[2] = snmp4jMbox.self();
        ackObj[3] = nchecksMbox.self();
        ackObj[4] = derbyMbox.self();
        ackObj[5] = mailMbox.self();
        ackObj[6] = new OtpErlangInt(jettyPort);
        OtpErlangTuple ackTuple = new OtpErlangTuple(ackObj);
        mainMbox.send("sysmo", foreignNodeName, ackTuple);

        while(node.ping(foreignNodeName, 2000)) try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            break;
        }

        mainMbox.exit("normal");
        /* Will raise OtpErlangExit exception on other threads and cause them
         * to terminate.
         */
        try {
            jettyThread.stop();
            nchecksThread.join();
            rrd4jThread.join();
            snmp4jThread.join();
            mailThread.join();
            eventDbThread.join();
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
        }
        System.exit(0);
    }
}
