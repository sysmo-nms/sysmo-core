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

import io.sysmo.nchecks.StateServer;
import org.eclipse.jetty.server.Server;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Paths;
import java.util.Properties;
import java.util.concurrent.TimeoutException;
import java.util.logging.LogManager;

public class SysmoServer {

    private static final String selfNodeName = "sysmo-jserver";

    public static void main(final String[] args) {

        // init logger
        LogManager logManager = LogManager.getLogManager();

        InputStream in = null;

        try {
            in = SysmoServer.class.getResourceAsStream("/logging.properties");
            logManager.readConfiguration(in);
        } catch (IOException e) {
            System.out.println("No logging properties file found");
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        }

        Logger logger = LoggerFactory.getLogger(SysmoServer.class);
        logger.info("Logger started");


        Server jettyThread   = null;
        Thread nchecksThread = null;
        Thread rrd4jThread   = null;
        Thread snmp4jThread  = null;
        Thread mailThread    = null;
        Thread eventDbThread = null;

        OtpMbox mainMbox = null;

        String exitReason = "normal";

        try {
            // Extract args
            String foreignNodeName;
            String erlangCookie;
            try {
                foreignNodeName = args[0];
                erlangCookie = args[1];
            } catch (ArrayIndexOutOfBoundsException e) {
                logger.error(e.getMessage(), e);
                return;
            }

            // generate paths
            FileSystem fs = FileSystems.getDefault();
            String rubyDir = fs.getPath("ruby").toString();
            String utilsDir = fs.getPath("utils").toString();
            String etcDir = fs.getPath("etc").toString();
            String docrootDir = fs.getPath("docroot").toString();
            String dataDir = fs.getPath("data").toString();

            String confFile = Paths.get(etcDir, "sysmo.properties").toString();

            InputStream propIn = null;
            int stateServerPort;
            String stateServerEmbedded;
            String stateServerHost;
            try {
                Properties props = new Properties();
                propIn = new FileInputStream(confFile);
                props.load(propIn);
                stateServerPort =
                        Integer.parseInt(props.getProperty("state_server_port"));
                stateServerEmbedded = props.getProperty("state_server_embedded", "true");
                stateServerHost = props.getProperty("state_server_host");
            } catch (Exception e) {
                stateServerPort = 0;
                stateServerEmbedded = "true";
                stateServerHost = "";
            } finally {
                if (propIn != null) {
                    try {
                        propIn.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }
            }

        /*
         * Connect to erlang-main node
         */
            OtpNode node;
            try {
                node = new OtpNode(selfNodeName, erlangCookie);
                if (!node.ping(foreignNodeName, 2000)) {
                    throw new TimeoutException("Can t connect to main erlang node.");
                }
            } catch (IOException e) {
                logger.error(e.getMessage(), e);
                exitReason = "OtpNode_io_exception";
                return;
            } catch (TimeoutException e) {
                logger.error(e.getMessage(), e);
                exitReason = "OtpNode_timed_out";
                return;
            }

            mainMbox = node.createMbox();

            OtpMbox rrd4jMbox = node.createMbox();
            OtpMbox snmp4jMbox = node.createMbox();
            OtpMbox nchecksMbox = node.createMbox();
            OtpMbox derbyMbox = node.createMbox();
            OtpMbox mailMbox = node.createMbox();
            OtpMbox stateDummyMbox = node.createMbox();

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
                mainMbox.link(stateDummyMbox.self());
            } catch (Exception e) {
                logger.error("Should not append here!" + e.getMessage(), e);
                return;
            }

        /*
         * Create state server thread if required
         */
            if (stateServerEmbedded.equals("true")) {
                try {
                    // dummy mbox only used to notify mainMbox of a failure
                    // and stop the JVM.
                    StateServer.start(dataDir, stateServerPort, stateDummyMbox);
                } catch (Exception e) {
                    logger.error(e.getMessage(), e);
                    exitReason = "state_server_init_error";
                    return;
                }
            }

        /*
         * Create derby thread
         */
            EventDb eventDb;
            try {
                eventDb = EventDb.getInstance(derbyMbox, foreignNodeName, dataDir);
            } catch (Exception e) {
                logger.error(e.getMessage(), e);
                exitReason = "derby_init_error";
                return;
            }
            eventDbThread = new Thread(eventDb);
            eventDbThread.start();

        /*
         * Create NChecks thread
         */
            NChecksErlang nchecks;
            try {

                InetAddress stateServerAddress;
                if (stateServerEmbedded.equals("true")) {
                    stateServerAddress = InetAddress.getByName(null);
                } else {
                    stateServerAddress = InetAddress.getByName(stateServerHost);
                }

                nchecks = NChecksErlang.getInstance(nchecksMbox, foreignNodeName,
                        rubyDir, utilsDir, etcDir,
                        stateServerAddress, stateServerPort);
            } catch (Exception e) {
                logger.error(e.getMessage(), e);
                exitReason = ("nchecks_init_error");
                return;
            }
            nchecksThread = new Thread(nchecks);
            nchecksThread.start();

        /*
         * Create rrd4j thread
         */
            RrdLogger rrd4j = RrdLogger.getInstance(rrd4jMbox, foreignNodeName);
            rrd4jThread = new Thread(rrd4j);
            rrd4jThread.start();

        /*
         * Create mail thread
         */
            MailSender mail = MailSender.getInstance(mailMbox, etcDir);
            mailThread = new Thread(mail);
            mailThread.start();

        /*
         * Create snmp4j thread
         */
            SnmpManager snmp4j = SnmpManager.getInstance(snmp4jMbox,
                    foreignNodeName, etcDir);
            snmp4jThread = new Thread(snmp4j);
            snmp4jThread.start();

        /*
         * Create simple http file server
         */
            jettyThread = JettyServer.startServer(docrootDir, etcDir);
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
            mainMbox.send("j_server", foreignNodeName, ackTuple);

            int pingTimeoutMs = 5000; // 5 seconds
            boolean alive;

            while (true) try {
                alive = node.ping(foreignNodeName, pingTimeoutMs);

                if (!alive) {
                    throw new TimeoutException("Erlang node as timed out");
                }

                Thread.sleep(1000);
            } catch (InterruptedException e) {
                logger.error(e.getMessage(), e);
                exitReason = "OtpNode_interrupted";
                return;
            } catch (TimeoutException e) {
                logger.error(e.getMessage(), e);
                exitReason = "OtpNode_timeout";
                return;
            }

        } finally {
            if (mainMbox != null) {
                mainMbox.exit(exitReason);
            }
            /* Will raise OtpErlangExit exception on other threads and cause them
             * to terminate.
             */
            if (StateServer.isStarted()) {
                StateServer.stop();
            }

            if (jettyThread != null) {
                try {
                    jettyThread.stop();
                } catch (Exception e) {
                    logger.error(e.getMessage(), e);
                }
            }

            if (nchecksThread != null) {
                try {
                    nchecksThread.join();
                } catch (InterruptedException e) {
                    logger.error(e.getMessage(), e);
                }
            }

            if (rrd4jThread != null) {
                try {
                    rrd4jThread.join();
                } catch (InterruptedException e) {
                    logger.error(e.getMessage(), e);
                }
            }

            if (snmp4jThread != null) {
                try {
                    snmp4jThread.join();
                } catch (InterruptedException e) {
                    logger.error(e.getMessage(), e);
                }
            }

            if (mailThread != null) {
                try {
                    mailThread.join();
                } catch (InterruptedException e) {
                    logger.error(e.getMessage(), e);
                }
            }

            if (eventDbThread != null) {

                try {
                    eventDbThread.join();
                } catch (InterruptedException e) {
                    logger.error(e.getMessage(), e);
                }
            }
            System.exit(0);
        }
    }
}
