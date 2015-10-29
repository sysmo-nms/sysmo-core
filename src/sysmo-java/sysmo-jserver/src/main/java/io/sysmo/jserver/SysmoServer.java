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
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
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

    private static Server jettyThread   = null;
    private static Thread nchecksThread = null;
    private static Thread rrd4jThread   = null;
    private static Thread snmp4jThread  = null;
    private static Thread mailThread    = null;
    private static Thread eventDbThread = null;

    private static OtpMbox mainMbox = null;
    private static String exitReason = "normal";

    private static Logger logger = LoggerFactory.getLogger(SysmoServer.class);

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

        SysmoServer.logger.info("Logger started");

        SysmoServer.exitReason = "normal";

        try {
            SysmoServer.startServerLoop(args);
        } catch (Exception e) {
            SysmoServer.logger.warn(e.getMessage(),e);
        }

        // cleanup
        if (SysmoServer.mainMbox != null) {
            SysmoServer.mainMbox.exit(SysmoServer.exitReason);
        }
            /* Will raise OtpErlangExit exception on other threads and cause them
             * to terminate.
             */
        if (StateServer.isStarted()) {
            StateServer.stop();
        }

        if (SysmoServer.jettyThread != null) {
            try {
                SysmoServer.jettyThread.stop();
            } catch (Exception e) {
                SysmoServer.logger.error(e.getMessage(), e);
            }
        }

        if (SysmoServer.nchecksThread != null) {
            try {
                SysmoServer.nchecksThread.join();
            } catch (InterruptedException e) {
                SysmoServer.logger.error(e.getMessage(), e);
            }
        }

        if (SysmoServer.rrd4jThread != null) {
            try {
                SysmoServer.rrd4jThread.join();
            } catch (InterruptedException e) {
                SysmoServer.logger.error(e.getMessage(), e);
            }
        }

        if (SysmoServer.snmp4jThread != null) {

            try {
                SysmoServer.snmp4jThread.join();
            } catch (InterruptedException e) {
                SysmoServer.logger.error(e.getMessage(), e);
            }
        }
        if (SysmoServer.mailThread != null) {
            try {
                SysmoServer.mailThread.join();
            } catch (InterruptedException e) {
                SysmoServer.logger.error(e.getMessage(), e);
            }

        }
        if (SysmoServer.eventDbThread != null) {
            try {
                SysmoServer.eventDbThread.join();
            } catch (InterruptedException e) {
                SysmoServer.logger.error(e.getMessage(), e);
            }
        }
        System.exit(0);
    }

    private static void startServerLoop(String[] args) throws Exception {
        // Extract args
        String foreignNodeName;
        String erlangCookie;
        try {
            foreignNodeName = args[0];
            erlangCookie = args[1];
        } catch (ArrayIndexOutOfBoundsException e) {
            SysmoServer.logger.error(e.getMessage(), e);
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
            node = new OtpNode(SysmoServer.selfNodeName, erlangCookie);
            if (!node.ping(foreignNodeName, 2000)) {
                throw new TimeoutException("Can t connect to main erlang node.");
            }
        } catch (IOException e) {
            SysmoServer.exitReason = "OtpNode_io_exception";
            throw e;
        } catch (TimeoutException e) {
            SysmoServer.exitReason = "OtpNode_timed_out";
            throw e;
        }

        SysmoServer.mainMbox = node.createMbox();

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
            SysmoServer.mainMbox.link(rrd4jMbox.self());
            SysmoServer.mainMbox.link(snmp4jMbox.self());
            SysmoServer.mainMbox.link(nchecksMbox.self());
            SysmoServer.mainMbox.link(derbyMbox.self());
            SysmoServer.mainMbox.link(mailMbox.self());
            SysmoServer.mainMbox.link(stateDummyMbox.self());
        } catch (Exception e) {
            SysmoServer.exitReason = "mbox_init_error";
            throw e;
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
                SysmoServer.exitReason = "state_server_init_error";
                throw e;
            }
        }

        /*
         * Create derby thread
         */
        EventDb eventDb;
        try {
            eventDb = EventDb.getInstance(derbyMbox, foreignNodeName, dataDir);
        } catch (Exception e) {
            SysmoServer.exitReason = "derby_init_error";
            throw e;
        }
        SysmoServer.eventDbThread = new Thread(eventDb);
        SysmoServer.eventDbThread.start();

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
            SysmoServer.exitReason = ("nchecks_init_error");
            throw e;
        }
        SysmoServer.nchecksThread = new Thread(nchecks);
        SysmoServer.nchecksThread.start();

        /*
         * Create rrd4j thread
         */
        RrdLogger rrd4j = RrdLogger.getInstance(rrd4jMbox, foreignNodeName);
        SysmoServer.rrd4jThread = new Thread(rrd4j);
        SysmoServer.rrd4jThread.start();

        /*
         * Create mail thread
         */
        MailSender mail = MailSender.getInstance(mailMbox, etcDir);
        SysmoServer.mailThread = new Thread(mail);
        SysmoServer.mailThread.start();

        /*
         * Create snmp4j thread
         */
        SnmpManager snmp4j = SnmpManager.getInstance(snmp4jMbox,
                foreignNodeName, etcDir);
        SysmoServer.snmp4jThread = new Thread(snmp4j);
        SysmoServer.snmp4jThread.start();

        /*
         * Create simple http file server
         */
        SysmoServer.jettyThread = JettyServer.startServer(docrootDir, etcDir);
        int jettyPort = JettyServer.getPort();


        /*
         * Send acknowledgement to the "sysmo" erlang process
         */
        OtpErlangObject[] ackObj = new OtpErlangObject[8];
        ackObj[0] = new OtpErlangAtom("java_connected");
        ackObj[1] = SysmoServer.mainMbox.self();
        ackObj[2] = rrd4jMbox.self();
        ackObj[3] = snmp4jMbox.self();
        ackObj[4] = nchecksMbox.self();
        ackObj[5] = derbyMbox.self();
        ackObj[6] = mailMbox.self();
        ackObj[7] = new OtpErlangInt(jettyPort);
        OtpErlangTuple ackTuple = new OtpErlangTuple(ackObj);
        SysmoServer.mainMbox.send("j_server", foreignNodeName, ackTuple);

        OtpErlangTuple tuple = (OtpErlangTuple) SysmoServer.mainMbox.receive();
        OtpErlangPid pid = (OtpErlangPid) tuple.elementAt(1);
        SysmoServer.mainMbox.link(pid);
        try {
            SysmoServer.mainMbox.receive(); // will raise exception on foreign pid close
        } catch (OtpErlangExit e) {
            SysmoServer.exitReason = "shutdown";
            throw e;
        } catch (OtpErlangDecodeException e) {
            SysmoServer.exitReason = "decode_exception";
            throw e;
        }
    }
}
