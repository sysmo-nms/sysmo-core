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

package io.sysmo.jworker;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpMsg;
import com.ericsson.otp.erlang.OtpNode;

import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

import io.sysmo.nchecks.NChecksErlang;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * SysmoWorker try to connect to the specified erlang node and loop on
 * any failures (remote node down/unreachable, connection closed,
 * network unreachable)
 */

public class SysmoWorker {

    static boolean active = true;
    static final String selfShortName = "jworker";

    public static void main(String[] args) throws Exception
    {
        String masterNode = "undefined";
        String masterCookie = "undefined";
        int weight = 10;
        int ackTimeout = 2000;

        String configFile;
        try {
            configFile = args[0];
        } catch (ArrayIndexOutOfBoundsException e) {
            // try default config file location
            configFile = "/etc/sysmo-jworker.properties";
        }

        try {
            Properties prop = new Properties();
            InputStream input = new FileInputStream(configFile);
            prop.load(input);
            masterNode = prop.getProperty("master_node");
            masterCookie = prop.getProperty("master_cookie");
            weight = Integer.parseInt(prop.getProperty("node_weight"));
            ackTimeout = Integer.parseInt(prop.getProperty("ack_timeout"));
        } catch (Exception e) {
            System.err.println(
                    "Error while loading the config file: " + e.toString());
            System.exit(1);
        }
        Logger logger = LoggerFactory.getLogger(SysmoWorker.class);
        logger.info("jworker started");

        // TODO read from a property file
        /*
        masterNode = "sysmo-erlang@127.0.0.1";
        masterCookie = "sysmo-cookie";
        weight = 1;
        ackTimeout = 5000;
        */

        Runtime.getRuntime().addShutdownHook(
                new Thread() {
                    @Override
                    public void run() {SysmoWorker.active = false;}
                }
        );

        /*
         * Indefinitely loop and wait for master to come up
         */
        while (active) {
            Thread.sleep(2000);

            /*
             * Try to connect to erlang master node.
             */
            OtpNode node;
            try {
                node = new OtpNode(SysmoWorker.selfShortName, masterCookie);
                // loop and wait for master node to come up
                if (!node.ping(masterNode, 2000)) {
                    logger.info("Foreign node down");
                    node.close();
                    continue;
                }
            } catch (IOException e) {
                logger.error(e.toString());
                continue;
            }

            logger.info("foreign node alive");

            OtpMbox mainMbox = node.createMbox();
            OtpMbox nchecksMbox = node.createMbox();
            mainMbox.link(nchecksMbox.self());

            /*
             * Create Nchecks thread
             */
            NChecksErlang nchecks =
                    new NChecksErlang(nchecksMbox, masterNode);

            Thread nchecksThread = new Thread(nchecks);
            nchecksThread.start();
            logger.info("nchecks thread started");

            /*
             * Send an availability information to the "nchecks" process
             */
            OtpErlangObject[] readyObj = new OtpErlangObject[5];
            readyObj[0] = new OtpErlangAtom("worker_available");
            readyObj[1] = new OtpErlangString(node.node());
            readyObj[2] = mainMbox.self();
            readyObj[3] = nchecksMbox.self();
            readyObj[4] = new OtpErlangInt(weight);
            OtpErlangTuple ackTuple = new OtpErlangTuple(readyObj);
            mainMbox.send("nchecks", masterNode, ackTuple);
            logger.info("availability object sent, waiting for ack");

            try {
                OtpMsg ack = mainMbox.receiveMsg(ackTimeout);
                OtpErlangAtom atom = (OtpErlangAtom) ack.getMsg();
                if (!atom.toString().equals("worker_ack"))
                    throw new JworkerInitException();
            } catch (Exception e) {
                logger.error("Ack timed out:" + e.toString());
                logger.info("Closing mbox");
                mainMbox.exit("normal");
                logger.info("Closing node");
                node.close();
                nchecksThread.join();
                continue;
            }

            while (node.ping(masterNode, 2000)) {
                try {
                    if (!active) throw new JworkerShutdownException();
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    logger.error(e.toString());
                    break;
                } catch (JworkerShutdownException e) {
                    logger.info(e.toString());
                    break;
                }
            }

            logger.info("Closing node and main mbox");
            // mbox.exit() will raise an exception in the nchecksThread mbox
            node.close();
            mainMbox.exit("normal");

            logger.info("Wait for nchecks thread to join");
            nchecksThread.join();
        }
        System.exit(0);
    }

    static class JworkerShutdownException extends Exception {
        public JworkerShutdownException() {
            super("Shutdown called from the machine");
        }
    }

    static class JworkerInitException extends Exception {
        public JworkerInitException() {
            super("Received bad term");
        }
    }
}
