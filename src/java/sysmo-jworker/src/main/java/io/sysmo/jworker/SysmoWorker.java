package io.sysmo.jworker;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpMsg;
import com.ericsson.otp.erlang.OtpNode;

import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

import io.sysmo.nchecks.NChecksErlang;

import java.io.IOException;

/**
 * SysmoWorker try to connect to the specified erlang node and loop on
 * any failures (remote node down/unreachable, connection closed,
 * network unreachable)
 */

public class SysmoWorker {

    static boolean active = true;

    public static void main(String[] args) throws Exception
    {
        Logger logger = LoggerFactory.getLogger(SysmoWorker.class);
        logger.info("jworker started");


        // TODO read from a property file
        String selfNodeName    = args[0];
        String foreignNodeName = args[1];
        String erlangCookie    = args[2];
        int weight; // 1 least used 10 most used. 0 is reserved to server proc.
        try {
            weight = Integer.parseInt(args[3]);
        } catch (Exception e) {
            logger.error(e.toString());
            weight = 5;
        }

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
                node = new OtpNode(selfNodeName, erlangCookie);
                // loop and wait for master node to come up
                if (!node.ping(foreignNodeName, 2000)) {
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
                    new NChecksErlang(nchecksMbox, foreignNodeName);

            Thread nchecksThread = new Thread(nchecks);
            nchecksThread.start();
            logger.info("nchecks thread started");

            /*
             * Send an availability information to the "nchecks" process
             */


            OtpErlangObject[] readyObj = new OtpErlangObject[4];
            readyObj[0] = new OtpErlangAtom("worker_available");
            readyObj[1] = mainMbox.self();
            readyObj[2] = nchecksMbox.self();
            readyObj[3] = new OtpErlangInt(weight);
            OtpErlangTuple ackTuple = new OtpErlangTuple(readyObj);
            mainMbox.send("nchecks", foreignNodeName, ackTuple);
            logger.info("availability object sent, waiting for ack");

            try {
                OtpMsg ack = mainMbox.receiveMsg(2000);
                OtpErlangAtom atom = (OtpErlangAtom) ack.getMsg();
                if (!atom.toString().equals("worker_ack"))
                    throw new JworkerInitException();
            } catch (Exception e) {
                logger.error(e.toString());
                node.close();
                mainMbox.exit("normal");
                nchecksThread.join();
                continue;
            }


            while (node.ping(foreignNodeName, 2000)) {
                try {
                    if (!active) throw new JworkerShutdownException();
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    logger.info(e.toString());
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
