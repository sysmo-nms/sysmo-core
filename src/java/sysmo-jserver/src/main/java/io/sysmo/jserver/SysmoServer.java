package io.sysmo.jserver;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import io.sysmo.nchecks.NChecksErlang;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

public class SysmoServer {

    private static final String selfNodeName = "sysmo-java";

    public static void main(final String[] args) {

        // init logger
        Logger logger = LoggerFactory.getLogger(SysmoServer.class);
        logger.info("Logger started");

        // Extract args
        String foreignNodeName = args[0];
        String erlangCookie    = args[1];

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
            logger.error(e.toString());
            return;
        }

        OtpMbox mainMbox = node.createMbox();

        OtpMbox rrd4jMbox   = node.createMbox();
        OtpMbox snmp4jMbox  = node.createMbox();
        OtpMbox nchecksMbox = node.createMbox();

        /*
         * Link to mainMbox.
         * this way exiting mainMbox will raise an OtpExitException on
         * other mailboxes.
         */
        try {
            mainMbox.link(rrd4jMbox.self());
            mainMbox.link(snmp4jMbox.self());
            mainMbox.link(nchecksMbox.self());
        } catch (Exception e) {
            logger.error("Should not append here!");
        }

        /*
         * Create rrd4j thread
         */
        RrdLogger rrd4j = new RrdLogger(rrd4jMbox, foreignNodeName);
        Thread rrd4jThread = new Thread(rrd4j);
        rrd4jThread.start();

        /*
         * Create snmp4j thread
         */

        /*
         * Create NChecks thread
         */
        NChecksErlang nchecks = new NChecksErlang(nchecksMbox, foreignNodeName);
        Thread nchecksThread = new Thread(nchecks);
        nchecksThread.start();

        /*
         * Send acknowledgement to the "sysmo" erlang process
         */
        OtpErlangObject[] ackObj = new OtpErlangObject[4];
        ackObj[0] = new OtpErlangAtom("java_connected");
        ackObj[1] = rrd4jMbox.self();
        ackObj[2] = snmp4jMbox.self();
        ackObj[3] = nchecksMbox.self();
        OtpErlangTuple ackTuple = new OtpErlangTuple(ackObj);
        mainMbox.send("sysmo", foreignNodeName, ackTuple);

        mainMbox.exit("normal");
        //rrd4jMbox.exit("shutdown");
    }
}