package io.sysmo.jserver;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import io.sysmo.nchecks.NChecks;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.FileSystems;

public class SysmoServer {

    private static final String selfNodeName = "sysmo-java";

    public static void main(String[] args) {

        // init logger
        Logger logger = LoggerFactory.getLogger(SysmoServer.class);

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
         * Create rrd4j thread
         */

        /*
         * Create snmp4j thread
         */

        /*
         * Create nchecks thread
         */

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
    }
}
