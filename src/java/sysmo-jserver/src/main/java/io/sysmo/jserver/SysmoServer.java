package io.sysmo.jserver;

import com.ericsson.otp.erlang.*;
import io.sysmo.nchecks.NChecks;

import java.io.IOException;
import java.nio.file.FileSystems;

public class SysmoServer {

    private static final String selfNodeName = "sysmo-java";

    public static void main(String[] args) {

        /*
         * Extract args
         */
        String foreignNodeName = args[0];
        String erlangCookie    = args[1];
        String workDir         = args[2];

        /*
         * Get log file
         */
        String logFile = FileSystems.getDefault()
                .getPath(workDir, "log", "jserver.log")
                .toString();

        /*
         * Get property file
         */
        String propFile = FileSystems.getDefault()
                .getPath(workDir, "etc", "jserver.properties")
                .toString();

        /*
         * Connect to erlang-main node
         */
        OtpNode node;
        try {
            node = new OtpNode(selfNodeName, erlangCookie);
            if (!node.ping(foreignNodeName, 2000)) {
                System.out.println("can t connect");
                return;
            }
        } catch (IOException e) {
            System.out.println(e);
            // TODO logging slf4j
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

        System.out.println("hello world " + NChecks.toctoc());
    }
}
