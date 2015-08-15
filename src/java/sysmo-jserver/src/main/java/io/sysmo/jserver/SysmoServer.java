package io.sysmo.jserver;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import io.sysmo.nchecks.NChecks;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystems;
import java.util.Properties;

public class SysmoServer {

    private static final String selfNodeName = "sysmo-java";

    public static void main(String[] args) {

        // Extract args
        String foreignNodeName = args[0];
        String erlangCookie    = args[1];
        String workDir         = args[2];

         // Get log file
        String logFile = FileSystems.getDefault()
                .getPath(workDir, "log", "jserver.log")
                .toString();

        // Get property file
        String propFile = FileSystems.getDefault()
                .getPath(workDir, "etc", "sysmo-jserver.properties")
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
            e.printStackTrace();
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

        Logger logger = LoggerFactory.getLogger(SysmoServer.class);
        logger.warn("hello worlod to file??????");
        System.out.println("hello world ");
    }
}
