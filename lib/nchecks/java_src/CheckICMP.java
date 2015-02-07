/* Copyright (C) 2014, Sebastien Serre <sserre.bx@gmail.com>
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package io.sysmo.nchecks.modules;

import io.sysmo.nchecks.NChecksInterface;
import io.sysmo.nchecks.Argument;
import io.sysmo.nchecks.Reply;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.InputStream;

import java.util.Map;
import java.net.InetAddress;


public class CheckICMP implements NChecksInterface
{
    private static String STATUS_OK       = "OK";
    private static String STATUS_WARNING  = "WARNING";
    private static String STATUS_CRITICAL = "CRITICAL";
    private static String STATUS_DOWN  = "DOWN";
    private String  host        = "";
    private int     pktsNumber  = 5;
    private int     pktsSize    = 56;
    private int     plWarning   = 50;
    private int     plCritical  = 100;
    private int     msWarning   = 200;
    private int     msCritical  = 2500;
    private int     msTimeout   = 5000;
    private int     msInterval  = 100;
    private String  useIpv6     = "false";
    private String  stdoutReturn;
    private String  stderrReturn;
    private static  String  pping;

    public static void setPping(String com)
    {
        pping = com;
    }

    public CheckICMP()
    {
    }

    public void setConfig(Map<String,Argument> config)
    {
        Argument hostArg     = config.get("host");
        Argument numberArg   = config.get("pkts_number");
        Argument sizeArg     = config.get("pkts_size");
        Argument plWarnArg   = config.get("pl_warning");
        Argument plCritArg   = config.get("pl_critical");
        Argument msWarnArg   = config.get("ms_warning");
        Argument msCritArg   = config.get("ms_critical");
        Argument timeoutArg  = config.get("ms_timeout");
        Argument intervalArg = config.get("ms_interval");
        Argument useV6Arg    = config.get("useIpv6");

        if (hostArg     != null) { this.host        = hostArg.getStr(); }
        if (numberArg   != null) { this.pktsNumber  = numberArg.getInt(); }
        if (sizeArg     != null) { this.pktsSize    = sizeArg.getInt(); }
        if (plWarnArg   != null) { this.plWarning   = plWarnArg.getInt(); }
        if (plCritArg   != null) { this.plCritical  = plCritArg.getInt(); }
        if (msWarnArg   != null) { this.msWarning   = msWarnArg.getInt(); }
        if (msCritArg   != null) { this.msCritical  = msCritArg.getInt(); }
        if (timeoutArg  != null) { this.msTimeout   = timeoutArg.getInt(); }
        if (intervalArg != null) { this.msInterval  = intervalArg.getInt(); }
        if (useV6Arg    != null) { this.useIpv6     = useV6Arg.getStr(); }
    }

    public void setReturnString(String returnString, String returnStream)
    {
        if (returnStream == "stdout")
        {
            this.stdoutReturn = returnString;
        }
        else if (returnStream == "stderr")
        {
            this.stderrReturn = returnString;
        }
    }

    public Reply execute()
    {

        Reply reply = new Reply();

        if ("".equals(host)) {
            reply.setStatus(STATUS_DOWN);
            reply.setReply("Host must be specified");
            return reply;
        }

        InetAddress addr;
        try {
            addr = InetAddress.getByName(host);
            this.host = addr.getHostAddress();
        } catch (Exception e) {
            reply.setStatus(STATUS_DOWN);
            reply.setReply("Host lookup fail for: " + host);
            return reply;
        }

        String[] cmd;
        String[] args = new String[2];
        args[0] = pping;
        args[1] =
            "--host="       + this.host         +
            " --interval="  + this.msInterval   +
            " --ipv6="      + this.useIpv6      +
            " --number="    + this.pktsNumber   +
            " --size="      + this.pktsSize     +
            " --timeout="   + this.msTimeout;

        int returnStatus;
        Process proc;
        try {
            String osName = System.getProperty("os.name");

            if (osName.startsWith("Windows"))
            {
                cmd = new String[args.length + 2];
                if (osName.equals("Windows 95")) {
                    cmd[0] = "command.com";
                } else {
                    cmd[0] = "cmd.exe";
                }
                cmd[1] = "/C";
                System.arraycopy(args, 0, cmd, 2, args.length);
            }
            else if (osName.equals("Linux"))
            {
                cmd = new String[3];
                cmd[0] = "/bin/sh";
                cmd[1] = "-c";
                cmd[2] = args[0] + " " + args[1];
            }
            else
            {
                cmd = args;
            }

            Runtime runtime = Runtime.getRuntime();
            proc = runtime.exec(cmd);
            StreamConsumer stdoutConsumer = new StreamConsumer(proc.getInputStream(), "stdout", this);
            StreamConsumer stderrConsumer = new StreamConsumer(proc.getErrorStream(), "stderr", this);
            stdoutConsumer.start();
            stderrConsumer.start();

            returnStatus = proc.waitFor();
            stdoutConsumer.join();
            stderrConsumer.join();

        } catch (Exception e) {
            reply.setStatus(STATUS_DOWN);
            reply.setReply(e.getMessage());
            return reply;
        }

        if (returnStatus != 0) {
            reply.setStatus(STATUS_DOWN);
            reply.setReply(stderrReturn);
            return reply;
        }

        String[] ppingReturn = stdoutReturn.split(",");
        if (! "<PPING_RETURN>".equals(ppingReturn[0])) {
            reply.setStatus(STATUS_DOWN);
            reply.setReply(stderrReturn);
            return reply;
        } 
        long percentLoss  = Long.valueOf(ppingReturn[1]).longValue();
        long minReplyTime = Long.valueOf(ppingReturn[2]).longValue();
        long avgReplyTime = Long.valueOf(ppingReturn[3]).longValue();
        long maxReplyTime = Long.valueOf(ppingReturn[4]).longValue();
        reply.putPerformance("percent_loss",          percentLoss);
        reply.putPerformance("ms_min_reply_duration", minReplyTime);
        reply.putPerformance("ms_avg_reply_duration", avgReplyTime);
        reply.putPerformance("ms_max_reply_duration", maxReplyTime);
        reply.setReply(stdoutReturn);

        if (percentLoss == 100) {
            reply.setStatus(STATUS_DOWN);
        }else if (percentLoss >= plCritical || avgReplyTime >= msCritical) {
            reply.setStatus(STATUS_CRITICAL);
        } else if (percentLoss >= plWarning || avgReplyTime >= msWarning) {
            reply.setStatus(STATUS_WARNING);
        } else {
            reply.setStatus(STATUS_OK);
        }
        return reply;
    }
}

class StreamConsumer extends Thread {
    private InputStream     is;
    private String          type;
    private StringBuffer    replyString;
    private CheckICMP       parent;

    public StreamConsumer(
            InputStream inputStream,
            String      type,
            CheckICMP   parent)
    {
        this.is     = inputStream;
        this.type   = type;
        this.replyString = new StringBuffer();
        this.parent = parent;
    }

    @Override
    public void run() {
        BufferedReader buff = null;
        try {
            buff = new BufferedReader(new InputStreamReader(is));
            String line;

            while ((line = buff.readLine()) != null) {
                replyString.append(line);
            }
            this.parent.setReturnString(replyString.toString(), type);
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if(buff != null) {
                try { buff.close(); } catch(Exception ignore) {}
            }
        }
    }
}
