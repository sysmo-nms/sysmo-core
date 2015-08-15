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

package io.sysmo.nchecks;
import io.sysmo.nchecks.NChecksInterface;
import io.sysmo.nchecks.NHelperInterface;
import io.sysmo.nchecks.NHelperInterface2;
import io.sysmo.nchecks.NChecksLogger;
import io.sysmo.nchecks.Argument;
import io.sysmo.nchecks.Reply;
import io.sysmo.nchecks.Query;
import io.sysmo.nchecks.modules.*;
import io.sysmo.nchecks.NChecksSNMP;
import io.sysmo.nchecks.NHelperReply;
import io.sysmo.nchecks.NChecksJRuby;

import com.ericsson.otp.erlang.*;


import java.io.FileInputStream;
import java.io.InputStream;
import java.io.File;
import java.nio.file.FileSystems;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.Iterator;
import java.util.Properties;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.logging.Logger;

import java.io.IOException;

public class NChecks
{
    public static final OtpErlangAtom atomReply       = new OtpErlangAtom("reply");
    public static final OtpErlangAtom atomOk          = new OtpErlangAtom("ok");
    public static final OtpErlangAtom atomQueueFull   = new OtpErlangAtom("queue_full");
    public static final OtpErlangAtom atomTimeout     = new OtpErlangAtom("timeout");
    public static final OtpErlangAtom atomError       = new OtpErlangAtom("error");

    // otp
    private static String selfNodeName;
    private static String foreignNodeName;
    private static String erlangCookie;
    private static String foreignPidName;
    private static OtpNode node;
    private static OtpMbox mbox;

    // nchecks vars
    private static  ThreadPoolExecutor threadPool;
    private static  int threadMaxPoolSize;
    private static  int threadCorePoolSize;
    private static  int threadQueueCapacity;

    public static Logger logger;

    public static void main(String[] args)
    {
        String logFile = FileSystems
            .getDefault()
            .getPath(args[0], "log", "nchecks.log")
            .toString();

        String propFile = FileSystems
            .getDefault()
            .getPath(args[0], "etc", "nchecks.properties")
            .toString();

        String rubyDir = FileSystems
            .getDefault()
            .getPath(args[0], "ruby")
            .toString();

        String utilsDir = FileSystems
            .getDefault()
            .getPath(args[0], "utils")
            .toString();


        // init logger
        logger = NChecksLogger.start(logFile);
        logger.info("Logger initialized");

        // if -test
        /*
        if (args.length != 0 && args[0].equals("--test"))
            {testSpace(); return;}
        */

        // read config
        try
        {
            Properties   prop  = new Properties();
            InputStream  input = new FileInputStream(propFile);
            prop.load(input);
            selfNodeName     = prop.getProperty("self_name");
            foreignPidName   = prop.getProperty("foreign_pid");
            erlangCookie     = prop.getProperty("cookie");
            threadMaxPoolSize =
                        Integer.parseInt(
                                    prop.getProperty("thread_pool_max_size"));
            threadCorePoolSize =
                        Integer.parseInt(
                                    prop.getProperty("thread_pool_core_size"));
            threadQueueCapacity =
                        Integer.parseInt(
                                    prop.getProperty("thread_queue_capacity"));
        }
        catch(IOException e)
        {
            logger.severe("Fail to load property file: " + e.getMessage() + e);
            return;
        }

        try
        {
            foreignNodeName  = args[1];
        } catch (Exception e) {
            logger.severe("Fail to read node name (args[1]): "
                                                        + e.getMessage() + e);
            return;
        }

        // init thread pool
        threadPool = new ThreadPoolExecutor(
                threadCorePoolSize,
                threadMaxPoolSize,
                10,
                TimeUnit.MINUTES,
                new ArrayBlockingQueue<Runnable>(threadQueueCapacity),
                new NChecksPoolReject());



        // Initialize otp
        try
        {
            node = new OtpNode(selfNodeName, erlangCookie);
            mbox = node.createMbox();
            if (!node.ping(foreignNodeName, 2000))
            {
                logger.severe("Connection timed out");
                return;
            }
        }
        catch (IOException e)
        {
            logger.severe("Otp Connection failure: " + e.getMessage() + e);
            return;
        }
        logger.info("OTP initialized");

        // when it is ok, inform the erl nchecks process
        acknowledgeOtpConnexion();
        logger.info("OTP send ack");
        // initialize special CheckICMP class
        CheckICMP.setPping(utilsDir);
        logger.info("CheckICMP init with path: " + utilsDir);
        // initialize .rb script cache
        NChecksJRuby.startJRuby(rubyDir);
        logger.info("JRuby init with path: " + rubyDir);
        // initialize snmpman
        NChecksSNMP.startSnmp();
        logger.info("SNMP started");

        // then begin to loop and wait for calls
        OtpErlangObject call = null;
        while (true) try
        {
            call = mbox.receive();
            handleMsg(call);
        }
        catch (OtpErlangExit e)
        {
            logger.severe("Exit: " + e.getMessage() + e);
            threadPool.shutdownNow();
            return;
        }
        catch (OtpErlangDecodeException e)
        {
            logger.severe("Decode Exception: " + e.getMessage() + e);
        }
    }

    private static void acknowledgeOtpConnexion()
    {
        OtpErlangObject[] msg = new OtpErlangObject[2];
        msg[0] = mbox.self();
        msg[1] = new OtpErlangAtom("nchecks_running");
        OtpErlangTuple tuple  = new OtpErlangTuple(msg);

        mbox.send(foreignPidName, foreignNodeName, tuple);
    }

    /**
     * Send a message to the caller.
     * Used by SnmpmanResponseListener SnmpmanTreeListener and SnmpmanTableListener
     * which are executed in the asynchroneously in another thread.
     */
    public static void sendReply(OtpErlangObject to, OtpErlangObject msg)
    {
        OtpErlangObject[] obj = new OtpErlangObject[3];
        obj[0] = atomReply;
        obj[1] = to;
        obj[2] = msg;
        OtpErlangTuple tuple = new OtpErlangTuple(obj);
        synchronized(mbox)
        {
            mbox.send(foreignPidName, foreignNodeName, tuple);
        }
    }

    public static OtpErlangTuple buildOkReply(OtpErlangObject msg)
    {
        OtpErlangObject[] valObj = new OtpErlangObject[2];
        valObj[0] = atomOk;
        valObj[1] = msg;
        OtpErlangTuple valTuple = new OtpErlangTuple(valObj);
        return valTuple;
    }

    public static OtpErlangTuple buildErrorReply(OtpErlangObject msg)
    {
        OtpErlangObject[] valObj   = new OtpErlangObject[2];
        valObj[0] = atomError;
        valObj[1] = msg;
        OtpErlangTuple valTuple = new OtpErlangTuple(valObj);
        return valTuple;
    }

    public static OtpErlangTuple buildQueueFullReply(OtpErlangObject msg)
    {
        OtpErlangObject[] valObj   = new OtpErlangObject[2];
        valObj[0] = atomQueueFull;
        valObj[1] = msg;
        OtpErlangTuple valTuple = new OtpErlangTuple(valObj);
        return valTuple;
    }

    private static void handleInit(OtpErlangTuple initMsg)
    {
    }


    private static void handleMsg(OtpErlangObject msg)
    {
        OtpErlangTuple  tuple;
        OtpErlangAtom   command;
        OtpErlangObject caller;
        OtpErlangTuple  payload;
        try
        {
            tuple       = (OtpErlangTuple)  msg;
            command     = (OtpErlangAtom)   (tuple.elementAt(0));
            caller      = (OtpErlangObject) (tuple.elementAt(1));
            payload     = (OtpErlangTuple)  (tuple.elementAt(2));
        }
        catch (Exception|Error e)
        {
            logger.warning("Fail to decode tuple: " + e.getMessage() + e);
            return;
        }

        try
        {
            System.out.println("hello: " + command.toString());
            String cmdstr = command.toString();
            if (cmdstr.equals("check"))
            {
                OtpErlangString erlangClassName = (OtpErlangString)
                    (payload.elementAt(0));

                // TODO full class name as argument
                String className = erlangClassName.stringValue();

                OtpErlangList checkArgs = (OtpErlangList)
                    (payload.elementAt(1));

                OtpErlangBinary opaque = (OtpErlangBinary)
                    (payload.elementAt(2));

                Runnable worker = new NChecksRunnable(
                        Class.forName(className).newInstance(),
                        caller,
                        checkArgs,
                        opaque);
                threadPool.execute(worker);
            }
            else if (cmdstr.equals("helper"))
            {
                OtpErlangString erlangClassName =
                    (OtpErlangString)
                    (payload.elementAt(0));
                String className = erlangClassName.stringValue();
                OtpErlangString erlangIdName =
                    (OtpErlangString)
                    (payload.elementAt(1));
                String idName = erlangIdName.stringValue();
                OtpErlangList args = (OtpErlangList)
                    (payload.elementAt(2));
                Runnable worker = new NHelperRunnable2(
                        Class.forName(className).newInstance(),
                        idName,
                        caller,
                        args);
                threadPool.execute(worker);
            }
            else if (cmdstr.equals("init"))     handleInit(payload);
            else if (cmdstr.equals("cleanup"))  NChecksSNMP.getInstance().cleanup();
            else
            {
                OtpErlangObject reply = buildErrorReply(command);
                sendReply(caller, reply);
            }
        }
        catch (Exception|Error e)
        {
            OtpErlangTuple reply = buildErrorReply(
                new OtpErlangString("NChecks error: " + e
                    + " " + command.toString() + " -> " + e.getMessage())
            );
            sendReply(caller, reply);
        }
    }

    public static Map<String,Argument> decodeArgs(OtpErlangList argList)
    {
        Map<String,Argument> result = new HashMap<String,Argument>();
        Iterator<OtpErlangObject> itr = argList.iterator();
        OtpErlangTuple element;
        while (itr.hasNext())
        {
            element             = (OtpErlangTuple) (itr.next());
            OtpErlangString key = (OtpErlangString) (element.elementAt(0));
            OtpErlangObject val = element.elementAt(1);
            if (val.getClass() == OtpErlangString.class)
            {
                OtpErlangString valStr = (OtpErlangString) (element.elementAt(1));
                Argument a = new Argument();
                a.set(valStr.stringValue());
                result.put(key.stringValue(), a);
            }
            else if (val.getClass() == OtpErlangLong.class)
            {
                OtpErlangLong valLong = (OtpErlangLong) (element.elementAt(1));
                Argument a = new Argument();
                int uInt;
                try {
                    uInt = valLong.uIntValue();
                } catch (OtpErlangRangeException e) {
                    uInt = 0;
                }
                a.set(uInt);
                result.put(key.stringValue(), a);
            }
        }
        return result;
    }

    /*
    private static void testSpace()
    {

        Map<String,Argument> testArguments = new HashMap<String,Argument>();
        Argument a = new Argument();
        a.set(161);
        testArguments.put("snmp_port", a);
        a = new Argument();
        a.set("3");
        testArguments.put("snmp_version", a);
        a = new Argument();
        a.set("authPriv");
        testArguments.put("snmp_seclevel", a);
        a = new Argument();
        a.set("public");
        testArguments.put("snmp_community", a);
        a = new Argument();
        a.set("jojo");
        testArguments.put("snmp_usm_user", a);
        a = new Argument();
        a.set("password123");
        testArguments.put("snmp_authkey", a);
        a = new Argument();
        a.set("MD5");
        testArguments.put("snmp_authproto", a);
        a = new Argument();
        a.set("enckey123");
        testArguments.put("snmp_privkey", a);
        a = new Argument();
        a.set("DES");
        testArguments.put("snmp_privproto", a);
        a = new Argument();
        a.set(2500);
        testArguments.put("snmp_timeout", a);
        a = new Argument();
        a.set(1);
        testArguments.put("snmp_retries", a);
        a = new Argument();
        a.set("1,2,3");
        testArguments.put("if_selection", a);
        a = new Argument();
        a.set("192.168.0.5");
        testArguments.put("host", a);
        a = new Argument();
        a.set("target-234345");
        testArguments.put("target_id", a);



        NHelperInterface module = new HelperNetworkInterfaces();
        Query query = new Query(testArguments);
        module.execute(query);
    }
    */

}

class NChecksPoolReject implements RejectedExecutionHandler
{
    private static OtpErlangString errMsg = new OtpErlangString("The job was rejected because the NChecks thread queue is full. I am overloaded!");

    public void rejectedExecution(Runnable r, ThreadPoolExecutor executor)
    {
        NChecksRunnable ncheckRun = (NChecksRunnable) r;
        OtpErlangObject caller = ncheckRun.getCaller();
        OtpErlangObject reply = NChecks.buildQueueFullReply(errMsg);
        NChecks.sendReply(caller, reply);
    }
}

class NChecksRunnable implements Runnable
{
    private NChecksInterface check;
    private OtpErlangObject  caller;
    private OtpErlangList    args;
    private OtpErlangBinary  opaqueData;

    public NChecksRunnable(
            Object          checkObj,
            OtpErlangObject callerObj,
            OtpErlangList   argsList,
            OtpErlangBinary opaque)
    {
        check   = (NChecksInterface) checkObj;
        caller  = callerObj;
        args    = argsList;
        opaqueData = opaque;
    }

    @Override
    public void run()
    {
        Query query = new Query(NChecks.decodeArgs(args), opaqueData.binaryValue());
        Reply reply = check.execute(query);
        OtpErlangObject replyMsg = NChecks.buildOkReply(reply.asTuple());
        NChecks.sendReply(caller, replyMsg);
    }

    public OtpErlangObject getCaller() {return this.caller;}
}

class NHelperRunnable implements Runnable
{
    private NHelperInterface helper;
    private OtpErlangObject  caller;
    private OtpErlangList    args;

    public NHelperRunnable(
            Object          helpObj,
            OtpErlangObject callerObj,
            OtpErlangList   argsList)
    {
        helper  = (NHelperInterface) helpObj;
        caller  = callerObj;
        args    = argsList;
    }

    @Override
    public void run()
    {
        Query           query        = new Query(NChecks.decodeArgs(args));
        NHelperReply    helperReply  = helper.execute(query);
        OtpErlangList   jsonCharList = buildErlangCharList(helperReply.toCharArray());
        OtpErlangObject replyMsg     = NChecks.buildOkReply(jsonCharList);
        NChecks.sendReply(caller, replyMsg);
    }

    public OtpErlangObject getCaller() {return this.caller;}

    private static OtpErlangList buildErlangCharList(char[] charList)
    {
        OtpErlangObject[] objList = new OtpErlangObject[charList.length];
        for (int i = 0; i < charList.length; i++)
        {
            objList[i] = new OtpErlangChar(charList[i]);
        }
        OtpErlangList erlangList = new OtpErlangList(objList);
        return erlangList;
    }
}

class NHelperRunnable2 implements Runnable
{
    private NHelperInterface2 helper;
    private String            helper_id;
    private OtpErlangObject   caller;
    private OtpErlangList     args;

    public NHelperRunnable2(
            Object          helpObj,
            String          id,
            OtpErlangObject callerObj,
            OtpErlangList   argsList)
    {
        helper    = (NHelperInterface2) helpObj;
        helper_id = id;
        caller    = callerObj;
        args      = argsList;
    }

    @Override
    public void run()
    {
        Query           query        = new Query(NChecks.decodeArgs(args));
        NHelperReply    helperReply  = helper.callHelper(query, helper_id);
        OtpErlangList   jsonCharList = buildErlangCharList(helperReply.toCharArray());
        OtpErlangObject replyMsg     = NChecks.buildOkReply(jsonCharList);
        NChecks.sendReply(caller, replyMsg);
    }

    public OtpErlangObject getCaller() {return this.caller;}

    private static OtpErlangList buildErlangCharList(char[] charList)
    {
        OtpErlangObject[] objList = new OtpErlangObject[charList.length];
        for (int i = 0; i < charList.length; i++)
        {
            objList[i] = new OtpErlangChar(charList[i]);
        }
        OtpErlangList erlangList = new OtpErlangList(objList);
        return erlangList;
    }
}
