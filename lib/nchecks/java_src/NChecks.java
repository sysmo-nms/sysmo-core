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
import io.sysmo.nchecks.Argument;
import io.sysmo.nchecks.Reply;
import io.sysmo.nchecks.Query;
import io.sysmo.nchecks.checks.*;
import io.sysmo.nchecks.helpers.*;
import io.sysmo.nchecks.NChecksSNMP;

import com.ericsson.otp.erlang.*;


import java.io.FileInputStream;
import java.io.InputStream;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
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

import java.io.IOException;

public class NChecks
{
    // my node name
    private static String selfNodeName   = null;

    // the foreign node name (-sname)
    private static String foreignNodeName = null;
    private static String erlangCookie = null;

    // the foreign nchecks.erl gen_server pid name
    private static String foreignPidName  = null;

    public static OtpErlangAtom atomReply       = new OtpErlangAtom("reply");
    public static OtpErlangAtom atomOk          = new OtpErlangAtom("ok");
    public static OtpErlangAtom atomQueueFull   = new OtpErlangAtom("queue_full");
    public static OtpErlangAtom atomTimeout     = new OtpErlangAtom("timeout");
    public static OtpErlangAtom atomError       = new OtpErlangAtom("error");

    // erlang server
    private static  OtpNode self = null;
    private static  OtpMbox mbox = null;

    // nchecks vars
    private static  String pingCommand = null;
    private static  ThreadPoolExecutor threadPool = null;
    private static  int threadMaxPoolSize;
    private static  int threadCorePoolSize;
    private static  int threadQueueCapacity;

    // snmp4j
    private static NChecksSNMP snmp;

    public static void main(String[] args)
    {
        // if -test
        if (args.length != 0)
            if (args[0].equals("--test"))
                {testSpace(); return;}
        
        // read config
        try
        {
            Properties   prop  = new Properties();
            InputStream  input = new FileInputStream("cfg/nchecks.properties");
            prop.load(input);
            selfNodeName     = prop.getProperty("self_name");
            foreignNodeName  = prop.getProperty("foreign_node");
            foreignPidName   = prop.getProperty("foreign_pid");
            pingCommand      = prop.getProperty("ping_command");
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
            e.printStackTrace();
            return;
        }

        try
        {
            erlangCookie = new Scanner(
                new File("cfg/sysmo.cookie") ).useDelimiter("\\Z").next();
        }
        catch(IOException e)
        {
            e.printStackTrace();
            return;
        }

        // init snmp4j
        NChecksSNMP.initialize();

        // init thread pool
        threadPool = new ThreadPoolExecutor(
                threadCorePoolSize,
                threadMaxPoolSize,
                10,
                TimeUnit.MINUTES,
                new ArrayBlockingQueue<Runnable>(threadQueueCapacity),
                new NChecksPoolReject());

        CheckICMP.setPping(pingCommand);


        // Initialize otp
        try 
        {
            self = new OtpNode(selfNodeName, erlangCookie);
            mbox = self.createMbox();
            if (!self.ping(foreignNodeName, 2000)) 
            { 
                System.out.println("Connection timed out");
                return;
            }
        }
        catch (IOException e1) 
        {
            e1.printStackTrace();
            return;
        }

        // when it is ok, inform the erl nchecks process
        acknowledgeOtpConnexion();

        // then begin to loop and wait for calls
        OtpErlangObject call = null;
        while (true) try 
        {
            call = mbox.receive();
            handleMsg(call);
        } 
        catch (OtpErlangExit e) 
        {
            threadPool.shutdownNow();
            e.printStackTrace();
            return;
        }
        catch (OtpErlangDecodeException e)
        {
            e.printStackTrace();
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
    
    // UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS 
    // UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS 
    // UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS 
    public static OtpErlangTuple buildOkReply(OtpErlangObject msg)
    {
        OtpErlangObject[] valObj   = new OtpErlangObject[2];
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
            e.printStackTrace();
            return;
        }

        try
        {
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
                OtpErlangList args = (OtpErlangList)
                    (payload.elementAt(1));
                Runnable worker = new NHelperRunnable(
                        Class.forName(className).newInstance(),
                        caller,
                        args);
                threadPool.execute(worker);
            } 
            else if (cmdstr.equals("init"))     handleInit(payload);
            else if (cmdstr.equals("cleanup"))  NChecksSNMP.cleanup();
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
                    e.printStackTrace();
                    uInt = 0; 
                }
                a.set(uInt);
                result.put(key.stringValue(), a);
            }
        }
        return result;
    }

    private static void testSpace()
    {
        // init snmp4j
        NChecksSNMP.initialize();

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
        return;
    } 

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
        Query query      = new Query(NChecks.decodeArgs(args));
        char[] jsonChars = helper.execute(query);
        OtpErlangList   jsonCharList = buildErlangCharList(jsonChars);
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
