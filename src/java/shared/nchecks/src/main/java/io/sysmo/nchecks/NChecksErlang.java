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

import io.sysmo.nchecks.modules.*;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangChar;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.file.FileSystems;
import java.util.HashMap;
import java.util.Map;
import java.util.Iterator;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;


public class NChecksErlang implements Runnable
{
    public static final OtpErlangAtom atomReply = new OtpErlangAtom("reply");
    public static final OtpErlangAtom atomOk = new OtpErlangAtom("ok");
    public static final OtpErlangAtom atomError = new OtpErlangAtom("error");
    public static final OtpErlangAtom atomQueueFull =
            new OtpErlangAtom("queue_full");

    // otp
    private String nodeName;
    private OtpMbox mbox;
    private static final Object lock = new Object();

    // nchecks vars
    private ThreadPoolExecutor threadPool;

    static NChecksErlang instance;
    public Logger logger;

    public NChecksErlang(final OtpMbox mbox, final String nodeName) {
        NChecksErlang.instance = this;
        this.nodeName = nodeName;
        this.mbox = mbox;
        this.logger = LoggerFactory.getLogger(NChecksErlang.class);
    }

    @Override
    public void run()
    {

        String rubyDir = FileSystems
            .getDefault()
            .getPath("ruby")
            .toString();

        String utilsDir = FileSystems
            .getDefault()
            .getPath("utils")
            .toString();

        this.logger.info("ruby dir is: " + rubyDir);

        // if -test
        /*
        if (args.length != 0 && args[0].equals("--test"))
            {testSpace(); return;}
        */

        // init thread pool
        this.threadPool = new ThreadPoolExecutor(
                8,  // base pool size
                20, // max pool size
                60, // return to base after
                TimeUnit.MINUTES,
                new ArrayBlockingQueue<>(2000), // queue capacity
                new NChecksPoolReject());

        // initialize special CheckICMP class
        CheckICMP.setPping(utilsDir);
        this.logger.info("CheckICMP init with path: " + utilsDir);

        // initialize .rb script cache
        NChecksJRuby.startJRuby(rubyDir);
        this.logger.info("JRuby init with path: " + rubyDir);

        // initialize snmpman
        NChecksSNMP.startSnmp();
        this.logger.info("SNMP started");

        // then begin to loop and wait for calls
        this.logger.info("begin too loop");
        OtpErlangObject call;
        while (true) try {
            call = this.mbox.receive();
            this.handleMsg(call);
        } catch (OtpErlangExit |OtpErlangDecodeException e) {
            logger.warn(e.toString());
            threadPool.shutdownNow();
            return;
        }
    }

    /**
     * Send a message to the caller.
     * Used by SnmpmanResponseListener SnmpmanTreeListener and SnmpmanTableListener
     * which are executed in the asynchroneously in another thread.
     */
    public static void sendReply(
            final OtpErlangObject to, final OtpErlangObject msg)
    {
        OtpErlangObject[] obj = new OtpErlangObject[3];
        obj[0] = atomReply;
        obj[1] = to;
        obj[2] = msg;
        OtpErlangTuple tuple = new OtpErlangTuple(obj);
        synchronized(NChecksErlang.lock)
        {
            NChecksErlang.instance.mbox.send(
                    "nchecks", NChecksErlang.instance.nodeName, tuple);
        }
    }

    public static OtpErlangTuple buildOkReply(final OtpErlangObject msg)
    {
        OtpErlangObject[] valObj = new OtpErlangObject[2];
        valObj[0] = atomOk;
        valObj[1] = msg;
        return new OtpErlangTuple(valObj);
    }

    public static OtpErlangTuple buildErrorReply(final OtpErlangObject msg)
    {
        OtpErlangObject[] valObj = new OtpErlangObject[2];
        valObj[0] = atomError;
        valObj[1] = msg;
        return new OtpErlangTuple(valObj);
    }

    public static OtpErlangTuple buildQueueFullReply(final OtpErlangObject msg)
    {
        OtpErlangObject[] valObj = new OtpErlangObject[2];
        valObj[0] = atomQueueFull;
        valObj[1] = msg;
        return new OtpErlangTuple(valObj);
    }

    private void handleInit(OtpErlangTuple initMsg)
    {
        this.logger.info("init?" + initMsg.toString());
    }


    private void handleMsg(final OtpErlangObject msg)
    {
        OtpErlangTuple  tuple;
        OtpErlangAtom   command;
        OtpErlangObject caller;
        OtpErlangTuple  payload;
        try {
            tuple       = (OtpErlangTuple)  msg;
            command     = (OtpErlangAtom)   (tuple.elementAt(0));
            caller      =                   (tuple.elementAt(1));
            payload     = (OtpErlangTuple)  (tuple.elementAt(2));
        } catch (Exception|Error e) {
            this.logger.warn("Fail to decode tuple: " + e.getMessage() + e);
            return;
        }

        try {
            String cmdString = command.toString();

            if (cmdString.equals("check")) {

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
                this.threadPool.execute(worker);

            } else if (cmdString.equals("helper")) {

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
                this.threadPool.execute(worker);

            } else if (cmdString.equals("init")) {
                this.handleInit(payload);

            } else if (cmdString.equals("cleanup")) {
                NChecksSNMP.getInstance().cleanup();

            } else {
                OtpErlangObject reply = buildErrorReply(command);
                NChecksErlang.sendReply(caller, reply);
            }
        }
        catch (Exception|Error e)
        {
            OtpErlangTuple reply = buildErrorReply(
                new OtpErlangString("NChecksErlang error: " + e
                    + " " + command.toString() + " -> " + e.getMessage())
            );
            NChecksErlang.sendReply(caller, reply);
        }
    }

    public static Map<String,Argument> decodeArgs(final OtpErlangList argList)
    {
        Map<String,Argument> result = new HashMap<>();
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

interface CheckCaller
{
    OtpErlangObject getCaller();
}


class NChecksPoolReject implements RejectedExecutionHandler
{
    private static OtpErlangString errMsg =
            new OtpErlangString("NChecksErlang thread queue is full");

    public void rejectedExecution(Runnable r, ThreadPoolExecutor executor)
    {
        CheckCaller ncheckRun = (CheckCaller) r;
        OtpErlangObject caller = ncheckRun.getCaller();
        OtpErlangObject reply = NChecksErlang.buildQueueFullReply(errMsg);
        NChecksErlang.sendReply(caller, reply);
    }
}

class NChecksRunnable implements Runnable, CheckCaller
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
        this.check   = (NChecksInterface) checkObj;
        this.caller  = callerObj;
        this.args    = argsList;
        this.opaqueData = opaque;
    }

    @Override
    public void run()
    {
        Query query = new Query(NChecksErlang.decodeArgs(
                this.args), this.opaqueData.binaryValue());
        Reply reply = this.check.execute(query);
        OtpErlangObject replyMsg = NChecksErlang.buildOkReply(reply.asTuple());
        NChecksErlang.sendReply(this.caller, replyMsg);
    }

    public OtpErlangObject getCaller() {return this.caller;}
}

class NHelperRunnable implements Runnable, CheckCaller
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
        Query           query        = new Query(NChecksErlang.decodeArgs(args));
        NHelperReply    helperReply  = helper.execute(query);
        OtpErlangList   jsonCharList = buildErlangCharList(helperReply.toCharArray());
        OtpErlangObject replyMsg     = NChecksErlang.buildOkReply(jsonCharList);
        NChecksErlang.sendReply(caller, replyMsg);
    }

    public OtpErlangObject getCaller() {return this.caller;}

    private static OtpErlangList buildErlangCharList(char[] charList)
    {
        OtpErlangObject[] objList = new OtpErlangObject[charList.length];
        for (int i = 0; i < charList.length; i++)
        {
            objList[i] = new OtpErlangChar(charList[i]);
        }
        return new OtpErlangList(objList);
    }
}

class NHelperRunnable2 implements Runnable, CheckCaller
{
    private NHelperInterface2 helper;
    private String            helper_id;
    private OtpErlangObject   caller;
    private OtpErlangList     args;

    public NHelperRunnable2(
            final Object helpObj,
            final String id,
            final OtpErlangObject callerObj,
            final OtpErlangList argsList)
    {
        this.helper    = (NHelperInterface2) helpObj;
        this.helper_id = id;
        this.caller    = callerObj;
        this.args      = argsList;
    }

    @Override
    public void run()
    {
        Query query = new Query(NChecksErlang.decodeArgs(this.args));
        NHelperReply helperReply = helper.callHelper(query, this.helper_id);
        OtpErlangList jsonCharList =
                this.buildErlangCharList(helperReply.toCharArray());
        OtpErlangObject replyMsg = NChecksErlang.buildOkReply(jsonCharList);
        NChecksErlang.sendReply(this.caller, replyMsg);
    }

    public OtpErlangObject getCaller() {return this.caller;}

    private OtpErlangList buildErlangCharList(char[] charList)
    {
        OtpErlangObject[] objList = new OtpErlangObject[charList.length];
        for (int i = 0; i < charList.length; i++)
        {
            objList[i] = new OtpErlangChar(charList[i]);
        }
        return new OtpErlangList(objList);
    }
}
