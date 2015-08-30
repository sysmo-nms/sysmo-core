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
    static Logger logger = LoggerFactory.getLogger(NChecksErlang.class);

    public NChecksErlang(
            final OtpMbox mbox, final String nodeName,
            final String rubyDir, final String utilsDir,
            final String varDir) throws Exception {
        NChecksErlang.instance = this;
        this.nodeName = nodeName;
        this.mbox = mbox;

        NChecksErlang.logger.info("ruby dir is: " + rubyDir);

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
                new ArrayBlockingQueue<Runnable>(2000), // queue capacity
                new NChecksPoolReject());

        // initialize special CheckICMP class
        CheckICMP.setPping(utilsDir);
        NChecksErlang.logger.info("CheckICMP init with path: " + utilsDir);

        // initialize .rb script cache
        NChecksJRuby.startJRuby(rubyDir);
        NChecksErlang.logger.info("JRuby init with path: " + rubyDir);

        // initialize snmpman
        NChecksSNMP.startSnmp(varDir);
        NChecksErlang.logger.info("SNMP started");
    }

    @Override
    public void run()
    {
        // loop and wait for calls
        NChecksErlang.logger.info("begin too loop");
        OtpErlangObject call;
        while (true) try {
            call = this.mbox.receive();
            this.handleMsg(call);
        } catch (Exception e) {
            NChecksErlang.logger.warn(e.getMessage(), e);
            this.threadPool.shutdownNow();
            this.mbox.exit("crach");
            break;
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
        NChecksErlang.logger.info("init?" + initMsg.toString());
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
            NChecksErlang.logger.warn(
                    "Fail to decode tuple: " + e.getMessage(), e);
            return;
        }

        try {
            String cmdString = command.toString();

            switch (cmdString)
            {
                case "check":
                    OtpErlangString erlangCheckClassName = (OtpErlangString)
                            (payload.elementAt(0));

                    // TODO full class name as argument
                    String checkClassName = erlangCheckClassName.stringValue();

                    OtpErlangList checkArgs = (OtpErlangList)
                            (payload.elementAt(1));

                    OtpErlangBinary opaque = (OtpErlangBinary)
                            (payload.elementAt(2));

                    Runnable checkWorker = new NChecksRunnable(
                            Class.forName(checkClassName).newInstance(),
                            caller,
                            checkArgs,
                            opaque);
                    this.threadPool.execute(checkWorker);
                    break;

                case "helper":
                    OtpErlangString erlangHelperClassName =
                            (OtpErlangString)
                                    (payload.elementAt(0));
                    String helperClassName = erlangHelperClassName.stringValue();
                    OtpErlangString erlangIdName =
                            (OtpErlangString)
                                    (payload.elementAt(1));
                    String idName = erlangIdName.stringValue();
                    OtpErlangList args = (OtpErlangList)
                            (payload.elementAt(2));
                    Runnable helperWorker = new NHelperRunnable(
                            Class.forName(helperClassName).newInstance(),
                            idName,
                            caller,
                            args);
                    this.threadPool.execute(helperWorker);
                    break;

                case "init":
                    this.handleInit(payload);
                    break;

                case "cleanup":
                    NChecksSNMP.getInstance().cleanup();
                    break;

                default:
                    OtpErlangObject reply = buildErrorReply(command);
                    NChecksErlang.sendReply(caller, reply);
            }
        }
        catch (Exception|Error e)
        {
            NChecksErlang.logger.error(e.getMessage(), e);
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
    private String            helper_id;
    private OtpErlangObject   caller;
    private OtpErlangList     args;

    public NHelperRunnable(
            final Object helpObj,
            final String id,
            final OtpErlangObject callerObj,
            final OtpErlangList argsList)
    {
        this.helper    = (NHelperInterface) helpObj;
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
