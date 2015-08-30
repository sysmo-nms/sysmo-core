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

package io.sysmo.jserver;

import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.file.FileSystems;
import java.util.Iterator;
import java.util.Properties;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpMbox;


import org.rrd4j.core.RrdDef;
import org.rrd4j.core.ArcDef;
import org.rrd4j.core.RrdDb;
import org.rrd4j.core.Sample;
import org.rrd4j.core.RrdDbPool;
import org.rrd4j.DsType;
import org.rrd4j.ConsolFun;

// TODO move logic to RrdRunnable class and make RrdLogger dead simple

public class RrdLogger implements Runnable
{
    private static RrdLogger instance;

    public static final OtpErlangAtom atomReply = new OtpErlangAtom("reply");
    public static final OtpErlangAtom atomOk    = new OtpErlangAtom("ok");
    public static final OtpErlangAtom atomError = new OtpErlangAtom("error");
    public static final OtpErlangAtom atomBusy  = new OtpErlangAtom("server_busy");

    // lock is used to synchronize access to the object
    private OtpMbox mbox;
    private String nodeName;
    private final Object lock = new Object();

    // rra definitions
    private ArcDef[] rraDefault;
    private ArcDef[] rraPrecise;

    // threads
    private RrdDbPool rrdDbPool;
    private ThreadPoolExecutor threadPool;

    // logging
    private Logger logger;

    public RrdLogger(final OtpMbox mbox, final String nodeName) {
        RrdLogger.instance = this;
        this.nodeName = nodeName;
        this.mbox = mbox;
        this.logger = LoggerFactory.getLogger(RrdLogger.class);

        String propFile = FileSystems
                .getDefault()
                .getPath("etc", "sysmo-rrd.properties")
                .toString();

        try {
            Properties prop = new Properties();
            InputStream input = new FileInputStream(propFile);
            prop.load(input);
            this.rraDefault = decodeRRADef(prop.getProperty("rra_default"));
            this.rraPrecise = decodeRRADef(prop.getProperty("rra_precise"));
        } catch (Exception | Error e) {
            this.logger.error(
                    "Fail to load property file: " + e.getMessage(), e);
            return;
        }

        // set up the threads
        this.threadPool = new ThreadPoolExecutor(
                8,  // initial pool size
                20, // max pool size
                60, // wait 60 minutes before going to initial
                TimeUnit.MINUTES,
                new ArrayBlockingQueue<Runnable>(2000), // queue capacity
                new RrdReject()
        );
        this.rrdDbPool = RrdDbPool.getInstance();
    }

    @Override
    public void run() {
        // begin to loop and wait for calls
        OtpErlangObject call;
        while (true) try {
            call = this.mbox.receive();
            this.threadPool.execute(new RrdRunnable(call));
        } catch (OtpErlangExit|OtpErlangDecodeException e) {
            this.logger.error(e.getMessage(), e);
            this.threadPool.shutdown();
            this.mbox.exit("crash");
            break;
        }
    }

    public static void sendReply(
            final OtpErlangObject to, final OtpErlangObject msg)
    {
        OtpErlangObject[] obj = new OtpErlangObject[3];
        obj[0] = atomReply;
        obj[1] = to;
        obj[2] = msg;
        OtpErlangTuple tuple = new OtpErlangTuple(obj);
        synchronized(RrdLogger.instance.lock)
        {
            RrdLogger.instance.mbox.send(
                    "errd4j", RrdLogger.instance.nodeName, tuple);
        }
    }

    public static OtpErlangTuple buildErrorReply(final OtpErlangObject msg)
    {
        OtpErlangObject[] valObj   = new OtpErlangObject[2];
        valObj[0] = atomError;
        valObj[1] = msg;
        return new OtpErlangTuple(valObj);
    }

   /*
    * Handle updates
    */
    public static void handleRrdMultiUpdate(
            final OtpErlangObject caller,
            final OtpErlangTuple tuple) throws Exception
    {
        OtpErlangList updates = (OtpErlangList) (tuple.elementAt(0));
        Iterator<OtpErlangObject> updatesIt = updates.iterator();
        while (updatesIt.hasNext())
        {
            OtpErlangTuple updateTuple = (OtpErlangTuple) updatesIt.next();
            RrdLogger.rrdUpdate(updateTuple);
        }
        RrdLogger.sendReply(caller, atomOk);
    }

    public static void handleRrdUpdate(
            final OtpErlangObject caller,
            final OtpErlangTuple tuple) throws Exception
    {
        RrdLogger.rrdUpdate(tuple);
        RrdLogger.sendReply(caller, atomOk);
    }

    private static void rrdUpdate(
            final OtpErlangTuple tuple) throws Exception
    {
        RrdLogger rrdObj = RrdLogger.instance;
        OtpErlangString filePath  = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangList   updates   = (OtpErlangList)   (tuple.elementAt(1));
        OtpErlangLong   timestamp = (OtpErlangLong) (tuple.elementAt(2));

        RrdDb rrdDb = rrdObj.rrdDbPool.requestRrdDb(filePath.stringValue());

        try {
            Sample sample = rrdDb.createSample();
            sample.setTime(timestamp.longValue());
            Iterator<OtpErlangObject> updatesIt = updates.iterator();
            while (updatesIt.hasNext())
            {
                OtpErlangTuple  up      = (OtpErlangTuple)  updatesIt.next();
                OtpErlangString name    = (OtpErlangString) (up.elementAt(0));
                OtpErlangLong   value   = (OtpErlangLong)   (up.elementAt(1));
                sample.setValue(name.stringValue(), value.longValue());
            }
            sample.update();
        } catch (Exception e) {
            rrdObj.logger.warn("Fail to update rrd: " + e.getMessage(), e);
            rrdObj.rrdDbPool.release(rrdDb);
            throw e;
        }
        rrdObj.rrdDbPool.release(rrdDb);
    }
 

    /*
     * Handle create a rrd file.
     */
    public static void handleRrdMultiCreate(
            final OtpErlangObject caller,
            final OtpErlangTuple tuple) throws Exception
    {
        OtpErlangList creates = (OtpErlangList) (tuple.elementAt(0));
        Iterator<OtpErlangObject> createsIt = creates.iterator();
        while (createsIt.hasNext())
        {
            OtpErlangTuple createTuple = (OtpErlangTuple) createsIt.next();
            RrdLogger.rrdCreate(createTuple);
        }
        RrdLogger.sendReply(caller, atomOk);
    }

    public static void handleRrdCreate(
            final OtpErlangObject caller,
            final OtpErlangTuple tuple) throws Exception
    {
        RrdLogger.rrdCreate(tuple);
        RrdLogger.sendReply(caller, atomOk);
    }

    private static void rrdCreate(final OtpErlangTuple tuple) throws Exception
    {
        OtpErlangString filePath = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangLong   step     = (OtpErlangLong)   (tuple.elementAt(1));
        OtpErlangList   dss      = (OtpErlangList)   (tuple.elementAt(2));
        OtpErlangString rraType  = (OtpErlangString) (tuple.elementAt(3));

        RrdDef rrdDef = new RrdDef(filePath.stringValue(), step.uIntValue());

        if (rraType.stringValue().equals("precise")) {
            rrdDef.addArchive(RrdLogger.instance.rraPrecise);
        } else {
            rrdDef.addArchive(RrdLogger.instance.rraDefault);
        }

        Iterator<OtpErlangObject> dssIt  = dss.iterator();
        while (dssIt.hasNext())
        {
            OtpErlangTuple  ds      = (OtpErlangTuple) dssIt.next();
            OtpErlangString dsName  = (OtpErlangString) (ds.elementAt(0));
            OtpErlangString dsTypeStr   = (OtpErlangString)   (ds.elementAt(1));
            OtpErlangLong   dsHbeat = (OtpErlangLong)   (ds.elementAt(2));
            DsType dsType = DsType.valueOf(dsTypeStr.stringValue());

            Double dsMinVal;
            Double dsMaxVal;
            try {
                OtpErlangDouble dsMin = (OtpErlangDouble) (ds.elementAt(3));
                dsMinVal = dsMin.doubleValue();
            } catch (Exception|Error e) {
                dsMinVal = Double.NaN;
            }
            try {
                OtpErlangDouble dsMax = (OtpErlangDouble) (ds.elementAt(4));
                dsMaxVal = dsMax.doubleValue();
            } catch (Exception|Error e) {
                dsMaxVal = Double.NaN;
            }

            rrdDef.addDatasource(
                dsName.stringValue(),
                dsType,
                dsHbeat.longValue(),
                dsMinVal,
                dsMaxVal
            );
        }

        RrdDb rrdDb = RrdLogger.instance.rrdDbPool.requestRrdDb(rrdDef);
        RrdLogger.instance.rrdDbPool.release(rrdDb);
    }



    private static ArcDef[] decodeRRADef(final String rraDef) throws Exception
    {
        String[] defs       = rraDef.split(",");
        ArcDef[] archiveDef = new ArcDef[defs.length];
        for (int i = 0; i < defs.length; i++) {
            String def = defs[i];
            String[] defElements = def.split(":");
            ConsolFun   cf    = ConsolFun.valueOf(defElements[1]);
            double      xff   = Double.parseDouble(defElements[2]);
            int         steps = Integer.parseInt(defElements[3]);
            int         rows  = Integer.parseInt(defElements[4]);
            ArcDef archive = new ArcDef(cf,xff,steps,rows);
            archiveDef[i] = archive;
        }
        return archiveDef;
    }
}

class RrdRunnable implements Runnable
{
    private OtpErlangObject caller;
    private OtpErlangObject msg;
    private static Logger logger = LoggerFactory.getLogger(RrdRunnable.class);

    public RrdRunnable(final OtpErlangObject message)
    {
        this.msg = message;
    }

    @Override
    public void run()
    {
        OtpErlangTuple  tuple;
        OtpErlangAtom   command;
        OtpErlangTuple  payload;
        try
        {
            tuple       = (OtpErlangTuple)  this.msg;
            command     = (OtpErlangAtom)   (tuple.elementAt(0));
            this.caller =                   (tuple.elementAt(1));
            payload     = (OtpErlangTuple)  (tuple.elementAt(2));
        }
        catch (Exception|Error e)
        {
            RrdRunnable.logger.warn(
                    "RrdRunnable fail to decode tuple: " + e.getMessage(), e);
            return;
        }

        try
        {
            switch (command.toString())
            {
                case "multi_update":
                    RrdLogger.handleRrdMultiUpdate(caller, payload);
                    break;
                case "update":
                    RrdLogger.handleRrdUpdate(caller, payload);
                    break;
                case "multi_create":
                    RrdLogger.handleRrdMultiCreate(caller, payload);
                    break;
                case "create":
                    RrdLogger.handleRrdCreate(caller, payload);
                    break;
                default:
                    RrdRunnable.logger.warn("unknown command: " + command);
                    OtpErlangTuple reply = RrdLogger.buildErrorReply(
                                            new OtpErlangString("undefined"));
                    RrdLogger.sendReply(caller, reply);
            }
        }
        catch (Exception|Error e)
        {
            RrdRunnable.logger.warn(
                    "RrdRunnable failure: " + e.getMessage(), e);
            OtpErlangTuple reply = RrdLogger.buildErrorReply(
                    new OtpErlangString("Java CATCH: Failed to honour command "
                            + command.toString() + " -> " + e
                            + " " + e.getMessage())
            );

            RrdLogger.sendReply(caller, reply);
        }
    }

    public OtpErlangObject getCaller()
    {
        return this.caller;
    }
}


class RrdReject implements RejectedExecutionHandler
{
    public void rejectedExecution(final Runnable r, final ThreadPoolExecutor e)
    {
        RrdRunnable failRunner = (RrdRunnable) r;
        RrdLogger.sendReply(failRunner.getCaller(), RrdLogger.atomBusy);
    }
}
