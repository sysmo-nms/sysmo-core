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

package io.sysmo.errd4j;


import java.io.*;
import java.util.*;
import java.nio.file.*;

import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;

import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangByte;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;


import org.rrd4j.core.RrdDef;
import org.rrd4j.core.ArcDef;
import org.rrd4j.core.RrdDb;
import org.rrd4j.core.Sample;
import org.rrd4j.core.FetchData;
import org.rrd4j.core.FetchRequest;
import org.rrd4j.core.RrdDbPool;
import org.rrd4j.graph.RrdGraphDef;
import org.rrd4j.graph.RrdGraph;
import org.rrd4j.DsType;
import org.rrd4j.ConsolFun;
import java.awt.image.BufferedImage;
import java.awt.Color;

public class Errd4j
{

    public static final OtpErlangAtom atomReply       = new OtpErlangAtom("reply");
    public static final OtpErlangAtom atomOk          = new OtpErlangAtom("ok");
    public static final OtpErlangAtom atomError       = new OtpErlangAtom("error");
    public static final OtpErlangAtom atomBusy        = new OtpErlangAtom("server_busy");

    // otp
    private static String selfNodeName;
    private static String foreignNodeName;
    private static String erlangCookie;
    private static String foreignPidName;
    private static  OtpNode self;
    private static  OtpMbox mbox;

    // rra definitions
    private static ArcDef[] rraDefault;
    private static ArcDef[] rraPrecise;

    // threads
    private static RrdDbPool rrdDbPool;
    private static ThreadPoolExecutor threadPool;
    private static int threadMaxPoolSize    = 20;
    private static int threadCorePoolSize   = 8;
    private static int threadQueueCapacity  = 2000;

    public static void main(String[] args)
    {
        try
        {
            Properties   prop  = new Properties();
            InputStream  input = new FileInputStream("cfg/errd4j.properties");
            prop.load(input);
            selfNodeName     = prop.getProperty("self_name");
            foreignNodeName  = prop.getProperty("foreign_node");
            foreignPidName   = prop.getProperty("foreign_pid");
            rraDefault       = decodeRRADef(prop.getProperty("rra_default"));
            rraPrecise       = decodeRRADef(prop.getProperty("rra_precise"));
        }
        catch(Exception|Error e)
        {
            e.printStackTrace();
            return;
        }
        System.out.println("foreign node is " + foreignNodeName);

        try
        {
            erlangCookie = new Scanner(new File("cfg/sysmo.cookie"), "UTF-8").useDelimiter("\\Z").next();
        }
        catch(IOException e)
        {
            e.printStackTrace();
            return;
        }

        // Initialize
        try 
        {
            System.out.println("Trying to connect to " + foreignNodeName);
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

        // when it is ok, inform the erl errd4j process
        acknowledgeOtpConnexion();

        // set up the threads
        threadPool = new ThreadPoolExecutor(
            threadCorePoolSize,
            threadMaxPoolSize,
            60,
            TimeUnit.MINUTES,
            new ArrayBlockingQueue<Runnable>(threadQueueCapacity),
            new RrdReject()
        );
        rrdDbPool = RrdDbPool.getInstance();    

        // then begin to loop and wait for calls
        OtpErlangObject call = null;
        RrdRunnable worker;
        while (true) try 
        {
            call = mbox.receive();
            threadPool.execute(new RrdRunnable(call));
        } 
        catch (OtpErlangExit e) 
        {
            e.printStackTrace();
            threadPool.shutdown();
            break;
        }
        catch (OtpErlangDecodeException e)
        {
            e.printStackTrace();
            threadPool.shutdown();
            break;
        }
        System.exit(0);
    }

    private static void acknowledgeOtpConnexion()
    {
        OtpErlangObject[] msg = new OtpErlangObject[2];
        msg[0] = mbox.self();
        msg[1] = new OtpErlangAtom("errd4j_running");

        OtpErlangTuple tuple  = new OtpErlangTuple(msg);

        synchronized(mbox)
        {
            mbox.send(foreignPidName, foreignNodeName, tuple);
        }

    }

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

   /*
    * Handle updates
    */
    public static void handleRrdMultiUpdate(OtpErlangObject caller, OtpErlangTuple tuple) throws Exception
    {
        OtpErlangList updates = (OtpErlangList) (tuple.elementAt(0));
        Iterator<OtpErlangObject> updatesIt = updates.iterator();
        while (updatesIt.hasNext())
        {
            OtpErlangTuple utuple = (OtpErlangTuple) updatesIt.next();
            rrdUpdate(utuple);
        }
        sendReply(caller, atomOk);
    }

    public static void handleRrdUpdate(OtpErlangObject caller, OtpErlangTuple tuple) throws Exception
    {
        rrdUpdate(tuple);
        sendReply(caller, atomOk);
    }

    private static void rrdUpdate(OtpErlangTuple tuple) throws Exception
    {
        OtpErlangString filePath  = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangList   updates   = (OtpErlangList)   (tuple.elementAt(1));
        OtpErlangLong   timestamp = (OtpErlangLong) (tuple.elementAt(2));

        RrdDb rrdDb = rrdDbPool.requestRrdDb(filePath.stringValue());

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
            e.printStackTrace();
            rrdDbPool.release(rrdDb);
            throw e;  
        }
        rrdDbPool.release(rrdDb);
    }
    

    /*
     * Handle create a rrd file.
     */
    public static void handleRrdMultiCreate(OtpErlangObject caller, OtpErlangTuple tuple) throws Exception
    {
        OtpErlangList creates = (OtpErlangList) (tuple.elementAt(0));
        Iterator<OtpErlangObject> createsIt = creates.iterator();
        while (createsIt.hasNext())
        {
            OtpErlangTuple ctuple = (OtpErlangTuple) createsIt.next();
            rrdCreate(ctuple);
        }
        sendReply(caller, atomOk);
    }

    public static void handleRrdCreate(OtpErlangObject caller, OtpErlangTuple tuple) throws Exception
    {
        rrdCreate(tuple);
        sendReply(caller, atomOk);
    } 

    private static void rrdCreate(OtpErlangTuple tuple) throws Exception
    {
        OtpErlangString filePath = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangLong   step     = (OtpErlangLong)   (tuple.elementAt(1));
        OtpErlangList   dss      = (OtpErlangList)   (tuple.elementAt(2));
        OtpErlangString rraType  = (OtpErlangString) (tuple.elementAt(3));

        RrdDef rrdDef = new RrdDef(filePath.stringValue(), step.uIntValue());

        if (rraType.stringValue().equals("precise")) {
            rrdDef.addArchive(rraPrecise);
        } else {
            rrdDef.addArchive(rraDefault);
        }

        Iterator<OtpErlangObject> dssIt  = dss.iterator();
        while (dssIt.hasNext())
        {
            OtpErlangTuple  ds      = (OtpErlangTuple) dssIt.next();
            OtpErlangString dsName  = (OtpErlangString) (ds.elementAt(0));
            OtpErlangString dsTypeStr   = (OtpErlangString)   (ds.elementAt(1));
            OtpErlangLong   dsHbeat = (OtpErlangLong)   (ds.elementAt(2));
            DsType dsType = DsType.valueOf(dsTypeStr.stringValue());

            Double dsMinVal = null;
            Double dsMaxVal = null;
            try {
                OtpErlangDouble dsMin   = (OtpErlangDouble)   (ds.elementAt(3));
                dsMinVal = dsMin.doubleValue();
            } catch (Exception|Error e) {
                dsMinVal = Double.NaN;
            }
            try {
                OtpErlangDouble  dsMax   = (OtpErlangDouble)   (ds.elementAt(4));
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

        RrdDb rrdDb = rrdDbPool.requestRrdDb(rrdDef);
        rrdDbPool.release(rrdDb);
    }



    private static ArcDef[] decodeRRADef(String rraDef) throws Exception
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

    public RrdRunnable(OtpErlangObject message)
    {
        msg = message;
    }

    @Override
    public void run()
    {
        OtpErlangTuple  tuple;
        OtpErlangAtom   command;
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
            switch (command.toString())
            {
                case "multi_update":
                    Errd4j.handleRrdMultiUpdate(caller, payload);
                    break;
                case "update":
                    Errd4j.handleRrdUpdate(caller, payload);
                    break;
                case "multi_create":
                    Errd4j.handleRrdMultiCreate(caller, payload);
                    break;
                case "create":
                    Errd4j.handleRrdCreate(caller, payload);
                    break;
                default:
                    OtpErlangTuple dreply = Errd4j.buildErrorReply(new OtpErlangString("undefined"));
                    Errd4j.sendReply(caller, dreply);
            }
        }
        catch (Exception|Error e)
        {
            OtpErlangTuple reply = Errd4j.buildErrorReply(
                new OtpErlangString("Java CATCH: Failed to honour command "
                    + command.toString() + " -> " + e + " " + e.getMessage())
            );

            e.printStackTrace();
            Errd4j.sendReply(caller, reply);
        }
    }

    public OtpErlangObject getCaller()
    {
        return caller;
    }
}


class RrdReject implements RejectedExecutionHandler
{
    public void rejectedExecution(Runnable r, ThreadPoolExecutor executor)
    {
        RrdRunnable failRunner = (RrdRunnable) r;
        Errd4j.sendReply(failRunner.getCaller(), Errd4j.atomBusy);
    }
}
