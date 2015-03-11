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
import org.rrd4j.core.RrdDb;
import org.rrd4j.core.Sample;
import org.rrd4j.graph.RrdGraphDef;
import org.rrd4j.graph.RrdGraph;
import org.rrd4j.DsType;
import org.rrd4j.ConsolFun;
import java.awt.image.BufferedImage;
import java.awt.Color;

public class Errd4j
{
    // my node name
    private static String selfNodeName   = null;

    // the foreign node name (-sname)
    private static String foreignNodeName = null;
    private static String erlangCookie = null;

    // the foreign errd4j.erl gen_server pid name
    private static String foreignPidName  = null;

    public static OtpErlangAtom atomReply       = new OtpErlangAtom("reply");
    public static OtpErlangAtom atomOk          = new OtpErlangAtom("ok");
    public static OtpErlangAtom atomError       = new OtpErlangAtom("error");
    // erlang server
    private static  OtpNode self = null;
    private static  OtpMbox mbox = null;

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
        }
        catch(IOException e)
        {
            e.printStackTrace();
            return;
        }
        System.out.println("foreign node is " + foreignNodeName);

        try
        {
            erlangCookie = new Scanner(new File("cfg/sysmo.cookie")).useDelimiter("\\Z").next();
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

        // then begin to loop and wait for calls
        OtpErlangObject call = null;
        while (true) try 
        {
            call = mbox.receive();
            handleMsg(call);
        } 
        catch (OtpErlangExit e) 
        {
            e.printStackTrace();
            break;
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

    private static void handleMsg(OtpErlangObject msg)
    {
        OtpErlangTuple  tuple;
        OtpErlangAtom   command;
        OtpErlangObject caller;
        OtpErlangTuple  payload;
        try
        {
            tuple       = (OtpErlangTuple) msg;
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
                case "create":
                    handleRrdCreate(caller, payload);
                    break;
                case "update":
                    handleRrdUpdate(caller, payload);
                    break;

                case "graph":
                    handleRrdGraph(caller, payload);
                    break;
                case "test":
                    handleRrdTest(caller, payload);
                    break;
                default:
                    OtpErlangTuple dreply = buildOkReply(new OtpErlangString("undefined"));
                    sendReply(caller, dreply);
            }
        }
        catch (Exception|Error e)
        {
            OtpErlangTuple reply = buildErrorReply(
                new OtpErlangString("Java CATCH: Failed to honour command "
                    + command.toString() + " -> " + e.getMessage())
            );

            sendReply(caller, reply);
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

    

    /*
    * Testing create, update, and graph from rrd4j tutorial.
    */
    private static void handleRrdTest(OtpErlangObject caller, OtpErlangTuple tuple) throws Exception
    {
        // create
        RrdDef rrdDef = new RrdDef("test.rrd");
        rrdDef.addDatasource("speed", DsType.COUNTER, 600, Double.NaN, Double.NaN);
        rrdDef.addArchive(ConsolFun.AVERAGE, 0.5, 1, 24);
        rrdDef.addArchive(ConsolFun.AVERAGE, 0.5, 6, 10);
        RrdDb rrdDb = new RrdDb(rrdDef);
        rrdDb.close();

        // update
        RrdDb rrdDbu = new RrdDb("./test.rrd");
        Sample sample = rrdDb.createSample();
        sample.setAndUpdate("920804700:12345");
        sample.setAndUpdate("920805000:12357");
        sample.setAndUpdate("920805300:12363");
        sample.setAndUpdate("920805600:12363");
        sample.setAndUpdate("920805900:12363");
        sample.setAndUpdate("920806200:12373");
        sample.setAndUpdate("920806500:12383");
        sample.setAndUpdate("920806800:12393");
        sample.setAndUpdate("920807100:12399");
        sample.setAndUpdate("920807400:12405");
        sample.setAndUpdate("920807700:12411");
        sample.setAndUpdate("920808000:12415");
        sample.setAndUpdate("920808300:12420");
        sample.setAndUpdate("920808600:12422");
        sample.setAndUpdate("920808900:12423");
        rrdDbu.close();

        // graph
        RrdGraphDef graphDef = new RrdGraphDef();
        graphDef.setTimeSpan(920804400L, 920808000L);
        graphDef.datasource("myspeed", "./test.rrd", "speed", ConsolFun.AVERAGE);
        graphDef.line("myspeed", new Color(0xFF, 0, 0), null, 2);
        graphDef.setFilename("./speed.ig");
        RrdGraph graph = new RrdGraph(graphDef);
        BufferedImage bi = new BufferedImage(100,100,BufferedImage.TYPE_INT_RGB);
        graph.render(bi.getGraphics());

    }

    /*
    * Handle graph png.
    */
    private static void handleRrdGraph(OtpErlangObject caller, OtpErlangTuple tuple) throws Exception
    {
        OtpErlangString srcFile = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangString dstPng  = (OtpErlangString) (tuple.elementAt(1));

        System.out.println("will graph?");
        RrdGraphDef gDef = new RrdGraphDef();
        gDef.setWidth(500);
        gDef.setHeight(300);
        gDef.setStartTime(-3000);
        gDef.setEndTime(-1);
        gDef.setImageFormat("png");
        gDef.setFilename("test.png");
        gDef.setTitle("errd4j test");
        gDef.setVerticalLabel("speed");
        gDef.datasource("speed", "test.rrd", "speed", ConsolFun.AVERAGE);
        
        RrdGraph graph = new RrdGraph(gDef);
        System.out.println("graph ok?: " + graph.getRrdGraphInfo().getByteCount());
        
        OtpErlangTuple ureply = buildOkReply(new OtpErlangLong(graph.getRrdGraphInfo().getByteCount()));
        sendReply(caller, ureply);
    }


    /*
    * Handle update a rrd file.
    */
    private static void handleRrdUpdate(OtpErlangObject caller, OtpErlangTuple tuple) throws Exception
    {
        OtpErlangString filePath = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangList   updates  = (OtpErlangList)   (tuple.elementAt(1));

        RrdDb rrdDb   = new RrdDb(filePath.stringValue());

        try {
            Sample sample = rrdDb.createSample();
            System.out.println("sample time is: " + sample.getTime());
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
            rrdDb.close();
            throw e;  
        }
        rrdDb.close();
        sendReply(caller, atomOk);
    }
    
    /*
    * Handle create a rrd file.
    */
    private static void handleRrdCreate(OtpErlangObject caller, OtpErlangTuple tuple) throws Exception
    {
        OtpErlangString filePath = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangLong   step     = (OtpErlangLong)   (tuple.elementAt(1));
        OtpErlangList   rras     = (OtpErlangList)   (tuple.elementAt(2));
        OtpErlangList   dss      = (OtpErlangList)   (tuple.elementAt(3));

        RrdDef rrdDef = new RrdDef(filePath.stringValue(), step.uIntValue());

        Iterator<OtpErlangObject> rrasIt = rras.iterator();
        while (rrasIt.hasNext())
        {
            OtpErlangTuple  rra      = (OtpErlangTuple)     rrasIt.next();
            OtpErlangString rraCfStr = (OtpErlangString)    (rra.elementAt(0));
            OtpErlangDouble rraXff   = (OtpErlangDouble)    (rra.elementAt(1));
            OtpErlangLong   rraStep  = (OtpErlangLong)      (rra.elementAt(2));
            OtpErlangLong   rraRows  = (OtpErlangLong)      (rra.elementAt(3));
            
            ConsolFun rraCf = ConsolFun.valueOf(rraCfStr.stringValue());
            rrdDef.addArchive(
                rraCf,
                rraXff.doubleValue(),
                rraStep.uIntValue(),
                rraRows.uIntValue()
            );
            
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

        RrdDb rrdDb = new RrdDb(rrdDef);
        rrdDb.close();

        OtpErlangTuple ureply = buildOkReply(new OtpErlangString("yoho"));
        sendReply(caller, ureply);
    }

}
