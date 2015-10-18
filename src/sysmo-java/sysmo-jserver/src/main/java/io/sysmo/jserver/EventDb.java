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

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.Statement;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonBuilderFactory;
import javax.json.JsonObjectBuilder;
import javax.json.JsonWriter;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

import java.sql.Timestamp;
import java.sql.Types;
import java.util.Calendar;
import java.util.Properties;

public class EventDb implements Runnable
{
    private Logger logger;
    private OtpMbox mbox;
    private String foreignNodeName;
    private Connection conn;

    private static EventDb instance;

    private static OtpErlangAtom atomReply = new OtpErlangAtom("reply");
    private static OtpErlangAtom atomOk = new OtpErlangAtom("ok");
    private static OtpErlangAtom atomError = new OtpErlangAtom("error");

    // NCHECKS_EVENTS and NCHECKS_LATEST_EVENTS table insert prepared statement
    private PreparedStatement psInsert;
    private PreparedStatement psInsertLast;
    private static final int PROBE_ID      = 1;
    private static final int DATE_CREATED  = 2;
    private static final int NCHECKS_ID    = 3;
    private static final int STATUS        = 4;
    private static final int STATUS_CODE   = 5;
    private static final int RETURN_STRING = 6;
    private static final int MONTH_CREATED = 7;
    private static final int HOST_DISPLAY  = 8;
    private static final int HOST_LOCATION = 9;
    private static final int HOST_CONTACT  = 10;
    private static final int PROBE_DISPLAY = 11;
    private static final int EVENT_ID      = 12;

    // NCHECKS_LATEST_EVENTS table delete prepared statement
    private PreparedStatement psDeletePrevious;
    private static final int PREVIOUS_PROBE_EVENT = 1;

    // SELECTS
    private PreparedStatement psSelectProbeEvents;

    private PreparedStatement psSelectLatestEvents;
    private static final int CURRENT_MONTH  = 1;
    private static final int PREVIOUS_MONTH = 2;

    public static EventDb getInstance(
            final OtpMbox mbox,
            final String foreignNodeName,
            final String dataDir) throws Exception {

        EventDb.instance = new EventDb(mbox,foreignNodeName,dataDir);
        return EventDb.instance;
    }

    private EventDb(
            final OtpMbox mbox,
            final String foreignNodeName,
            final String dataDir) throws Exception {
        this.mbox = mbox;
        this.foreignNodeName = foreignNodeName;
        this.logger = LoggerFactory.getLogger(EventDb.class);

        /*
         * Initialize derby.system.home
         */
        String derbySysmoHome = Paths.get(dataDir, "events").toString();

        Properties p = System.getProperties();
        p.setProperty("derby.system.home", derbySysmoHome);
        this.logger.info("derby.system.home set: " + derbySysmoHome);

        /*
         * Load the driver
         */
        try {
            Class.forName("org.apache.derby.jdbc.EmbeddedDriver").newInstance();
            this.logger.info("Loaded the appropriate driver");
        } catch (Exception e) {
            this.logger.error(e.getMessage(), e);
            throw e;
        }

        String protocol = "jdbc:derby:";
        this.conn = null;

        /*
         * Boot database
         */
        try {
            Properties props = new Properties();
            props.put("user", "user1");
            props.put("password", "user1");

            String dbName = "sysmoDB";

            // Create (if needed) and connect to the database
            this.conn = DriverManager.getConnection(protocol + dbName
                    + ";create=true", props);

            this.logger.info("Connected to and created database " + dbName);

            this.conn.setAutoCommit(false);

            Statement statement = this.conn.createStatement();
            //statements.add(s);

            try {
                statement.execute("CREATE TABLE NCHECKS_EVENTS("
                        + "EVENT_ID INT NOT NULL "
                            + "GENERATED ALWAYS AS IDENTITY "
                                + "(START WITH 1, INCREMENT BY 1),"
                        + "PROBE_ID      VARCHAR(40)  NOT NULL,"
                        + "MONTH_CREATED VARCHAR(6)   NOT NULL," // for master sync ie: 201509
                        + "DATE_CREATED  TIMESTAMP    NOT NULL," // from notif ts
                        + "NCHECKS_ID    VARCHAR(40)  NOT NULL,"
                        + "STATUS        VARCHAR(20)  NOT NULL,"
                        + "STATUS_CODE   INT          NOT NULL,"
                        + "RETURN_STRING VARCHAR(255) NOT NULL,"
                        + "HOST_DISPLAY  VARCHAR(255) NOT NULL,"
                        + "HOST_LOCATION VARCHAR(255) NOT NULL,"
                        + "HOST_CONTACT  VARCHAR(255) NOT NULL,"
                        + "PROBE_DISPLAY VARCHAR(255) NOT NULL,"
                        + "PRIMARY KEY (EVENT_ID))");

                statement.execute("CREATE INDEX PROBE_ID_INDEX "
                        + "ON NCHECKS_EVENTS (PROBE_ID)");

                statement.execute("CREATE INDEX MONTH_CREATED_INDEX "
                        + "ON NCHECKS_EVENTS (MONTH_CREATED)");

                statement.execute("CREATE TABLE NCHECKS_LATEST_EVENTS("
                        + "EVENT_ID        INT          NOT NULL,"
                        + "PROBE_ID        VARCHAR(40)  NOT NULL,"
                        + "MONTH_CREATED   VARCHAR(6)   NOT NULL,"
                        + "DATE_CREATED    TIMESTAMP    NOT NULL," // from notif ts
                        + "NCHECKS_ID      VARCHAR(40)  NOT NULL,"
                        + "STATUS          VARCHAR(20)  NOT NULL,"
                        + "STATUS_CODE     INT          NOT NULL,"
                        + "RETURN_STRING   VARCHAR(255) NOT NULL,"
                        + "HOST_DISPLAY    VARCHAR(255) NOT NULL,"
                        + "HOST_LOCATION   VARCHAR(255) NOT NULL,"
                        + "HOST_CONTACT    VARCHAR(255) NOT NULL,"
                        + "PROBE_DISPLAY   VARCHAR(255) NOT NULL,"
                        + "PRIMARY KEY (EVENT_ID))");
                this.conn.commit();
                this.logger.info("Database successfully initialized");

            } catch (SQLException e) {
                if (e.getSQLState().equals("X0Y32")) {
                    this.logger.info("Database already initialized.");
                } else {
                    this.logger.error("Error in database initialization");
                    this.printSQLException(e);
                    throw e;
                }
            } finally {
                try {
                    if (statement != null) {
                        statement.close();
                    }
                } catch (SQLException e) {
                    // ignore
                }
            }

            this.conn.setAutoCommit(true);


            this.psInsert = conn.prepareStatement(
                    "INSERT INTO NCHECKS_EVENTS "
                            + "(PROBE_ID,DATE_CREATED,NCHECKS_ID,"
                            + "STATUS,STATUS_CODE,RETURN_STRING,MONTH_CREATED,"
                            + "HOST_DISPLAY,HOST_LOCATION,HOST_CONTACT,"
                            + "PROBE_DISPLAY) "
                            + "VALUES (?,?,?,?,?,?,?,?,?,?,?)",
                    Statement.RETURN_GENERATED_KEYS);

            this.psInsertLast = conn.prepareStatement(
                    "INSERT INTO NCHECKS_LATEST_EVENTS "
                            + "(PROBE_ID,DATE_CREATED,NCHECKS_ID,"
                            + "STATUS,STATUS_CODE,RETURN_STRING,MONTH_CREATED,"
                            + "HOST_DISPLAY,HOST_LOCATION,HOST_CONTACT,"
                            + "PROBE_DISPLAY,EVENT_ID) "
                            + "VALUES (?,?,?,?,?,?,?,?,?,?,?,?)");


            this.psDeletePrevious = conn.prepareStatement(
                    "DELETE FROM NCHECKS_LATEST_EVENTS "
                            + "WHERE PROBE_ID = ?");


            this.psSelectProbeEvents = conn.prepareStatement(
                    "SELECT * FROM NCHECKS_EVENTS WHERE PROBE_ID = ?");

            this.psSelectLatestEvents = conn.prepareStatement(
                    "SELECT * FROM NCHECKS_EVENTS "
                            + "WHERE MONTH_CREATED = ? OR MONTH_CREATED = ? "
                            + "UNION "
                            + "SELECT * FROM NCHECKS_LATEST_EVENTS");

        } catch (SQLException e) {
            printSQLException(e);
            throw e;
        }
    }

    @Override
    public void run() {
        // begin to loop and wait for calls (select) or casts (insert)
        this.logger.info("begin to loop");

        OtpErlangObject call;
        while (true) try {

            call = this.mbox.receive();
            this.handleEvent(call);

        } catch (OtpErlangExit e) {
            this.logger.info(e.getMessage(), e);
            break;
        } catch (OtpErlangDecodeException e) {
            this.mbox.exit("erlang_decode_exception");
            this.logger.info(e.getMessage(), e);
            break;
        } catch (SQLException e) {
            this.mbox.exit("sql_exception");
            this.printSQLException(e);
            break;
        }

        Connection con = null;
        try {
            con = DriverManager.getConnection("jdbc:derby:;shutdown=true");
        } catch (SQLException se) {
            if (se.getErrorCode() == 50000 &&
                    se.getSQLState().equals("XJ015")) {
                this.logger.info("Derby shut down normally");
            } else {
                this.logger.error("Derby did not shut down normally");
                this.printSQLException(se);
            }
        } finally {
            if (con != null) {
                try {
                    con.close();
                } catch (SQLException ignore) {
                    //ignore
                }
            }
        }

        // CLEANUP close statements
        try {
            if (this.psDeletePrevious != null) {
                this.psDeletePrevious.close();
            }
        } catch (SQLException e) {
            this.printSQLException(e);
        }

        try {
            if (this.psInsertLast != null) {
                this.psInsertLast.close();
            }
        } catch (SQLException e) {
            this.printSQLException(e);
        }

        try {
            if (this.psInsert != null) {
                this.psInsert.close();
            }
        } catch (SQLException e) {
            this.printSQLException(e);
        }

        // CLEANUP close connection
        try {
            if (this.conn != null) {
                this.conn.close();
            }
        } catch (SQLException e) {
            this.printSQLException(e);
        }
    }

    private void handleEvent(OtpErlangObject event) throws SQLException {
        OtpErlangTuple tuple = (OtpErlangTuple) event;
        OtpErlangAtom command = (OtpErlangAtom) tuple.elementAt(0);

        switch (command.toString()) {
            case "notify":
                this.handleNotification(tuple.elementAt(1));
                break;
            case "dump_latest_events":
                this.handleSelectLatestEvents(tuple);
                break;
            case "dump_probe_events":
                this.handleSelectProbeEvents(tuple);
                break;
            default:
                this.logger.info("Unknown command " + command.toString());
        }
    }

    private void handleSelectLatestEvents(OtpErlangTuple call)
    {
        OtpErlangObject caller  = call.elementAt(1);
        OtpErlangString dumpDir = (OtpErlangString) call.elementAt(2);

        String file = "latest.json";
        String filePath = Paths.get(dumpDir.stringValue(), file).toString();

        OtpErlangObject reply;
        ResultSet rs = null;

        String currentMonth = this.getCurrentMonth();
        String previousMonth = this.getPreviousMonth();
        try {
            this.psSelectLatestEvents.setString(EventDb.CURRENT_MONTH, currentMonth);
            this.psSelectLatestEvents.setString(EventDb.PREVIOUS_MONTH, previousMonth);
            rs = this.psSelectLatestEvents.executeQuery();
            this.writeToJsonFile(rs, filePath);
            reply = this.buildOkReply(new OtpErlangString(file));
        } catch (Exception e) {
            reply = this.buildErrorReply(new OtpErlangString(e.getMessage()));
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
            } catch (SQLException ignore) {
                // ignore
            }
        }

        this.sendReply(caller, reply);
    }


    private void handleSelectProbeEvents(OtpErlangTuple call)
    {
        OtpErlangObject caller  = call.elementAt(1);
        OtpErlangTuple payload  = (OtpErlangTuple)  call.elementAt(2);

        OtpErlangString probe   = (OtpErlangString) payload.elementAt(0);
        OtpErlangString dumpDir = (OtpErlangString) payload.elementAt(1);

        String file = "allProbeEvents.json";
        String filePath = Paths.get(dumpDir.stringValue(), file).toString();

        OtpErlangObject reply;
        ResultSet rs = null;
        try {
            this.psSelectProbeEvents.setString(1, probe.stringValue());
            rs = this.psSelectProbeEvents.executeQuery();
            this.writeToJsonFile(rs, filePath);
            reply = this.buildOkReply(new OtpErlangString(file));
        } catch (Exception e) {
            this.logger.error("Select probe error: " + probe.stringValue(), e);
            reply = this.buildErrorReply(new OtpErlangString(e.getMessage()));
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
            } catch (SQLException ignore) {
                //ignore
            }
        }

        this.sendReply(caller, reply);
    }

    private void handleNotification(OtpErlangObject notif) throws SQLException {
        OtpErlangTuple tuple = (OtpErlangTuple) notif;
        OtpErlangString      probeIdObj = (OtpErlangString) tuple.elementAt(1);
        OtpErlangString      checkIdObj = (OtpErlangString) tuple.elementAt(2);
        OtpErlangString       statusObj = (OtpErlangString) tuple.elementAt(3);
        OtpErlangLong     statusCodeObj = (OtpErlangLong)   tuple.elementAt(4);
        OtpErlangLong      timestampObj = (OtpErlangLong)   tuple.elementAt(5);
        OtpErlangString returnStringObj = (OtpErlangString) tuple.elementAt(6);
        OtpErlangString probeDisplayObj = (OtpErlangString) tuple.elementAt(7);
        OtpErlangString  hostDisplayObj = (OtpErlangString) tuple.elementAt(8);
        OtpErlangString hostLocationObj = (OtpErlangString) tuple.elementAt(9);
        OtpErlangString  hostContactObj = (OtpErlangString) tuple.elementAt(10);

        String probeId = probeIdObj.stringValue();
        String checkId = checkIdObj.stringValue();
        String status = statusObj.stringValue();
        int statusCode = (int) statusCodeObj.longValue();
        Timestamp timestamp = new Timestamp(timestampObj.longValue() * 1000);
        String returnString = returnStringObj.stringValue();
        String probeDisplayName = probeDisplayObj.stringValue();
        String hostDisplayName = hostDisplayObj.stringValue();
        String hostLocation = hostLocationObj.stringValue();
        String hostContact = hostContactObj.stringValue();
        String monthRow = this.getCurrentMonth();

        this.psInsert.setString(EventDb.PROBE_ID, probeId);
        this.psInsert.setString(EventDb.NCHECKS_ID, checkId);
        this.psInsert.setString(EventDb.MONTH_CREATED, monthRow);
        this.psInsert.setTimestamp(EventDb.DATE_CREATED, timestamp);
        this.psInsert.setString(EventDb.STATUS, status);
        this.psInsert.setInt(EventDb.STATUS_CODE, statusCode);
        this.psInsert.setString(EventDb.RETURN_STRING, returnString);
        this.psInsert.setString(EventDb.PROBE_DISPLAY, probeDisplayName);
        this.psInsert.setString(EventDb.HOST_DISPLAY, hostDisplayName);
        this.psInsert.setString(EventDb.HOST_LOCATION, hostLocation);
        this.psInsert.setString(EventDb.HOST_CONTACT, hostContact);

        this.psInsert.executeUpdate();

        int key;
        try {
            ResultSet keyRs = this.psInsert.getGeneratedKeys();
            keyRs.next();
            key = keyRs.getInt(1);
        } catch (SQLFeatureNotSupportedException|NullPointerException e) {
            this.logger.error("getGeneratedKeys fail: " + e);
            throw e;
        }

        try {
            this.psDeletePrevious.setString(
                    EventDb.PREVIOUS_PROBE_EVENT, probeId);
            this.psDeletePrevious.executeUpdate();
        } catch (SQLException e) {
            // key does not exist
        }

        this.psInsertLast.setInt(EventDb.EVENT_ID, key);
        this.psInsertLast.setString(EventDb.PROBE_ID, probeId);
        this.psInsertLast.setString(EventDb.NCHECKS_ID, checkId);
        this.psInsertLast.setString(EventDb.MONTH_CREATED, monthRow);
        this.psInsertLast.setTimestamp(EventDb.DATE_CREATED, timestamp);
        this.psInsertLast.setString(EventDb.STATUS, status);
        this.psInsertLast.setInt(EventDb.STATUS_CODE, statusCode);
        this.psInsertLast.setString(EventDb.RETURN_STRING, returnString);
        this.psInsertLast.setString(EventDb.PROBE_DISPLAY, probeDisplayName);
        this.psInsertLast.setString(EventDb.HOST_DISPLAY, hostDisplayName);
        this.psInsertLast.setString(EventDb.HOST_LOCATION, hostLocation);
        this.psInsertLast.setString(EventDb.HOST_CONTACT, hostContact);
        this.psInsertLast.executeUpdate();

        //this.printTables();
        MailEventMessage message = new MailEventMessage(
                probeId,checkId,status,statusCode,
                timestamp,returnString,probeDisplayName,hostDisplayName,
                hostLocation,hostContact);

        MailSender.sendMail(message);
    }

    private void printSQLException(SQLException e)
    {
        while (e != null)
        {
            String errorOutput = "\n\n";
            errorOutput += "SQLException ========================= BEGIN\n";
            errorOutput += "SQLState:\t" + e.getSQLState() + "\n";
            errorOutput += "Severity:\t" + e.getErrorCode() + "\n";
            errorOutput += "Message:\t"  + e.getMessage() + "\n";
            this.logger.warn(errorOutput, e);
            this.logger.warn("SQLException ========================= END\n\n");
            e = e.getNextException();
        }
    }

    public void printTables() {

        Statement s = null;
        Statement s2 = null;
        ResultSet rs = null;
        ResultSet rs2 = null;
        try {
            s = this.conn.createStatement();
            s2 = this.conn.createStatement();
            rs = s.executeQuery("SELECT * FROM NCHECKS_EVENTS");
            rs2 = s2.executeQuery("SELECT * FROM NCHECKS_LATEST_EVENTS");
            this.logger.info("Result set event: ");
            this.logger.info(this.tableToString(rs));
            this.logger.info("Result set latest events: ");
            this.logger.info(this.tableToString(rs2));
        } catch (SQLException e) {
            this.printSQLException(e);
        } finally {
            try {
                if (s != null) {
                    s.close();
                }
            } catch (SQLException e) {
                //ignore
            }
            try {
                if (s2 != null) {
                    s2.close();
                }
            } catch (SQLException e) {
                //ignore
            }
            try {
                if (rs != null) {
                    rs.close();
                }
            } catch (SQLException e) {
                //ignore
            }
            try {
                if (rs2 != null) {
                    rs2.close();
                }
            } catch (SQLException e) {
                //ignore
            }
        }
    }

    private String tableToString(ResultSet rs) {
        StringBuilder buff = new StringBuilder();
        buff.append("\n\n");
        try {
            ResultSetMetaData rsmd = rs.getMetaData();
            int columnsNumber = rsmd.getColumnCount();

            while (rs.next()) {
                for (int i=1; i<=columnsNumber; i++) {
                    buff.append(rs.getString(i));
                    buff.append("\t");

                }
                buff.append("\n");
            }
        } catch (SQLException e) {
            //ignore
        }
        buff.append("\n\n");
        return buff.toString();
    }

    private void sendReply(
            final OtpErlangObject to, final OtpErlangObject msg)
    {
        OtpErlangObject[] obj = new OtpErlangObject[3];
        obj[0] = EventDb.atomReply;
        obj[1] = to;
        obj[2] = msg;
        OtpErlangTuple tuple = new OtpErlangTuple(obj);
        this.mbox.send("j_server_eventdb", this.foreignNodeName, tuple);
    }

    private OtpErlangTuple buildErrorReply(OtpErlangObject msg)
    {
        OtpErlangObject[] valObj = new OtpErlangObject[2];
        valObj[0] = EventDb.atomError;
        valObj[1] = msg;
        return new OtpErlangTuple(valObj);
    }

    private OtpErlangTuple buildOkReply(OtpErlangObject msg)
    {
        OtpErlangObject[] valObj = new OtpErlangObject[2];
        valObj[0] = EventDb.atomOk;
        valObj[1] = msg;
        return new OtpErlangTuple(valObj);
    }

    public void writeToJsonFile(ResultSet resultSet, String file)
            throws SQLException
    {
        JsonBuilderFactory factory = Json.createBuilderFactory(null);
        JsonArrayBuilder arrayBuilder = factory.createArrayBuilder();

        while (resultSet.next()) {
            int total_rows = resultSet.getMetaData().getColumnCount();
            JsonObjectBuilder obj = factory.createObjectBuilder();
            for (int i = 0; i < total_rows; i++) {
                int rowType = resultSet.getMetaData().getColumnType(i + 1);
                if (rowType == Types.TIMESTAMP) {
                    Timestamp tsObj = resultSet.getTimestamp(i + 1);
                    obj.add(resultSet.getMetaData().getColumnLabel(i + 1),
                            tsObj.getTime() / 1000);
                } else if (rowType == Types.INTEGER) {
                    Integer tsObj = resultSet.getInt(i + 1);
                    obj.add(resultSet.getMetaData().getColumnLabel(i + 1),
                            tsObj);
                } else {
                    obj.add(resultSet.getMetaData().getColumnLabel(i + 1),
                            resultSet.getObject(i + 1).toString());
                }
            }
            arrayBuilder.add(obj);
        }

        FileOutputStream output;
        try {
            output = new FileOutputStream(file);
        } catch (FileNotFoundException e) {
            this.logger.error("Can't create dump file", e);
            return;
        }

        JsonWriter jsonWriter = Json.createWriter(output);
        jsonWriter.writeArray(arrayBuilder.build());
    }

    private String getCurrentMonth() {
        Calendar now = Calendar.getInstance();
        int month = now.get(Calendar.MONTH);
        int year = now.get(Calendar.YEAR);
        return String.valueOf(year) + String.valueOf(month);
    }

    private String getPreviousMonth() {
        Calendar notNow = Calendar.getInstance();
        notNow.add(Calendar.MONTH, -1);
        int month = notNow.get(Calendar.MONTH);
        int year = notNow.get(Calendar.YEAR);
        return String.valueOf(year) + String.valueOf(month);
    }
}
