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

import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;

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
import java.util.Properties;

public class EventDb implements Runnable
{
    private Logger logger;
    private OtpMbox mbox;
    private String foreignNodeName;
    private Connection conn;

    // NCHECKS_EVENTS table prepared statement
    private static final int PROBE_ID      = 1;
    private static final int DATE_CREATED  = 2;
    private static final int NCHECKS_ID    = 3;
    private static final int STATUS        = 4;
    private static final int STATUS_CODE   = 5;
    private static final int RETURN_STRING = 6;
    private static final int MONTH_CREATED = 7;

    // NCHECKS_LATEST_EVENTS table prepared statement
    private static final int LATEST_PROBE_ID      = 1;
    private static final int LATEST_DATE_CREATED  = 2;
    private static final int LATEST_NCHECKS_ID    = 3;
    private static final int LATEST_STATUS        = 4;
    private static final int LATEST_STATUS_CODE   = 5;
    private static final int LATEST_RETURN_STRING = 6;
    private static final int LATEST_HOST_DISPLAY  = 7;
    private static final int LATEST_HOST_LOCATION = 8;
    private static final int LATEST_HOST_CONTACT  = 9;
    private static final int LATEST_PROBE_DISPLAY = 10;

    private PreparedStatement psInsert;
    private PreparedStatement psDeleteLast;
    private PreparedStatement psInsertLast;

    EventDb(final OtpMbox mbox, final String foreignNodeName,
            final String dataDir) throws SQLException {
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
        }

        String protocol = "jdbc:derby:";
        this.conn = null;
        //Statement s;

        //ArrayList<Statement> statements = new ArrayList<>();
        //PreparedStatement psUpdate;
        //ResultSet rs = null;

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
                        + "MONTH_CREATED INT          NOT NULL," // for master sync ie: 201509
                        + "DATE_CREATED  TIMESTAMP    NOT NULL," // from notif ts
                        + "NCHECKS_ID    VARCHAR(40)  NOT NULL,"
                        + "STATUS        VARCHAR(20)  NOT NULL,"
                        + "STATUS_CODE   INT          NOT NULL,"
                        + "RETURN_STRING VARCHAR(255) NOT NULL,"
                        + "PRIMARY KEY (EVENT_ID))");

                statement.execute("CREATE INDEX PROBE_ID_INDEX "
                        + "ON NCHECKS_EVENTS (PROBE_ID)");

                statement.execute("CREATE INDEX MONTH_CREATED_INDEX "
                        + "ON NCHECKS_EVENTS (MONTH_CREATED)");

                statement.execute("CREATE TABLE NCHECKS_LATEST_EVENTS("
                        + "PROBE_ID        VARCHAR(40)  NOT NULL,"
                        + "DATE_CREATED    TIMESTAMP    NOT NULL," // from notif ts
                        + "NCHECKS_ID      VARCHAR(40)  NOT NULL,"
                        + "STATUS          VARCHAR(20)  NOT NULL,"
                        + "STATUS_CODE     INT          NOT NULL,"
                        + "RETURN_STRING   VARCHAR(255) NOT NULL,"
                        + "HOST_DISPLAY    VARCHAR(255) NOT NULL,"
                        + "HOST_LOCATION   VARCHAR(255) NOT NULL,"
                        + "HOST_CONTACT    VARCHAR(255) NOT NULL,"
                        + "PROBE_DISPLAY   VARCHAR(255) NOT NULL,"
                        + "PRIMARY KEY (PROBE_ID))");
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
                            + "STATUS,STATUS_CODE,RETURN_STRING,"
                            + "MONTH_CREATED) "
                            + "VALUES (?,?,?,?,?,?,?)");

            this.psDeleteLast = conn.prepareStatement(
                    "DELETE FROM NCHECKS_LATEST_EVENTS "
                            + "WHERE PROBE_ID = ?");

            this.psInsertLast = conn.prepareStatement(
                    "INSERT INTO NCHECKS_LATEST_EVENTS "
                            + "(PROBE_ID,DATE_CREATED,NCHECKS_ID,"
                            + "STATUS,STATUS_CODE,RETURN_STRING,"
                            + "HOST_DISPLAY,HOST_LOCATION,HOST_CONTACT,"
                            + "PROBE_DISPLAY) "
                            + "VALUES (?,?,?,?,?,?,?,?,?,?)");

        } catch (SQLException e) {
            printSQLException(e);
            throw e;
        }
    }


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

        try {
            DriverManager.getConnection("jdbc:derby:;shutdown=true");
        } catch (SQLException se) {
            if (se.getErrorCode() == 50000 &&
                    se.getSQLState().equals("XJ015")) {
                this.logger.info("Derby shut down normally");
            } else {
                this.logger.error("Derby did not shut down normally");
                this.printSQLException(se);
            }
        }

        // CLEANUP close statements
        try {
            if (this.psDeleteLast != null) {
                this.psDeleteLast.close();
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
        OtpErlangString      probeId = (OtpErlangString) tuple.elementAt(1);
        OtpErlangString      checkId = (OtpErlangString) tuple.elementAt(2);
        OtpErlangString       status = (OtpErlangString) tuple.elementAt(3);
        OtpErlangLong     statusCode = (OtpErlangLong)   tuple.elementAt(4);
        OtpErlangLong      timestamp = (OtpErlangLong)   tuple.elementAt(5);
        OtpErlangString returnString = (OtpErlangString) tuple.elementAt(6);
        OtpErlangString probeDisplay = (OtpErlangString) tuple.elementAt(7);
        OtpErlangString  hostDisplay = (OtpErlangString) tuple.elementAt(8);
        OtpErlangString hostLocation = (OtpErlangString) tuple.elementAt(9);
        OtpErlangString  hostContact = (OtpErlangString) tuple.elementAt(10);

        Timestamp ts = new Timestamp(timestamp.longValue() * 1000);

        this.psInsert.setString(EventDb.PROBE_ID, probeId.stringValue());
        this.psInsert.setString(EventDb.NCHECKS_ID, checkId.stringValue());
        this.psInsert.setInt(EventDb.MONTH_CREATED, 201509);
        this.psInsert.setTimestamp(EventDb.DATE_CREATED, ts);
        this.psInsert.setString(EventDb.STATUS, status.stringValue());
        this.psInsert.setInt(EventDb.STATUS_CODE, (int) statusCode.longValue());
        this.psInsert.setString(EventDb.RETURN_STRING, returnString.stringValue());
        this.psInsert.executeUpdate();

        try {
            this.psDeleteLast.setString(EventDb.PROBE_ID, probeId.stringValue());
            this.psDeleteLast.executeUpdate();
        } catch (SQLException e) {
            // key does not exist
        }

        this.psInsertLast.setString(
                EventDb.LATEST_PROBE_ID, probeId.stringValue());
        this.psInsertLast.setTimestamp(
                EventDb.LATEST_DATE_CREATED, ts);
        this.psInsertLast.setString(
                EventDb.LATEST_NCHECKS_ID, checkId.stringValue());
        this.psInsertLast.setString(
                EventDb.LATEST_STATUS, status.stringValue());
        this.psInsertLast.setInt(
                EventDb.LATEST_STATUS_CODE, (int) statusCode.longValue());
        this.psInsertLast.setString(
                EventDb.LATEST_RETURN_STRING, returnString.stringValue());
        this.psInsertLast.setString(
                EventDb.LATEST_HOST_DISPLAY, hostDisplay.stringValue());
        this.psInsertLast.setString(
                EventDb.LATEST_PROBE_DISPLAY, probeDisplay.stringValue());
        this.psInsertLast.setString(
                EventDb.LATEST_HOST_LOCATION, hostLocation.stringValue());
        this.psInsertLast.setString(
                EventDb.LATEST_HOST_CONTACT, hostContact.stringValue());
        this.psInsertLast.executeUpdate();

        //this.printTables();

        MailSender.getInstance().sendMail(event);
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
        try {
            s = this.conn.createStatement();
            s2 = this.conn.createStatement();
            ResultSet rs = s.executeQuery("SELECT * FROM NCHECKS_EVENTS");
            ResultSet rs2 = s2.executeQuery("SELECT * FROM NCHECKS_LATEST_EVENTS");
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
        }
    }

    private String tableToString(ResultSet rs) {
        String str = "\n\n";
        try {
            ResultSetMetaData rsmd = rs.getMetaData();
            int columnsNumber = rsmd.getColumnCount();

            while (rs.next()) {
                for (int i=1; i<=columnsNumber; i++) {
                    str = str + rs.getString(i) + "\t";

                }
                str = str + "\n";
            }
        } catch (SQLException e) {
            //ignore
        }
        return str + "\n\n";
    }
}
