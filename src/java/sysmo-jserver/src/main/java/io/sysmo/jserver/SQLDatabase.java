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
import java.sql.SQLException;
import java.sql.Statement;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpMbox;

import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

import java.util.ArrayList;
import java.util.Properties;

/*
 * See http://db.apache.org/derby/papers/DerbyTut/embedded_intro.html
 * and
 * db-derby-10.10.2.0-src/java/demo/workingwithderby/WwdEmbedded.java
 */
public class SQLDatabase implements Runnable
{
    private Logger logger;
    private OtpMbox mbox;
    private String foreignNodeName;
    private Connection conn;

    SQLDatabase(OtpMbox mbox, String foreignNodeName, String dataDir)
    {
        this.mbox = mbox;
        this.foreignNodeName = foreignNodeName;
        this.logger = LoggerFactory.getLogger(SQLDatabase.class);

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
        conn = null;

        ArrayList<Statement> statements = new ArrayList<>();
        PreparedStatement psInsert;
        PreparedStatement psUpdate;
        Statement s;
        ResultSet rs = null;


        /*
         * Boot database
         */
        try {
            Properties props = new Properties();
            props.put("user", "user1");
            props.put("password", "user1");

            String dbName = "sysmoDB";

            // Create (if needed) and connect to the database
            conn = DriverManager.getConnection(protocol + dbName
                    + ";create=true", props);

            System.out.println("Connected to and created database " + dbName);

            conn.setAutoCommit(false);

            s = conn.createStatement();
            statements.add(s);

            /* TODO TODO  TODO TODO TODO TODO TODO TODO TODO
             * TODO TODO  TODO TODO TODO TODO TODO TODO TODO
             * TODO TODO  TODO TODO TODO TODO TODO TODO TODO
             * TODO TODO  TODO TODO TODO TODO TODO TODO TODO
             * TODO TODO  TODO TODO TODO TODO TODO TODO TODO
             * Maybe use year/month number in place of date_created
             *  s.execute("CREATE TABLE NCHECKS_EVENTS(
             *      ID              int          NOT NULL AUTO_INCREMENT,
             *      DATE_CREATED    DATE         NOT_NULL DEFAULT(GETDATE()),
             *      PROBE_ID        varchar(40)  NOT_NULL,
             *      NCHECKS_ID      varchar(40)  NOT_NULL,
             *      STATUS          varchar(20)  NOT_NULL,
             *      STATUS_CODE     int          NOT_NULL,
             *      TIMESTAMP       int          NOT_NULL, // TIMESTAMP type?
             *      STRING          varchar(255) NOT_NULL,
             *      PRIMARY KEY (ID))");
             *
             *
             * For probe synchronization event SELECT
             *  s.execute("CREATE INDEX PROBE_ID_INDEX
             *      ON NCHECKS_EVENTS (PROBE_ID))");
             *
             *
             * For master_channel synchronisation event SELECT
             *  s.execute("CREATE INDEX DATE_CREATED_INDEX
             *      ON NCHECKS_EVENTS (DATE_CREATED))");
             * 1 - select all events from now to 4 weeks back,
             * 2 - select all probes latest event,
             * Contain duplicate but known as the latest event for all probes
             *  s.execute("CREATE TABLE NCHECKS_LATEST_EVENTS(
             *      PROBE_ID        varchar(40)  NOT_NULL,
             *      NCHECKS_ID      varchar(40)  NOT_NULL,
             *      STATUS          varchar(20)  NOT_NULL,
             *      STATUS_CODE     int          NOT_NULL,
             *      TIMESTAMP       int          NOT_NULL,
             *      STRING          varchar(255) NOT_NULL,
             *      PRIMARY KEY (PROBE_ID))");
             *  );
             * 3 - merge and return
             *
             */

            s.execute("CREATE TABLE LOCATION(NUM INT, ADDR VARCHAR(40))");
            System.out.println("Created table location");

            psInsert = conn.prepareStatement(
                        "INSERT INTO LOCATION VALUES (?, ?)");

            statements.add(psInsert);

            psInsert.setInt(1, 1956);
            psInsert.setString(2, "Webster St.");
            psInsert.executeUpdate();
            System.out.println("Inserted 1956 Webster");

            psInsert.setInt(1, 1910);
            psInsert.setString(2, "Union St.");
            psInsert.executeUpdate();
            System.out.println("Inserted 1910 Union");

            psUpdate = conn.prepareStatement(
                        "update location set num=?, addr=? where num=?");
            statements.add(psUpdate);

            psUpdate.setInt(1, 180);
            psUpdate.setString(2, "Grand Ave.");
            psUpdate.setInt(3, 1956);
            psUpdate.executeUpdate();
            System.out.println("Updated 1956 Webster to 180 Grand");

            psUpdate.setInt(1, 300);
            psUpdate.setString(2, "Lakeshore Ave.");
            psUpdate.setInt(3, 180);
            psUpdate.executeUpdate();
            System.out.println("Updated 180 Grand to 300 Lakeshore");


            rs = s.executeQuery(
                    "SELECT num, addr FROM location ORDER BY num");

            int number; // street number retrieved from the database
            boolean failure = false;
            if (!rs.next())
            {
                failure = true;
                reportFailure("No rows in ResultSet");
            }

            if ((number = rs.getInt(1)) != 300)
            {
                failure = true;
                reportFailure(
                        "Wrong row returned, expected num=300, got " + number);
            }

            if (!rs.next())
            {
                failure = true;
                reportFailure("Too few rows");
            }

            if ((number = rs.getInt(1)) != 1910)
            {
                failure = true;
                reportFailure(
                        "Wrong row returned, expected num=1910, got " + number);
            }

            if (rs.next())
            {
                failure = true;
                reportFailure("Too many rows");
            }

            if (!failure) {
                System.out.println("Verified the rows");
            }

            // delete the table
            s.execute("drop table location");
            System.out.println("Dropped table location");

            conn.commit();
            System.out.println("Committed the transaction");

            try {
                DriverManager.getConnection("jdbc:derby:;shutdown=true");
            } catch (SQLException se) {
                if (( (se.getErrorCode() == 50000)
                        && ("XJ015".equals(se.getSQLState()) ))) {
                    System.out.println("Derby shut down normally");
                } else {
                    System.err.println("Derby did not shut down normally");
                    printSQLException(se);
                }
            }
        } catch (SQLException sqle) {
            printSQLException(sqle);
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                    rs = null;
                }
            } catch (SQLException sqle) {
                printSQLException(sqle);
            }

            int i = 0;
            while (!statements.isEmpty()) {
                Statement st = (Statement)statements.remove(i);
                try {
                    if (st != null) {
                        st.close();
                        st = null;
                    }
                } catch (SQLException sqle) {
                    printSQLException(sqle);
                }
            }

            //Connection
            try {
                if (conn != null) {
                    conn.close();
                    conn = null;
                }
            } catch (SQLException sqle) {
                printSQLException(sqle);
            }
        }
    }

    public void run() {
        // begin to loop and wait for calls (select) or casts (insert)
        this.logger.info("begin too loop");
        OtpErlangObject call;
        while (true) try {
            call = this.mbox.receive();
            this.handleEvent(call);
        } catch (OtpErlangExit|OtpErlangDecodeException e) {
            this.logger.warn(e.getMessage(), e);
            try {
                DriverManager.getConnection("jdbc:derby:;shutdown=true");
                this.conn.close();
            } catch (SQLException se) {
                if (    se.getErrorCode() == 50000 &&
                        se.getSQLState().equals("XJ015") ) {
                    this.logger.info("Derby shutdown ok", se);
                }
                this.logger.warn("Shutdown exception: ", se);
            } catch (Exception other) {
                this.logger.warn("Shutdown exception: ", other);
            }
            break;
        }
    }

    private void handleEvent(OtpErlangObject event) {
        this.logger.info("Should insert event!!! " + event.toString());
        MailSender.getInstance().sendMail(event);
    }

    private void reportFailure(String message) {
        this.logger.error("Data verification failed:" + message);
    }

    public void printSQLException(SQLException e)
    {
        while (e != null)
        {
            this.logger.error("SQLException ", e);
            e = e.getNextException();
        }
    }
}
