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

import com.ericsson.otp.erlang.OtpMbox;

import com.sleepycat.je.Database;
import com.sleepycat.je.DatabaseConfig;
import com.sleepycat.je.DatabaseEntry;
import com.sleepycat.je.Environment;
import com.sleepycat.je.EnvironmentConfig;
import com.sleepycat.je.LockMode;
import com.sleepycat.je.OperationStatus;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.UnsupportedEncodingException;

import java.net.ServerSocket;
import java.net.Socket;

import java.nio.file.Paths;

/**
 * Created by seb on 18/10/15.
 * TODO Provide berkley db store for NChecks modules state.
 */
public class StateServer implements Runnable {
    private static StateServer instance;
    private static final String DB_NAME = "NCHECKS_STATES";
    public static final int DEFAULT_PORT = 9760;
    private Database db;
    private Environment env;
    private Logger logger;
    private OtpMbox mbox;
    private StateServerSocket server;
    private final Object stopLock = new Object();
    private final Object lock = new Object();

    public void stop() {
        synchronized (this.stopLock) {
            this.stopLock.notify();
        }
    }

    public static synchronized byte[] getState(String key) {
        synchronized (StateServer.instance.lock) {
            StateServer.instance.logger.info("get state called");
            try {
                DatabaseEntry theKey = new DatabaseEntry(key.getBytes("UTF-8"));
                DatabaseEntry data = new DatabaseEntry();
                if (StateServer.instance.db.get(null, theKey, data,
                        LockMode.DEFAULT) == OperationStatus.SUCCESS) {
                    return data.getData();
                } else {
                    return new byte[0];
                }
            } catch (UnsupportedEncodingException e) {
                StateServer.instance.logger.warn(e.getMessage(), e);
                return new byte[0];
            }
        }
    }

    public static synchronized void setState(String key, byte[] value) {
        synchronized (StateServer.instance.lock) {
            try {
                DatabaseEntry theKey = new DatabaseEntry(key.getBytes("UTF-8"));
                DatabaseEntry theData = new DatabaseEntry(value);
                StateServer.instance.db.put(null, theKey, theData);
                StateServer.instance.logger.info("log to state server key: " + key);
            } catch (UnsupportedEncodingException e) {
                // ignore
            }
        }
    }
    public static synchronized StateServer getInstance(
            String dataDir,
            int port,
            OtpMbox mbox) throws IOException {
        if (StateServer.instance == null) {
            if (port == 0) {
                port = StateServer.DEFAULT_PORT;
            }
            StateServer.instance =
                    new StateServer(dataDir, port, mbox);
        }
        return StateServer.instance;
    }

    private StateServer(String dataDir, int port, OtpMbox mbox)
            throws IOException
    {

        this.logger = LoggerFactory.getLogger(this.getClass());
        this.mbox = mbox;

        // init db
        String home = Paths.get(dataDir, "states").toString();
        EnvironmentConfig envConfig = new EnvironmentConfig();
        envConfig.setAllowCreate(true);
        this.env = new Environment(new File(home), envConfig);

        DatabaseConfig dbConfig = new DatabaseConfig();
        dbConfig.setAllowCreate(true);
        dbConfig.setTemporary(true);
        this.db = this.env.openDatabase(null, StateServer.DB_NAME, dbConfig);
        this.logger.info("database ok");

        this.server = new StateServerSocket(port);
        Thread serverThread = new Thread(server);
        serverThread.start();
        this.logger.info("server listening on " + port);

    }

    @Override
    public void run() {
        // populate test
        String aKey = "keyjojo";
        String aData = "datajojoqsdfqsdfj";
        try {
            this.logger.info("will put data in db");
            DatabaseEntry theKey = new DatabaseEntry(aKey.getBytes("UTF-8"));
            DatabaseEntry theData = new DatabaseEntry(aData.getBytes("UTF-8"));
            this.db.put(null, theKey, theData);

            this.logger.info("will get data from db");
            DatabaseEntry theKey2 = new DatabaseEntry("keyjojo".getBytes("UTF-8"));
            DatabaseEntry theData2 = new DatabaseEntry();
            if (this.db.get(null, theKey2, theData2, LockMode.DEFAULT) ==
                    OperationStatus.SUCCESS) {
                byte[] binData = theData2.getData();
                String strData = new String(binData, "UTF-8");
                this.logger.info("for key: 'key' found data: " + strData);
            } else {
                this.logger.info("for key: 'key' found no data");
            }
        } catch (UnsupportedEncodingException e) {
            this.logger.error("encoding exception", e);
        }

        try {
            synchronized (this.stopLock) {
                this.stopLock.wait();
            }
        } catch (InterruptedException e) {
            this.logger.error(e.getMessage(), e);
            // mbox.exit will close SysmoServer thread and all childs.
            this.mbox.exit("crash");
        } finally {
            this.db.close();
            this.env.close();
            this.server.stop();
            this.mbox.close();
            this.logger.info("end run");
        }
    }


    // utility classes
    static class StateServerSocket implements Runnable
    {
        // server loop
        private ServerSocket server = null;
        private Logger logger;

        StateServerSocket(int port) throws IOException {
            this.logger = LoggerFactory.getLogger(this.getClass());
            this.server = new ServerSocket(port);
            this.logger.info("create state server socket");
        }

        public void stop() {
            this.logger.info("stop socket listener");
            if (this.server != null) try {
                this.server.close();
                this.server = null;
            } catch (IOException e) {
                // ignore
            }
        }

        @Override
        public void run() {
            this.logger.info("Start socket listener");
            while (true) try {

                Socket client = this.server.accept();
                Runnable clientRunnable = new ServerClientSocket(client);
                Thread clientThread = new Thread(clientRunnable);
                clientThread.start();
                this.logger.debug("have accepted client");

            } catch (Exception | Error e) {
                this.logger.error(e.getMessage(), e);
                if (this.server != null) {
                    try {
                        this.server.close();
                    } catch (IOException ignore) {
                        // ignore
                    }
                }
                break;
            }
            StateServer.instance.stop();
        }
    }

    static class ServerClientSocket implements Runnable {
        private Socket socket;
        private Logger logger;

        ServerClientSocket(Socket socket) {
            this.logger = LoggerFactory.getLogger(this.getClass());
            this.socket = socket;
            this.logger.info("Accept client for socket: " +
                    socket.getInetAddress());
        }

        @Override
        public void run() {
            this.logger.debug("run client for socket");
            ObjectInputStream in = null;
            ObjectOutputStream out = null;
            try {
                // Why ???
                this.socket.getOutputStream().flush();
                //

                in = new ObjectInputStream(this.socket.getInputStream());
                out = new ObjectOutputStream(this.socket.getOutputStream());

                String key;
                byte[] bytes;

                StateMessage message;
                StateMessage reply;
                while (this.socket.isConnected()) try {

                    message = (StateMessage) in.readObject();

                    /*
                     * get action
                     */
                    switch (message.getAction()) {
                        case StateMessage.SET:
                            key = message.getKey();
                            bytes = message.getBytes();
                            StateServer.setState(key, bytes);
                            break;
                        case StateMessage.GET:
                            key = message.getKey();
                            bytes = StateServer.getState(key);
                            reply = new StateMessage(StateMessage.GET);
                            reply.setKey(key);
                            reply.setBytes(bytes);
                            out.writeObject(reply);
                            out.flush();
                            break;
                    }

                } catch (Exception inner) {
                    break;
                }


            } catch (Exception e) {
                this.logger.warn(e.getMessage(), e);
                // ignore
            } finally {
                try {
                    if (in != null) {
                        in.close();
                    }
                } catch (IOException ignore) {
                    // ignore
                }

                try {
                    if (out != null) {
                        out.close();
                    }
                } catch (IOException ignore) {
                    // ignore
                }

                try {
                    this.socket.close();
                } catch (IOException ignore) {
                    // ignore
                }
            }
        }
    }
}
