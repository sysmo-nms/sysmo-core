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

import com.sleepycat.je.Database;
import com.sleepycat.je.DatabaseConfig;
import com.sleepycat.je.DatabaseEntry;
import com.sleepycat.je.Environment;
import com.sleepycat.je.LockMode;
import com.sleepycat.je.OperationStatus;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * Created by seb on 18/10/15.
 * TODO Provide berkley db store for NChecks modules state.
 */
public class NChecksStateServer {
    private static Database db;

    public static synchronized void getState(String key) {

    }

    public static synchronized void setState(String key, String data) {

    }
    public static void startStateServer() {
        // init db
        File home = new File("./states");
        Environment env = new Environment(home, null);
        DatabaseConfig dbConfig = new DatabaseConfig();
        dbConfig.setAllowCreate(true);
        dbConfig.setTemporary(true);
        NChecksStateServer.db = env.openDatabase(null, "nchecks_states", dbConfig);

        // populate test
        String aKey = "key";
        String aData= "data";
        try {
            DatabaseEntry theKey = new DatabaseEntry(aKey.getBytes("UTF-8"));
            DatabaseEntry theData = new DatabaseEntry(aData.getBytes("UTF-8"));
            NChecksStateServer.db.put(null, theKey, theData);
        } catch (UnsupportedEncodingException e) {
            // exception
        }

        // get test
        try {
            DatabaseEntry theKey = new DatabaseEntry("key".getBytes("UTF-8"));
            DatabaseEntry theData = new DatabaseEntry();
            if (NChecksStateServer.db.get(null, theKey, theData, LockMode.DEFAULT) ==
                    OperationStatus.SUCCESS) {
                byte[] originalData = theData.getData();
                String strData = new String(originalData, "UTF-8");
                System.out.println("for key: 'key' found data: " + strData);
            } else {
                System.out.println("for key: 'key' found no data");
            }
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }

        // server loop
        ServerSocket server = null;
        try {
            server = new ServerSocket(8867);
        } catch (IOException e) {
            // ignore
        }

        while (true) try {

            Socket client = server.accept();
            Runnable clientRunnable = new StateStoreClient(client);
            Thread clientThread = new Thread(clientRunnable);
            clientThread.start();

        } catch (Exception|Error e) {
            break;
        }

        // close
    }
}

class StateStoreClient implements Runnable {

    StateStoreClient(Socket client) {

    }

    @Override
    public void run() {
        // handle client socket read write

    }
}
