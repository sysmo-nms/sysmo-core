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

import org.eclipse.jetty.server.Handler;
import org.eclipse.jetty.server.Server;

import org.eclipse.jetty.server.handler.DefaultHandler;
import org.eclipse.jetty.server.handler.HandlerList;
import org.eclipse.jetty.server.handler.ResourceHandler;
import org.eclipse.jetty.server.nio.SelectChannelConnector;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.Properties;

/**
 * Created by seb on 21/08/15.
 */
public class JettyServer
{
    private static Logger logger = LoggerFactory.getLogger(JettyServer.class);
    private static final int DEFAULT_PORT = 9759;
    private static int port;

    public static Server startServer(
            final String docroot,
            final String etcDir)
    {
        String propFile = Paths.get(etcDir, "sysmo.properties").toString();
        InputStream input = null;
        try {
            Properties  props = new Properties();
            input = new FileInputStream(propFile);
            props.load(input);
            JettyServer.port = Integer.parseInt(props.getProperty("web_server_port"));
        } catch (Exception e) {
            JettyServer.logger.error(
                    "Can not read property file. Use default port: " +
                            JettyServer.DEFAULT_PORT);
            JettyServer.port = DEFAULT_PORT;
        } finally {
            try {
                if (input != null) {
                    input.close();
                }
            } catch (IOException ignore) {
                //ignore
            }
        }

        Server jettyThread = new Server();
        SelectChannelConnector connector = new SelectChannelConnector();
        connector.setPort(JettyServer.port);
        jettyThread.addConnector(connector);

        ResourceHandler resource_handler = new ResourceHandler();
        resource_handler.setDirectoriesListed(false);
        resource_handler.setResourceBase(docroot);

        HandlerList handlers = new HandlerList();
        handlers.setHandlers(
                new Handler[] {resource_handler, new DefaultHandler()});
        jettyThread.setHandler(handlers);
        jettyThread.setStopAtShutdown(true);

        try {
            jettyThread.start();
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
        }
        return jettyThread;
    }

    public static int getPort() {
        return JettyServer.port;
    }
}
