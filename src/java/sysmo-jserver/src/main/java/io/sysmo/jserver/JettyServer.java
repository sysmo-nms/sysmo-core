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
 * along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
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

/**
 * Created by seb on 21/08/15.
 */
public class JettyServer
{
    private static Logger logger = LoggerFactory.getLogger(JettyServer.class);

    public static Server startServer()
    {
        Server jettyThread = new Server();
        SelectChannelConnector connector = new SelectChannelConnector();
        connector.setPort(8080);
        jettyThread.addConnector(connector);

        ResourceHandler resource_handler = new ResourceHandler();
        resource_handler.setDirectoriesListed(false);
        resource_handler.setResourceBase("./docroot");

        HandlerList handlers = new HandlerList();
        handlers.setHandlers(
                new Handler[] {resource_handler, new DefaultHandler()});
        jettyThread.setHandler(handlers);
        jettyThread.setStopAtShutdown(true);

        try {
            jettyThread.start();
        } catch (Exception e) {
            logger.error(e.toString());
        }
        return jettyThread;
    }
}
