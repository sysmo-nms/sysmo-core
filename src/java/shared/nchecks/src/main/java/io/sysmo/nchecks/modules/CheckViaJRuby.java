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

package io.sysmo.nchecks.modules;

import io.sysmo.nchecks.NChecksInterface;
import io.sysmo.nchecks.Reply;
import io.sysmo.nchecks.Query;
import io.sysmo.nchecks.NChecksJRuby;
import org.jruby.embed.ScriptingContainer;

import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

public class CheckViaJRuby implements NChecksInterface //, NHelperInterface
{
    private static Logger logger = LoggerFactory.getLogger(CheckViaJRuby.class);

    public Reply execute(Query query)
    {
        String rbScript = "undefined";
        String script;
        try {
            rbScript = query.get("check_id").asString();
            script = NChecksJRuby.getInstance().getScript(rbScript);
        } catch (Exception e) {
            CheckViaJRuby.logger.error(e.toString());
            return CheckViaJRuby.handleError(
                    "Script not found: " + rbScript, e);
        }

        ScriptingContainer container = new ScriptingContainer();
        Object receiver = container.runScriptlet(script);
        Reply rep;
        try {
            rep = container.callMethod(receiver,"check",query,Reply.class);
        } catch(Exception e) {
            CheckViaJRuby.logger.error(e.toString());
            return CheckViaJRuby.handleError(
                    "Script execution failure: " + rbScript, e);
        }
        return rep;
    }

    /*
    public NHelperReply callHelper(Query query, String id)
    {
        // TODO implement helper for ruby
    }
    */

    private static Reply handleError(String txt, Exception e) {
        String msg = e.getMessage();
        CheckViaJRuby.logger.error(e.toString());
        Reply reply = new Reply();
        reply.setStatus(Reply.STATUS_ERROR);
        reply.setReply("CheckViaJRuby ERROR: " + txt + msg);
        return reply;
    }
}
