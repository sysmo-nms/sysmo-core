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

package io.sysmo.nchecks.modules;

import io.sysmo.nchecks.NChecksInterface;
import io.sysmo.nchecks.Argument;
import io.sysmo.nchecks.Reply;
import io.sysmo.nchecks.Query;
import io.sysmo.nchecks.NChecksJRuby;
import io.sysmo.nchecks.NChecksLogger;
import org.jruby.embed.ScriptingContainer;

public class CheckViaJRuby implements NChecksInterface
{
    public Reply execute(Query query)
    {
        String rbScript = "undefined";
        String script;
        try {
            rbScript = query.get("check_id").asString();
            script = NChecksJRuby.getInstance().getScript(rbScript);
        } catch (Exception e) {
            Reply err = handleError("Script not found: " + rbScript, e);
            return err;
        }

        ScriptingContainer container = new ScriptingContainer();
        Object receiver = container.runScriptlet(script);
        Reply rep;
        try {
            rep = container.callMethod(receiver,"check",query,Reply.class);
        } catch(Exception e) {
            Reply err = handleError("Script execution failure: " + rbScript, e);
            return err;
        }
        return rep;
    }

    private static Reply handleError(String txt, Exception e) {
        String msg = e.getMessage();
        NChecksLogger.getLogger().severe(e.toString());
        Reply reply = new Reply();
        reply.setStatus(Reply.STATUS_ERROR);
        reply.setReply("CheckViaJRuby ERROR: " + txt + msg);
        return reply;
    }
}
