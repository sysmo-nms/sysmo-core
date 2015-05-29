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

package io.sysmo.nchecks;
import io.sysmo.nchecks.NChecksLogger;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;

public class NChecksJRuby {
    private String scriptPath;
    private HashMap<String,String> smap;

    private static NChecksJRuby INSTANCE;
    public static NChecksJRuby getInstance() {return INSTANCE;}
    public static void startJRuby(String scriptPath) {
        new NChecksJRuby(scriptPath);
    }

    private NChecksJRuby(String scriptDir) {
        NChecksLogger.getLogger().info("init path: " + scriptPath);
        scriptPath = scriptDir;
        smap = new HashMap<String,String>();
        INSTANCE = this;
        NChecksLogger.getLogger().info("JRuby init with path: " + scriptPath);
    }

    public String getScript(String identifier) throws Exception {
        synchronized (smap) {
            String val = smap.get(identifier);
            if(val == null) {
                String script = identifier + ".rb";
                byte[] fbytes = Files.readAllBytes(Paths.get(scriptPath,script));
                val = new String(fbytes);
                smap.put(script,val);
                return val;
            } else {
                return val;
            }
        }
    }
}
