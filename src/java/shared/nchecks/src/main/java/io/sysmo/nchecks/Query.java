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
import java.util.Map;
import java.util.HashMap;

/**
* The Reply class contain all the values and informations related to the execution of 
* a module implementing NCheckInterface (a check module).
*/
public class Query
{
    private Map<String,Argument> arguments;
    private byte[]               state;

    public Query(Map<String,Argument> args, byte[] state)
    {
        this.state      = state.clone();
        this.arguments  = new HashMap<>(args);
    }

    public Query(Map<String,Argument> args)
    {
        this.arguments = new HashMap<>(args);
    }


    /*
    * Retrieve the state set by the previous check call.
    * (see Reply.setState)
    * Deserializtaion example:
    * DESERIALIZATION EXAMPLE
    *       ByteArrayInputStream b = new ByteArrayInputStream(opaqueData);
    *       ObjectInputStream o = new ObjectInputStream(b);
    *       Object myObject = o.readObject();
    *       or beter
    *       MyObjectClass = (MyObjectClass) o.readObject();
    */
    public byte[] getState() {return this.state.clone();}

    /*
    * Return the argument corresponding to the key.
    */
    public Argument get(String key) {return this.arguments.get(key);}

}
