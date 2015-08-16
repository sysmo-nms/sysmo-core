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

public class Argument
{
    public static final int STRING_VALUE  = 1;
    public static final int INT_VALUE     = 2;

    private String   argumentString;
    private int      argumentInteger;
    private int      type;

    public void set(int val) {
        this.type = Argument.INT_VALUE;
        this.argumentInteger = val;
    }

    public void set(String val) {
        this.type = Argument.STRING_VALUE;
        this.argumentString = val;
    }

    public int asInteger() throws Exception,Error {
        if (type == Argument.STRING_VALUE) {
            return Integer.parseInt(this.argumentString);
        } else {
            return this.argumentInteger;
        }
    }

    public String asString() throws Exception, Error {
        if (type == Argument.INT_VALUE) {
            return Integer.toString(this.argumentInteger);
        } else {
            return this.argumentString;
        }
    }
}
