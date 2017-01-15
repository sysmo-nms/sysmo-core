/*
 * Sysmo NMS Network Management and Monitoring solution (http://www.sysmo.io)
 *
 * Copyright (c) 2012-2017 Sebastien Serre <ssbx@sysmo.io>
 *
 * Sysmo NMS is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * Sysmo NMS is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * Sysmo.  If not, see <http://www.gnu.org/licenses/>.
 */
package main

import "fmt"
import "flag"
import "bufio"
import "os"
import "log"
import "strings"
import "math/rand"
import "time"
import "encoding/xml"
import "bytes"
import "io"
import "path/filepath"

var UpdateCookie bool
var UpdateAdminPassword bool
var UpdateAdminPasswordVar string

const cookie_size int = 25

var pass_file string
var args_file string
var tmp_args_file string
var tmp_pass_file string

type XmlUsers struct {
    XMLName xml.Name `xml:"xml_users"`
    Users   []User   `xml:"user"`
}

type User struct {
    XMLName xml.Name `xml:"user"`
    Id string      `xml:"Id,attr"`
    Passwd string  `xml:"Password,attr"`
    Groups []Group `xml:"group"`
}

type Group struct {
    XMLName xml.Name `xml:"group`
    Id      string   `xml:"Id,attr"`
}

func init() {
    flag.BoolVar(&UpdateCookie, "update_cookie", false, "Set a random cookie")
    flag.BoolVar(&UpdateAdminPassword, "update_admin_password", false,
        "Update the admin password. Requires --admin_password_string.")
    flag.StringVar(&UpdateAdminPasswordVar, "admin_password_string", "",
        "The new password to be set")
}

func generate_cookie() string {
    rand.Seed(time.Now().UnixNano())
    letterRunes := []rune("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

    b := make([]rune, cookie_size)
    for i := range b {
        b[i] = letterRunes[rand.Intn(len(letterRunes))]
    }
    return string(b)
}

func update_cookie() {

    file, err := os.Open(args_file)
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    tmp_file, tmp_err := os.Create(tmp_args_file)
    if tmp_err != nil {
        log.Fatal(err)
    }
    defer tmp_file.Close()

    scanner := bufio.NewScanner(file)
    cookie_updated := false
    for scanner.Scan() {
        text := scanner.Text()
        if strings.HasPrefix(text, "-setcookie") {
            new_cookie := generate_cookie()
            _, tmp_err = tmp_file.WriteString("-setcookie " + new_cookie + "\n")
            cookie_updated = true
        } else {
            _, tmp_err = tmp_file.WriteString(text + "\n")
        }
        if tmp_err != nil {
            log.Fatal(tmp_err)
        }
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }

    if cookie_updated == false {
        new_cookie := generate_cookie()
        _, tmp_err = tmp_file.WriteString("-setcookie " + new_cookie + "\n")
        if tmp_err != nil {
            log.Fatal(tmp_err)
        }
    }

    file.Close()
    tmp_file.Close()

    os.Rename(tmp_args_file, args_file)

    fmt.Println("Cookie at " + args_file + " is updated")
}

func update_admin_password() {
    xmlFile, err := os.Open(pass_file)
    if err != nil {
        log.Fatal(err)
    }
    defer xmlFile.Close()
    buf := bytes.NewBuffer(nil)
    io.Copy(buf, xmlFile)
    xmlFile.Close()

    v := XmlUsers{}
    err = xml.Unmarshal(buf.Bytes(), &v)
    if err != nil {
        log.Fatal(err)
    }

    for i := range v.Users {
        user := &v.Users[i]
        if user.Id == "admin" {
            user.Passwd = UpdateAdminPasswordVar
            break
        }
    }

    output, err2 := xml.MarshalIndent(&v, "", "    ")

    if err2 != nil {
        fmt.Println("xml encode error")
        log.Fatal(err2)
    }

    tmp_file, err := os.Create(tmp_pass_file)
    if err != nil {
        log.Fatal(err)
    }
    defer tmp_file.Close()

    _, err = tmp_file.Write(output)
    if err != nil {
        log.Fatal(err)
    }

    tmp_file.Close()

    os.Rename(tmp_pass_file, pass_file)

    fmt.Println("Users file " + pass_file + " updated")
}

func main() {
    current_dir, err := filepath.Abs(filepath.Dir(os.Args[0]))
    if err != nil {
        log.Fatal(err)
    }
    parent_dir := filepath.Dir(current_dir)
    etc_dir := filepath.Join(parent_dir, "etc")

    pass_file := filepath.Join(etc_dir, "users.xml")
    args_file := filepath.Join(etc_dir, "vm.args")
    tmp_pass_file := filepath.Join(etc_dir, "users.xml.tmp")
    tmp_args_file := filepath.Join(etc_dir, "vm.args.tmp")

    flag.Parse()
    if (UpdateCookie == false) && (UpdateAdminPassword == false) {
        fmt.Println("Correct usage can be:")
        flag.PrintDefaults()
        os.Exit(1)
    }

    if UpdateCookie == true {
       update_cookie()
    }

    if UpdateAdminPassword == true {
       if len(UpdateAdminPasswordVar) > 0 {
          update_admin_password()
       }
    }
}
