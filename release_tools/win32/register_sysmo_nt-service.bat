"C:\Documents and Settings\seb\Desktop\testInstall\sysmo-server\erts-5.10.4\bin\erlsrv.exe" ^
    add "Noctopus NMS" -comment "Noctopus server service" ^
    -stopaction "init:stop()." ^
    -onfail restart ^
    -machine "C:\Documents and Settings\seb\Desktop\testInstall\sysmo-server\erts-5.10.4\bin\erl.exe" ^
    -priority high ^
    -args "-boot \"C:/Documents and Settings/seb/Desktop/testInstall/sysmo-server/releases/0.2.1/start\" -config \"C:/Documents and Settings/seb/Desktop/testInstall/sysmo-server/releases/0.2.1/sys\""

"C:\Documents and Settings\seb\Desktop\testInstall\sysmo-server\erts-5.10.4\bin\erlsrv.exe" start "Noctopus-NMS"
