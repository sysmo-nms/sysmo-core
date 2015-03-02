"C:\Documents and Settings\dev32\Desktop\testInstall\sysmo-server\erts-6.3\bin\erlsrv.exe" ^
    add "Sysmo NMS" -comment "Sysmo server service" ^
    -stopaction "init:stop()." ^
    -onfail restart ^
    -machine "C:\Documents and Settings\dev32\Desktop\testInstall\sysmo-server\erts-6.3\bin\erl.exe" ^
    -priority high ^
    -args "-boot \"C:/Documents and Settings/dev32/Desktop/testInstall/sysmo-server/releases/0.2.1/start\" -config \"C:/Documents and Settings/dev32/Desktop/testInstall/sysmo-server/releases/0.2.1/sys\""

"C:\Documents and Settings\dev32\Desktop\testInstall\sysmo-server\erts-6.3\bin\erlsrv.exe" start "Sysmo-NMS"
