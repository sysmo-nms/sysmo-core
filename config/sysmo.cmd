:: IMPORTANT: edit this file with a CRLF line ending editor.
:: This file was edited with nodepad++
@setlocal

:: set node_name
@set node_name=sysmo

:: set service_name
@set service_name=Sysmo Core

:: set service_description
@set service_description=Sysmo Core OSS

:: set node_root
@for /F "delims=" %%I in ("%~dp0..") do @set node_root=%%~fI

:: cd into node_root
@cd %node_root%

:: set releases_dir
@set releases_dir=%node_root%\releases

:: set sysmo_relutils
@set sysmo_relutils=%node_root%\bin\sysmo_relutils.exe

:: set erts_version and release_version from start_erl.data
@for /F "usebackq tokens=1,2" %%I in ("%releases_dir%\start_erl.data") do @(
    @call :set_trim erts_version    %%I
    @call :set_trim release_version %%J
)

:: set vm_args
@set vm_args=%node_root%\etc\vm.args

:: set sys_config
@set sys_config=%node_root%\etc\app.config

:: set node_boot
@set node_boot=%releases_dir%\%release_version%\%node_name%

:: set erts_bin
@set erts_bin=%node_root%\erts-%erts_version%\bin

:: write_ini
@set erl_ini=%erts_bin%\erl.ini
@set converted_bindir=%erts_bin:\=\\%
@set converted_rootdir=%node_root:\=\\%
@echo [erlang] > "%erl_ini%"
@echo Bindir=%converted_bindir% >> "%erl_ini%"
@echo Progname=erl >> "%erl_ini%"
@echo Rootdir=%converted_rootdir% >> "%erl_ini%"


@if "%1"=="install"   @goto install
@if "%1"=="start"     @goto start
@if "%1"=="stop"      @goto stop
@if "%1"=="list"      @goto list
@if "%1"=="uninstall" @goto uninstall
@if "%1"=="console"   @goto console
@if "%1"=="init_admin_pass" @goto init_sysmo_credentials

@echo Unknown command: "%1"
@goto :EOF
:: END OF PROGRAM


:install
:: WTF: All node files must have "Administrators" permissions
:: to get it work with erlsrv.exe. Console mode only requires the command
:: to be started in Administrator mode.
:: So to get it work, first install the files then use erlsrv.
@set args= -boot \"%node_boot%\" -config \"%sys_config%\" -args_file \"%vm_args%\"
@"%erts_bin%\erlsrv.exe" add "%service_name%" ^
	-stopaction "init:stop()." ^
	-onfail     restart ^
	-workdir    "%node_root%" ^
	-sname      "%node_name%" ^
	-comment    "%service_description%" ^
	-machine    "%erts_bin%\erl.exe" ^
	-args       "%args%"
@goto :EOF
:: END OF PROGRAM

:start
:: WARNING: see WTF
@"%erts_bin%\erlsrv.exe" start "%service_name%"
@goto :EOF
:: END OF PROGRAM

:stop
:: WARNING: see WTF
@"%erts_bin%\erlsrv.exe" stop "%service_name%"
@goto :EOF
:: END OF PROGRAM

:list
:: WARNING: see WTF
@"%erts_bin%\erlsrv.exe" list "%service_name%"
@goto :EOF
:: END OF PROGRAM

:uninstall
:: WARNING: see WTF
@"%erts_bin%\erlsrv.exe" stop   "%service_name%"
@"%erts_bin%\erlsrv.exe" remove "%service_name%"
@"%erts_bin%\epmd.exe" -kill
@goto :EOF
:: END OF PROGRAM

:console
@"%erts_bin%\erl.exe" ^
	-boot      "%node_boot%" ^
	-config    "%sys_config%" ^
	-args_file "%vm_args%" ^
	-sname     "%node_name%"
@goto :EOF
:: END OF PROGRAM

:init_sysmo_credentials
@set new_password=%2
@"%sysmo_relutils%" --update_cookie --update_admin_password="%2"
@goto :EOF

:set_trim
@set %1=%2
@goto :EOF
:: END OF PROGRAM
