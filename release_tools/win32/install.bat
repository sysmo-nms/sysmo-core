@ECHO OFF
setlocal EnableDelayedExpansion

SET VAR_DIR=var\
SET LOG_DIR=log\
SET SSL_DIR=ssl\
SET CONFIG_DIR=cfg\
SET RRDTOOL_PATH=c:\rrdtool\rrdtool.exe
SET ROOT_DIR=c:\noctopus\
:: Later, use the NSIS ReplaceInFile function
SET SrcFile=sys.config.src
SET FirstPassFile=firstPass.txt
SET SecondPassFile=secondPass.txt
SET ThirdPassFile=thirdPass.txt
SET FourthPassFile=fourthPass.txt
SET FifthPassFile=fifthPass.txt
SET FinalFile=sys.config

:: Substitute the %ROOT_DIR% pattern.
FOR /F "tokens=* delims==" %%a IN (%SrcFile%) DO (

	SET TextLine=%%a
	ECHO !TextLine:%%ROOT_DIR%%=%ROOT_DIR%! >> %FirstPassFile%
)

:: Substitute the %CONFIG_DIR% pattern.
FOR /F "tokens=* delims==" %%b IN (%FirstPassFile%) DO (

	SET TextLine=%%b
	ECHO !TextLine:%%CONFIG_DIR%%=%CONFIG_DIR%! >> %SecondPassFile%
) 

:: Substitute the %VAR_DIR% pattern.
FOR /F "tokens=* delims==" %%c IN (%SecondPassFile%) DO (

	SET TextLine=%%c
	ECHO !TextLine:%%VAR_DIR%%=%VAR_DIR%! >> %ThirdPassFile%
)

:: Substitute the %RRDTOOL_PATH% pattern.
FOR /F "tokens=* delims==" %%d IN (%ThirdPassFile%) DO (

	SET TextLine=%%d
	ECHO !TextLine:%%RRDTOOL_PATH%%=%RRDTOOL_PATH%! >> %FourthPassFile%
)

:: Substitute the %LOG_DIR% pattern.
FOR /F "tokens=* delims==" %%e IN (%FourthPassFile%) DO (

	SET TextLine=%%e
	ECHO !TextLine:%%LOG_DIR%%=%LOG_DIR%! >> %FifthPassFile%
)

:: Substitute the %SSL_DIR% pattern.
FOR /F "tokens=* delims==" %%f IN (%FifthPassFile%) DO (

	SET TextLine=%%f
	ECHO !TextLine:%%SSL_DIR%%=%SSL_DIR%! >> %FinalFile%
)