@echo off
rem Starts the server in its own window, then the client here.
cd /d "%~dp0"
start "Login server" dotnet fsi Server.fsx
echo Waiting for the server to start...
timeout /t 8 /nobreak >nul
dotnet fsi Client.fsx
pause
