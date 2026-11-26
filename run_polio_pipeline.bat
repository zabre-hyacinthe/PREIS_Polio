@echo off
setlocal EnableDelayedExpansion
cd /d D:\PREIS_Polio_FV
if not exist logs mkdir logs
set "LOG=D:\PREIS_Polio_FV\logs\task_scheduler_test.log"
echo START %date% %time% > "%LOG%"

rem Find the newest installed R automatically (no hard-coded version)
set "RS="
for /f "delims=" %%D in ('dir /b /ad /o-n "C:\Program Files\R\R-*" 2^>nul') do (
  if not defined RS if exist "C:\Program Files\R\%%D\bin\Rscript.exe" set "RS=C:\Program Files\R\%%D\bin\Rscript.exe"
)
if not defined RS (
  echo ERROR: Rscript.exe not found under C:\Program Files\R >> "%LOG%"
  echo END %date% %time% >> "%LOG%"
  exit /b 1
)
echo Using %RS% >> "%LOG%"

"%RS%" "D:\PREIS_Polio_FV\R\110_run_polio_production_pipeline.R" >> "%LOG%" 2>&1
set "RC=%ERRORLEVEL%"
echo END %date% %time% exit=%RC% >> "%LOG%"
exit /b %RC%
