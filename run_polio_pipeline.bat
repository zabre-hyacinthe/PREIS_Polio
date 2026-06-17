@echo off

cd /d D:\PREIS_Polio_FV

if not exist logs mkdir logs

echo START %date% %time% > D:\PREIS_Polio_FV\logs\task_scheduler_test.log

"C:\PROGRA~1\R\R-45~1.2\bin\x64\Rscript.exe" "D:\PREIS_Polio_FV\R\110_run_polio_production_pipeline.R" >> D:\PREIS_Polio_FV\logs\task_scheduler_test.log 2>&1

echo END %date% %time% >> D:\PREIS_Polio_FV\logs\task_scheduler_test.log