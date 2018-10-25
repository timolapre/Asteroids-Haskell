@echo off

:again
   cabal run
   set /p answer=Want to rerun (Y/N)?
   if /i "%answer:~,1%" EQU "Y" goto again
   if /i "%answer:~,1%" EQU "N" exit /b