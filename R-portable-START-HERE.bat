@echo off
SETLOCAL

pushd %~dp0
@echo off

   SET R_PORTABLE=R-Portable\App\R-Portable\bin\x64


@echo off
SET ROPTS=--no-save --no-environ --no-init-file --no-restore --no-Rconsole
%R_PORTABLE%\Rscript.exe %ROPTS% R-portable-LandWeb.R 1> ShinyApp2.log 2>&1
:: %R_PORTABLE%\R.exe 

popd

