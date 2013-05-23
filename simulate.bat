@echo off

for /F "usebackq tokens=1,2 delims==" %%i in (`wmic os get LocalDateTime /VALUE 2^>NUL`) do if '.%%i.'=='.LocalDateTime.' set ldt=%%j
set ldt=%ldt:~0,4%-%ldt:~4,2%-%ldt:~6,2%-%ldt:~8,2%-%ldt:~10,2%-%ldt:~12,2%
mkdir out\%ldt%

echo Running Monte Carlo simulation of electron beam in Nitrogen
echo.

runner.exe < input.in

move /Y *.dat out\%ldt%\ > NUL
copy /Y input.in out\%ldt%\ > NUL

echo.
echo Output files are located in in out\%ldt%
echo.

pause
