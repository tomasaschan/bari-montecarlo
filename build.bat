@echo off

echo Building program...

make

echo Creating distributable zip archive...

del mc.zip

7z a mc.zip runner.exe input.in simulate.bat data

echo All done!

pause