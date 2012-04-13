@echo off
if '%1' == '' %0 css_defaults.rc
if '%2' == '' %0 %1 css_defaults.res
brcc32 -fo%2 %1
pause
