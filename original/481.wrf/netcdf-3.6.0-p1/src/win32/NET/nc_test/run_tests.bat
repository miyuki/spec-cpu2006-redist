echo on
cd ..\%1
rem We need a non-netcdf file called tests.h to pass a test.
rem Here we create one with directory contents, then delete it.
dir > tests.h
nc_test -c
nc_test
erase tests.h