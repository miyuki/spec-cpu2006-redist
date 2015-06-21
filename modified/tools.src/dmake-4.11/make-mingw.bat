@echo off
rem
rem Batch file to build dmake on Windows with MinGW GCC
rem
rem $Id: make-mingw.bat 6361 2011-03-03 22:29:36Z cloyce $

if "%1" == "clean" (
  del /q /f /s *.o
  del /q /f /s *~
  del /q /f /s *.exe
  goto :EOF
)

echo on
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o infer.o infer.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o make.o make.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o stat.o stat.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o expand.o expand.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o dmstring.o dmstring.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o hash.o hash.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o dag.o dag.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o dmake.o dmake.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o path.o path.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o imacs.o imacs.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o sysintf.o sysintf.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o parse.o parse.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o getinp.o getinp.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o quit.o quit.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o state.o state.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o dmdump.o dmdump.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o macparse.o macparse.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o rulparse.o rulparse.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o percent.o percent.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o function.o function.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o dchdir.o win95\dchdir.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o switchar.o win95\switchar.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o dstrlwr.o msdos\dstrlwr.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o arlib.o msdos\arlib.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o dirbrk.o msdos\dirbrk.c
rem gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o tempnam.o tempnam.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o tempnam.o winnt\microsft\tempnam.c
copy NUL: dmakeroot.h
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o ruletab.o win95\microsft\ruletab.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o runargv.o msdos\runargv.c
rem gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o runargv.o winnt\microsft\vpp40\runargv.c
gcc -c -I. -Iwin95 -Iwin95\microsft -Iwin95\microsft\vpp40 -O2 -o rmprq.o msdos\rmprq.c
gcc -o dmake.exe *.o
@echo off
dir dmake.exe
