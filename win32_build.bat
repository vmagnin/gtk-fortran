@echo off
REM Free Software Foundation, Inc.
REM
REM This file is part of the gtk-fortran gtk+ Fortran Interface library.
REM
REM This is free software; you can redistribute it and/or modify
REM it under the terms of the GNU General Public License as published by
REM the Free Software Foundation; either version 3, or (at your option)
REM any later version.
REM
REM This software is distributed in the hope that it will be useful,
REM but WITHOUT ANY WARRANTY; without even the implied warranty of
REM MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
REM GNU General Public License for more details.
REM
REM Under Section 7 of GPL version 3, you are granted additional
REM permissions described in the GCC Runtime Library Exception, version
REM 3.1, as published by the Free Software Foundation.
REM
REM You should have received a copy of the GNU General Public License along with
REM this program; see the files COPYING3 and COPYING.RUNTIME respectively.
REM If not, see <http://www.gnu.org/licenses/>.
REM
REM Contributed by Jens Hunger: 03/06/2013

REM WIN32 build file for GtkFortran

ECHO.|set /p =Checking for wget ... 
FOR %%X IN (wget.exe) DO (SET FOUND=%%~$PATH:X)
if defined FOUND (
  ECHO found: %FOUND%
) ELSE (
  ECHO not found
  ECHO Please download and install wget from http://gnuwin32.sourceforge.net/packages/wget.htm
  EXIT
)

ECHO.|set /p =Checking for GFortran ... 
FOR %%X IN (gfortran.exe) DO (SET FOUND=%%~$PATH:X)
IF defined FOUND (
  ECHO found: %FOUND%
) ELSE (
  ECHO not found
  ECHO Downloading MinGW from http://sourceforge.net/projects/mingw
  wget http://sourceforge.net/projects/mingw/files/Installer/mingw-get-inst/mingw-get-inst-20120426/mingw-get-inst-20120426.exe/download
  ECHO Installing MinGW
  FOR /f "tokens=*" %%G IN ('DIR /b mingw-get-inst*.exe') DO %%G
  DEL mingw-get-inst*
)

ECHO.|set /p =Checking for CMake ... 
FOR %%X IN (cmake.exe) DO (SET FOUND=%%~$PATH:X)
IF defined FOUND (
  ECHO found: %FOUND%
) ELSE (
  ECHO not found
  ECHO Downloading CMake for Windows from http://www.cmake.org/cmake/resources/software.html
  wget http://www.cmake.org/files/v2.8/cmake-2.8.10.2-win32-x86.exe
  ECHO Installing CMake
  cmake-2.8.10.2-win32-x86.exe
  DEL cmake-2.8.10.2-win32-x86.exe
)

ECHO.|set /p =Checking for GTK ... 
pkg-config --libs gtk+-3.0 > TMP.dat         
SET /p FOUND= < TMP.dat
DEL TMP.dat
IF defined FOUND (
  ECHO found: %FOUND%
) ELSE (
  ECHO not found
  ECHO Downloading GTK+ for Windows from http://www.tarnyko.net
  wget "http://www.tarnyko.net/repo/GTK+-Bundle-3.6.1_(TARNYKO).exe"
  ECHO Installing GTK
  "GTK+-Bundle-3.6.1_(TARNYKO).exe"
  DEL "GTK+-Bundle-3.6.1_(TARNYKO).exe"
)

ECHO.|set /p =Checking for sed ... 
FOR %%X IN (sed.exe) DO (SET FOUND=%%~$PATH:X)
IF defined FOUND (
  ECHO found: %FOUND%
) ELSE (
  ECHO not found
  ECHO Downloading sed from http://sourceforge.net/projects/gnuwin32
  wget http://sourceforge.net/projects/gnuwin32/files//sed/4.2.1/sed-4.2.1-setup.exe/download
  ECHO Installing sed
  sed-4.2.1-setup.exe
  DEL sed-4.2.1-setup.exe
  SET PATH=C:\Program Files\GnuWin32\bin;%PATH%
)

MKDIR build
CD build
pkg-config --cflags-only-I gtk+-3.0 > TMP.dat         
SET /p CMAKE_INCLUDE_PATH= < TMP.dat
pkg-config --libs-only-L gtk+-3.0 > TMP.dat         
SET /p CMAKE_LIBRARY_PATH= < TMP.dat
pkg-config --cflags-only-I glib-2.0 > TMP.dat         
SET /p GLIBCONFIG_INCLUDE_DIR= < TMP.dat
DEL TMP.dat
SET CMAKE_INCLUDE_PATH=%CMAKE_INCLUDE_PATH: -I=;%
SET CMAKE_INCLUDE_PATH=%CMAKE_INCLUDE_PATH:-I=%
SET CMAKE_LIBRARY_PATH=%CMAKE_LIBRARY_PATH: -L=;%
SET CMAKE_LIBRARY_PATH=%CMAKE_LIBRARY_PATH:-L=%
SET GLIBCONFIG_INCLUDE_DIR=%GLIBCONFIG_INCLUDE_DIR: -I=;%
SET GLIBCONFIG_INCLUDE_DIR=%GLIBCONFIG_INCLUDE_DIR:-I=%
cmake -DCMAKE_INCLUDE_PATH=%CMAKE_INCLUDE_PATH% -DCMAKE_LIBRARY_PATH=%CMAKE_LIBRARY_PATH% -DGTK3_GLIBCONFIG_INCLUDE_DIR=%GLIBCONFIG_INCLUDE_DIR% -G "MinGW Makefiles" ..
mingw32-make
