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
REM Contributed by Jens Hunger: 03/05/2013

REM WIN32 build file for GtkFortran

ECHO.|set /p =Checking for wget ... 
FOR %%X IN (wget.exe) DO (SET FOUND=%%~$PATH:X)
if defined FOUND (
  ECHO found: %FOUND%
) ELSE (
  ECHO not found
  ECHO Please download and install wget from http://gnuwin32.sourceFORge.net/packages/wget.htm
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
  RM mingw-get-inst*
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
  RM cmake-2.8.10.2-win32-x86.exe
)

ECHO.|set /p =Checking for GTK ... 
FOR %%X IN (gtk+-2.0.pc) DO (SET FOUND=%%~$PKG_CONFIG_PATH:X)
IF defined FOUND (
  echo found: %FOUND%
) ELSE (
  ECHO not found
  ECHO.|set /p =Checking for unzip ... 
  FOR %%X IN (unzip.exe) DO (SET FOUND=%%~$PATH:X)
  IF defined FOUND (
    ECHO found: %FOUND%
  ) ELSE (
    ECHO not found
    ECHO Downloading unzip from http://sourceforge.net/projects/gnuwin32
    wget http://sourceforge.net/projects/gnuwin32/files/unzip/5.51-1/unzip-5.51-1.exe/download
    ECHO Installing unzip
    unzip-5.51-1.exe
    RM unzip-5.51-1.exe
    SET PATH=%PATH%;C:\Program Files\GnuWin32\bin
  )
  ECHO Downloading GTK+ for Windows from http://ftp.gnome.org
  wget http://ftp.gnome.org/pub/gnome/binaries/win32/gtk+/2.24/gtk+-bundle_2.24.10-20120208_win32.zip
  ECHO Installing GTK
  MKDIR c:\gtk
  MV gtk+-bundle_2.24.10-20120208_win32.zip c:\gtk
  unzip c:\gtk\gtk+-bundle_2.24.10-20120208_win32.zip
  RM c:\gtk\gtk+-bundle_2.24.10-20120208_win32.zip
  SET PATH=%PATH%;c:\gtk
  SET PKG_CONFIG_PATH=%PKG_CONFIG_PATH%;C:\gtk\lib\pkgconfig
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
  RM sed-4.2.1-setup.exe
  SET PATH=%PATH%;C:\Program Files\GnuWin32\bin
)

MKDIR build
CD build
cmake -DCMAKE_INCLUDE_PATH=C:/gtk/include;C:/gtk/lib/glib-2.0/include;C:/gtk/lib/gtk-2.0/include -DCMAKE_LIBRARY_PATH=C:/gtk/lib -G "MinGW Makefiles" ..
mingw32-make
