Last update: 02-25-2019

Building & Installing GtkFortran
================================

The build install system uses 'cmake'.

UNIX/Linux
----------

To do an "out of source" build from the top-level directory on a
Unix/Linux system: 
    mkdir build && cd build
    cmake ..
    make
    sudo make install

If the building of some examples causes an error, you can ignore them with
the -i option:
    make -i

cmake variables are set by using -D<variable>=<value>
for example to change the default install directory from /usr/local to /usr:
cmake -DCMAKE_INSTALL_DIR=/usr ..

Useful variables that are specific to gtk-fortran are:
       EXCLUDE_PLPLOT -- set this to disable building the plplot
         integration even if plplot is found
       INSTALL_EXAMPLES -- set this to install the source code of the
         examples into
         ${CMAKE_INSTALL_DATAROOTDIR/gtk-fortran/examples<gtkversion>,
         this would for example be useful if you were making a binary
         package of gtk-fortran.
       NO_BUILD_EXAMPLES -- set this to prevent compiling the example
         programs, also mostly useful for packagers.

To interactively control the build, use 'ccmake' in place of 'cmake'

**************************************************************************
* Sometimes it can help to clean out the build directory and re-run cmake:
*     cd build
*     rm -r *   ## MAKE SURE YOU ARE IN THE BUILD DIRECTORY BEFORE
*     	    	## DOING THIS
*     cmake ..
**************************************************************************

Windows
-------
Use MSYS2-MINGW64 and use same commands as under Linux, except:
cmake -G "MSYS Makefiles" -D EXCLUDE_PLPLOT=Y .. 


Dependencies:
-------------

You need a Fortran compiler with the ISO_C_BINDING module, i.e. compliant with 
the Fortran 2003 standard, for example gfortran>=4.6.

gtk-fortran needs GTK>=2.24 and the associated development files.
For 2.xx use the "master" git banch, for 3.x use "gtk3".

The build system needs CMake 2.8.5 or better.

PLplot is used if available (you need the development files).

GTK3:
-----

For the GTK 3 version, a FindGTK3.cmake module is included in the
distribution, it is placed in the cmake subdirectory which should be
added to the module search path by the top-level CMakeLists.txt rules.

N.B. FindGTK3.cmake has been tested on Debian Sid, Ubuntu and
Manjaro, it is possible that other systems may have different naming
conventions (If you have problems look for the lines with several
asterisks in the comments, as these are the places where I think there
could be issues [i.e. where I was guessing until it worked]).

Known issues
------------

On Gtk3, there is a cmake warning:

    -- Some or all of the gtk libraries were not found. (missing:  GTK3_GDKCONFIG_INCLUDE_DIR) 
that appears harmless and is probably a failing of the FindGTK3.cmake file.

You can see or post issues on this page:
https://github.com/vmagnin/gtk-fortran/issues

Building your application
=========================

On Linux and Unix systems the build system generates a pkg-config file
and installs it. So building a single source file application should be
as simple as:

    gfortran my_app.f90 `pkg-config --cflags --libs gtk-fortran`

If you have made a default install to /usr/local you *may* need to run:

    export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
(Or
    setenv PKG_CONFIG_PATH /usr/local/lib/pkgconfig
if you use csh or one of its derivatives)

this will depend on your distribution, Ubuntu looks there by default,
Pardus and Manjaro don't.

More informations
=================

See the documentation: https://github.com/vmagnin/gtk-fortran/wiki
