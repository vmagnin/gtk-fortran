Last update: 2021-08-02

Building & Installing gtk-fortran
================================

The build install system uses `cmake`. This file gives quick instructions to install gtk-fortran. See the Wiki documentation for more detailed instructions.

UNIX/Linux
----------

To do an "out of source" build from the top-level directory on a
Unix/Linux system:

    mkdir build && cd build
    cmake ..
    make
    sudo make install

If the building of some examples causes an error, you can ignore them with
the `-i` option:

    make -i

cmake variables are set by using `-D<variable>=<value>`, for example to change the default install directory from `/usr/local` to `/usr`:

    cmake -DCMAKE_INSTALL_DIR=/usr ..

Useful variables that are specific to gtk-fortran are:

       EXCLUDE_PLPLOT -- set this to disable building the plplot
         integration even if PLplot is found.
       NO_BUILD_HL -- set this to disable building the High Level sub-library 
         (includes PLplot and sketcher).
       NO_BUILD_EXAMPLES -- set this to prevent compiling the example
         programs, also mostly useful for packagers.
       INSTALL_EXAMPLES -- set this to install the source code of the
         examples into
         ${CMAKE_INSTALL_DATAROOTDIR/gtk-fortran/examples<gtkversion>,
         this would for example be useful if you were making a binary
         package of gtk-fortran.

To interactively control the build, use `ccmake` in place of `cmake`

**************************************************************************
Sometimes it can help to clean out the build directory and re-run `cmake`:

     cd build
     rm -r *   ## MAKE SURE YOU ARE IN THE BUILD DIRECTORY BEFORE DOING THIS
     cmake ..
**************************************************************************

Other systems (Windows, macOS, FreeBSD...)
-------

See the Wiki documentation for detailed instructions.


Dependencies
------------

- A modern Fortran compiler (>= Fortran 2008 standard), for example gfortran.
- GTK and the associated development files. For 4.x use the "gtk4" branch.
- CMake >= 3.4 or better and pkg-config.
- PLplot is used if available (you need the development files).


Known issues
------------

You can see or post issues on this page:

https://github.com/vmagnin/gtk-fortran/issues

Building your application
=========================

On Linux and Unix systems the build system generates a pkg-config file
and installs it. So building a single source file application should be
as simple as:

    gfortran my_app.f90 $(pkg-config --cflags --libs gtk-4-fortran)

If you have made a default install to `/usr/local` you *may* need to run:

    export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig

(Or

    setenv PKG_CONFIG_PATH /usr/local/lib/pkgconfig

if you use csh or one of its derivatives)

this will depend on your distribution, Ubuntu looks there by default,
Pardus and Manjaro don't.

Uninstalling GtkFortran
=======================

    sudo make uninstall

See https://gitlab.kitware.com/cmake/community/-/wikis/FAQ#can-i-do-make-uninstall-with-cmake

More informations
=================

See the documentation: https://github.com/vmagnin/gtk-fortran/wiki
