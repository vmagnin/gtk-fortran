Last update: 2022-04-14

Dependencies
================================

- A modern Fortran compiler (>= Fortran 2008 standard), for example gfortran, ifort, ifx...
- GTK and the associated development files. For 4.x use the "gtk4" branch.
- CMake 3.7 or higher and pkg-config.
- PLplot is used if available (you need the development files).

Using gtk-fortran as a fpm dependency
================================

Starting from version 4.2, gtk-fortran can be used as a [fpm](https://fpm.fortran-lang.org) dependency. You simply need to add gtk-fortran in the dependencies section of the `fpm.toml` manifest of your project:

```toml
[dependencies]
gtk-fortran = { git = "https://github.com/vmagnin/gtk-fortran.git", branch = "gtk4" }
```

See the [gtkzero_fpm example](https://github.com/vmagnin/gtkzero_fpm) (MIT license) for a demonstration.


Building & installing gtk-fortran
================================

The build install system uses `cmake`. This file gives quick instructions to install gtk-fortran. **See the [Wiki documentation](https://github.com/vmagnin/gtk-fortran/wiki#installation-and-building) for more detailed instructions.**

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

Default compiler options can be overridden, for example:

    cmake -D CMAKE_Fortran_FLAGS_RELEASE="-O2 -std=f2018" ..

The system default Fortran compiler can be overridden, for example to use the Intel ifx compiler:

    cmake -D CMAKE_Fortran_COMPILER:FILEPATH=$(which ifx) ..

**************************************************************************
Sometimes it can help to clean out the build directory and re-run `cmake`:

     cd build
     rm -r *   ## MAKE SURE YOU ARE IN THE BUILD DIRECTORY BEFORE DOING THIS
     cmake ..
**************************************************************************

Other systems (Windows, macOS, FreeBSD...)
-------

See the Wiki documentation for specific and detailed instructions.

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

Uninstalling gtk-fortran
========================

    sudo make uninstall

See https://gitlab.kitware.com/cmake/community/-/wikis/FAQ#can-i-do-make-uninstall-with-cmake

More informations
=================

See the documentation: https://github.com/vmagnin/gtk-fortran/wiki
