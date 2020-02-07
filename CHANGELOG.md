# Changelog
All notable changes to the gtk-fortran project will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]
### Added
- Experimental `meson.build` files have been added. Meson>=0.50.999 is needed. Commands are `meson buildmeson` and `ninja -C buildmeson`. You can install the library using `sudo ninja -C buildmeson install`, but there is still some problems for installing the `.mod` files (see https://github.com/mesonbuild/meson/issues/5374).
- examples/menu2.f90: that new example should work in GTK 4 (the menu.f90 example is based on deprecated functions).
- CMake -D NO_BUILD_HL=true option to disable building the High Level sub-library (which includes PLplot and sketcher).

### Changed
- CMake>=3.4 required.
- cmake/cmake_uninstall.cmake.in: updated with the latest code from https://gitlab.kitware.com/cmake/community/-/wikis/FAQ#can-i-do-make-uninstall-with-cmake
- test.sh has been renamed alt_build_test.sh
- The master branch has been renamed gtk2.
- src/gdkevents-auto3.f90: no reason to indicate the GTK version for that file. The "3" was removed.
- src/gtk-3-fortran.pc.in: improved pkg-config file.

### Removed
- Gtkextra directory: that directory was not maintained for 9 years, the gtkextra library is not maintained anymore and is based on GTK 2. The gtksheet part was forked (https://github.com/fpaquet/gtksheet), but it is necessary to reduce the amount of work to maintain gtk-fortran. So it was removed from the gtk3 branch.
- Doxygen dependence. It was introduced at the beginning of the gtk-fortran project but never used.
- win32_install.bat: this file was last updated in 2013 and may be brokken. You should instead install MSYS2 under Windows and follow the instructions on the wiki.
- cmake/FindPlplotF95.cmake: deprecated module to find the PLplot library (does not work with PLplot>=5.11 released the 2015-04-12).
- cmake/CheckFortranSourceCompiles.cmake: this macro is included in CMake since 3.1 version.
- cmake/FindGTK3.cmake: PkgConfig is used instead.
- test.bat: a deprecated script to build gtk-fortran (GTK 2) under Windows.
- The test/ directory containing the run_all.pl script. You can use CTest instead (see the wiki).
- Deprecated functions, to be ready for GTK 4.

### Fixed
- FindGTK3.cmake: fixed GDKCONFIG issue ("missing: GTK3_GDKCONFIG_INCLUDE_DIR").
- src/usemodules.py was printing false deprecated functions alerts in the hl files of the src directory.


## [gtk-fortran 19.04] - 2019-04-24
### Added
- The `cfwrapper.py` detects the status of each function (AVAILABLE or DEPRECATED) and writes it in the `*-auto.f90` files and in `gtk-fortran-index.csv`. It will help to remove deprecated functions during the GTK 4 migration. Developers can use the `-d` argument to remove DEPRECATED functions from the library: using `make -i` will then show errors for each deprecated function used in the project.
- The `usemodules.py` script prints warnings when deprecated functions are found in Fortran files, and tries to split `USE` lines cleanly.
- A `show_versions.sh` script that shows the versions of the main tools and libraries used in gtk-fortran. Useful for gtk-fortran developers or for reporting bugs.
- A `README` file in each directory, explaining the role of each file.
- Parallel building (gtk3) using `make -j` or `make --jobs`. On some systems, like FreeBSD, the number of jobs must be given: `make -j 4` for example. By [@ChinouneMehdi](https://github.com/ChinouneMehdi).
- A video quickstart guide on the Wiki.
- A `is_UNIX_OS()` function in `gtk-sup.f90`.
- This `CHANGELOG.md` file.
- gtk-fortran can now be cited: Vincent MAGNIN, James TAPPIN, Jens HUNGER, Jerry DE LISLE, "gtk-fortran: a GTK+ binding to build Graphical User Interfaces in Fortran", Journal of Open Source Software, 4(34), 1109, 12th January 2019, https://doi.org/10.21105/joss.01109

### Changed
- gtk3 branch based on **GTK 3.24.8, GLib 2.60.0,** generated with Ubuntu 19.04 x86_64, PLplot>=5.13.
- master (GTK 2) branch based on **GTK 2.24.32, GLib 2.60.0,** generated with Ubuntu 19.04 x86_64, PLplot<=5.10.
- The `cfwrapper.py` script has been moved in the `src/cfwrapper` directory, and splitted
in several modules to ease maintenance.
- Major revision of the PLplot part (code and documentation) in the gtk3 branch: PLplot>=5.13 is now required. And it runs under MSYS2/Windows. The gtk2 branch will keep PLplot<=5.10.
- Major update of the Wiki documentation.
- New examples: `gtkzero.f90` (just a window) and `gtkhello.f90` (two buttons).
- Updated examples.
- The default branch is now gtk3. The master (gtk2) branch should not be used for new projects.
- The gtk-fortran repository URL is now https://github.com/vmagnin/gtk-fortran. The URL https://github.com/jerryd/gtk-fortran is automatically redirected to the new URL. So it is transparent to the user. You are not obliged to modify it in your git settings, but if you want, type: `git remote set-url origin git@github.com:vmagnin/gtk-fortran.git`
- Doxygen (not yet used in the project) is optional.

### Removed
- old stuff in the `plplot/` directory.
- `old-cfwrapper.py`: a previous Python 2 version of the wrapper, last modified in 2013.

### Deprecated
- `cmake/FindPlplotF95.cmake`: deprecated module to find the PLplot<=5.10 library.

### Fixed
- The `cfwrapper.py` script can now scan the `gstdio.h` and `giochannel.h` files, except for the `g_io_channel_win32_new_messages()` function which can be declared with two different parameters types.
- Less warnings in Debug mode.
- Fixed some bugs in examples.
- The PLplot examples now work under MSYS2.
- `gtkf-sketcher.f90` now works under MSYS2.


## [gtk-fortran 17.10] - 2018-05-01
### Changed
- GTK 3.22.25, GLib 2.54.1
- GTK 2.24.31, GLib 2.54.1

## [gtk-fortran 16.10] - 2017-01-09
### Changed
- GTK 3.20.9,  GLib 2.50.2
- GTK 2.24.30, GLib 2.50.2
- The code of the heart of gtk-fortran, the `cfwrapper.py` script, has been refactored and improved in order to ease maintenance. 
- CMake files have been unified in master (gtk2) and gtk3 branches.

## [gtk-fortran 16.04] - 2016-06-15
### Changed
- GTK 3.18.9,  GLib 2.48.0
- GTK 2.24.30, GLib 2.48.0

## [gtk-fortran 13.10]
### Changed
- GTK 3.10.1
- GTK 2.24.22

## [first commit] - 2011-01-10
### Added
- Creation of the github repository by [@jerryd](https://github.com/jerryd/).
