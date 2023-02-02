# gtk-fortran

**This branch is for GTK 4**

The gtk-fortran project aims to offer scientists programming in Fortran a cross-platform library to build Graphical User Interfaces (GUI). Gtk-fortran is a partial GTK / Fortran binding 100% written in Fortran, thanks to the ISO_C_BINDING module for interoperability between C and Fortran, which is a part of the Fortran 2003 standard.

To install gtk-fortran, you can follow the instructions in the INSTALL file or the more detailed instructions of the project wiki:
[https://github.com/vmagnin/gtk-fortran/wiki](https://github.com/vmagnin/gtk-fortran/wiki)

Please post bugs on GitHub:
[https://github.com/vmagnin/gtk-fortran/issues](https://github.com/vmagnin/gtk-fortran/issues)


# Files in this directory

* README.md: the present file.
* CHANGELOG.md: list of the releases with main changes.
* CITATION.cff: file used by the GitHub interface (button "Cite this repository").
* CMakeLists.txt: main CMake instructions to build the project.
* codemeta.json: metadata about the project.
* fpm.toml: Fortran Package Manager manifest.
* INSTALL: quick installation instructions (see the Wiki for more details).
* LICENSE: text of the GNU GPL v3 license.
* LICENSE_EXCEPTION: text of the GCC Runtime Library Exception version 3.1.
* README-high-level: about the High Level part of the gtk-fortran library.
* VERSIONS: a CSV file with the gtk-fortran, GTK, GLib and distribution versions.


# Citing gtk-fortran

Please acknowledge the use of gtk-fortran by citing the following publication:

Vincent MAGNIN, James TAPPIN, Jens HUNGER, Jerry DE LISLE, "gtk-fortran: a GTK+ binding to build Graphical User Interfaces in Fortran", _Journal of Open Source Software,_ 4(34), 1109, 12th January 2019, [https://doi.org/10.21105/joss.01109](https://doi.org/10.21105/joss.01109)
