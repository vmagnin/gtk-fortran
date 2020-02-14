# cfwrapper source files

- cfwrapper/cfwrapper.py: this is the precious heart of gtk-fortran. Developers use it to automatically parse the GTK libraries header files. It generates:
  - ../*-auto.f90: these files contains the Fortran interfaces to the C functions of the various GTK libraries.
  - ../gtk-fortran-index.csv: list of all the interfaces generated in the *-auto.f90 files, with the name of the library, the name of the function, its status (deprecated or not), the names of the .f90 file and the .h file, the C prototype of the function and the Fortran definition.
  - cfwrapper/gtk-fortran-hash.pkl: the SHA1 hash of all *-auto.f90 files. Useful for the development of cfwrapper.py to detect modifications in the resulting .f90 files.
  - cfwrapper/cfwrapper-errors.csv: for the developers, a list of problems encountered by the cfwrapper.py script when parsing the .h files. Not pushed in github.
  - ../../VERSIONS: a CSV file with the gtk-fortran, GTK, GLib and Ubuntu versions. It is used by the build system.
- Other *.py files: the modules used by cfwrapper.py.
