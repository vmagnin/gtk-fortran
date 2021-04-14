! $ gfortran -Wall -Wextra -std=f2008 -pedantic -g my_first_gtk_app1.f90 $(pkg-config --cflags --libs gtk-3-fortran)

module handlers
  use, intrinsic :: iso_c_binding
  use gtk, only:
  use g, only:

  implicit none

  contains

end module handlers

program my_first_gtk_app
  use handlers

  implicit none

end program
