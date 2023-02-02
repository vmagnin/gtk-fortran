! Copyright (C) 2011
! Free Software Foundation, Inc.

! This file is part of the gtk-fortran gtk+ Fortran Interface library.

! This is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3, or (at your option)
! any later version.

! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! Under Section 7 of GPL version 3, you are granted additional
! permissions described in the GCC Runtime Library Exception, version
! 3.1, as published by the Free Software Foundation.

! You should have received a copy of the GNU General Public License along with
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Contributed by Jerry DeLisle and Vincent Magnin
! Last modification: vmagnin, 02-19-2019
! gfortran -g gtk.f90 mandelbrot.f90 `pkg-config --cflags --libs gtk+-2.0`

module handlers
  use iso_c_binding, only: c_int
  use gtk, only: gtk_container_add, gtk_events_pending, gtk_image_new, gtk_image_&
  &new_from_image, gtk_main, gtk_main_iteration, gtk_main_iteration_do, gtk_widge&
  &t_map, gtk_widget_show, gtk_widget_unmap, gtk_window_new, gtk_window_set_defau&
  &lt, gtk_window_set_default_size, gtk_window_set_title,&
  &FALSE, TRUE, c_null_char, c_null_ptr, GTK_WINDOW_TOPLEVEL, GDK_IMAGE_FASTEST,&
  &gtk_init, g_signal_connect, gtk_main_quit
  use gdk, only: gdk_image_new, gdk_image_put_pixel, gdk_rgb_get_visual

  implicit none

  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult

contains
  ! User defined event handlers go here
  function delete_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata
    !print *, "Delete_event"
    run_status = FALSE
    ret = FALSE
    call gtk_main_quit()
  end function delete_event

  subroutine pending_events ()
    do while(IAND(gtk_events_pending(), run_status) /= FALSE)
      boolresult = gtk_main_iteration_do(FALSE) ! False for non-blocking
    end do
  end subroutine pending_events

end module handlers

program mandelbrot
  use iso_c_binding, only: c_ptr, c_null_char, c_null_ptr, c_funloc
  use handlers
  implicit none
  type(c_ptr) :: my_window, my_gdk_image, my_gtk_image

  call gtk_init ()

  ! Properties of the main window :
  my_window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  call gtk_window_set_default_size(my_window, 600_c_int, 600_c_int)
  call gtk_window_set_title(my_window, "A tribute to Benoit MANDELBROT (1924-2010)"//c_null_char)
  call g_signal_connect (my_window, "delete-event"//c_null_char, &
       & c_funloc(delete_event))

  my_gdk_image = gdk_image_new(GDK_IMAGE_FASTEST, gdk_rgb_get_visual(), &
       & 600_c_int, 600_c_int)
  my_gtk_image = gtk_image_new_from_image(my_gdk_image, c_null_ptr)    
  call gtk_container_add(my_window, my_gtk_image)
  call gtk_widget_show (my_gtk_image)

  call gtk_widget_show (my_window)

  call Mandelbrot_set(my_gdk_image, my_gtk_image, 600_c_int, 600_c_int, &
       & -2d0, +1d0, -1.5d0, +1.5d0, 1000_c_int*10_c_int)

  ! The window stays opened after the computation
  ! Main loop:
  call gtk_main()
  print *, "All done"

contains

!*********************************************
! A tribute to Benoit MANDELBROT (1924-2010)
! http://en.wikipedia.org/wiki/Mandelbrot_set
!*********************************************
subroutine Mandelbrot_set(the_gdk_image, the_gtk_image, width, height, xmin, xmax, ymin, ymax, itermax)
  ! Whole set: xmin=-2d0, xmax=+1d0, ymin=-1.5d0, ymax=+1.5d0, itermax=1000
  ! Seahorse valley:  around x=-0.743643887037151, y=+0.13182590420533, itermax=5000
  use iso_c_binding, only: c_ptr, c_int
  use handlers
  implicit none

  type(c_ptr), intent(in)    :: the_gdk_image, the_gtk_image
  integer(c_int), intent(in) :: width   ! pixels
  integer(c_int), intent(in) :: height  ! pixels
  integer(4) :: i, j, k, itermax
  real(8)    :: x, y, xmin, xmax, ymin, ymax ! coordinates in the complex plane
  complex(8) :: c, z   
  real(8)    :: scx, scy             ! scales
  integer(1) :: red, green, blue     ! rgb color

  scx = ((xmax-xmin)/width)   ! x scale
  scy = ((ymax-ymin)/height)  ! y scale

  do i=0, width-1
    call gtk_widget_unmap (the_gtk_image)
    call gtk_widget_map (the_gtk_image)

    x = xmin + scx * i
    do j=0, height-1
      y = ymin + scy * j
      c = x + y*(0d0,1d0)       ! Starting point
      z = (0d0, 0d0)            ! z0
      k = 1
      do while ((k <= itermax) .and. ((real(z)**2+aimag(z)**2)<4d0)) 
        z = z*z+c
        k = k+1
      end do

      if (k>itermax) then
        ! Black pixel:
        call gdk_image_put_pixel(the_gdk_image, i, j, &
             & int(0*65536+0*256+0,c_int))
      else
        red   = int(min(255, k*2),  kind=1)
        green = int(min(255, k*5),  kind=1)
        blue  = int(min(255, k*10), kind=1)
        call gdk_image_put_pixel(the_gdk_image, i, j, &
             & int(red*65536+green*256+blue,c_int))
      end if
      ! This subrountine processes gtk events as needed during the computation.
      call pending_events()
      if (run_status == FALSE) return ! Exit if we had a delete event.
    end do
  end do
end subroutine mandelbrot_set

end program mandelbrot 
