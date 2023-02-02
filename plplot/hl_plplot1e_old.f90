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
! gfortran hl_plplot1e.f90 `pkg-config --cflags --libs gtk-fortran plplotd-f95`
! Contributed by: James Tappin
! PLplot code derived from PLplot's example 1 by Alan W. Irwin

module common_ex1
  use iso_c_binding
  use gtk, only: gtk_button_new, gtk_container_add, gtk_drawing_area&
       &_new, gtk_main, gtk_main_quit, &
       & gtk_widget_show, gtk_widget_show_all, gtk_window_new, gtk_init
  use g, only: g_object_get_data

  use gtk_draw_hl

  use plplot_extra

  integer(kind=c_int) :: height, width
  type(c_ptr) :: window
end module common_ex1

module plplot_code_ex1
  use plplot, PI => PL_PI
  use iso_c_binding
  use common_ex1

  implicit none

  real(plflt) :: xscale, yscale, xoff, yoff

contains
  subroutine x01f95(area)

    type(c_ptr), intent(in) :: area

    type(c_ptr) :: cc

    character(len=80) :: version
    character(len=20) :: geometry
    integer :: digmax

    ! Define colour map 0 to match the "GRAFFER" colour table in
    ! place of the PLPLOT default.
    integer, parameter, dimension(16) :: rval = (/255, 0, 255, &
         & 0, 0, 0, 255, 255, 255, 127, 0, 0, 127, 255, 85, 170/),&
         & gval = (/ 255, 0, 0, 255, 0, 255, 0, 255, 127, 255, 255, 127,&
         & 0, 0, 85, 170/), &
         & bval = (/ 255, 0, 0, 0, 255, 255, 255, 0, 0, 0, 127, 255, 255,&
         & 127, 85, 170/)

    !  Process command-line arguments
    call plparseopts(PL_PARSE_FULL)

    !  Print plplot version
    call plgver(version)
    write (*,'(a,a)') 'PLplot library version: ', trim(version)

    ! Get a cairo context from the drawing area.

    cc = hl_gtk_drawing_area_cairo_new(area)

    !  Initialize plplot
    call plscmap0(rval, gval, bval)
    call plsdev("extcairo")

    ! By default the "extcairo" driver does not reset the background
    ! This is equivalent to the command line option "-drvopt set_background=1"
    call plsetopt("drvopt", "set_background=1")  

    ! The "extcairo" device doesn't read the size from the context.
    write(geometry, "(I0,'x',I0)") width, height
    call plsetopt("geometry",  geometry)
 
    !  Divide page into 2x2 plots
    call plstar(2,2)

    ! Tell the "extcairo" driver where the context is located. This must be
    ! done AFTER the plstar or plinit call.

    call pl_cmd(PLESC_DEVINIT, cc)

    !  Set up the data
    !  Original case

    xscale = 6._plflt
    yscale = 1._plflt
    xoff = 0._plflt
    yoff = 0._plflt

    !  Do a plot
    call plot1()

    !  Set up the data

    xscale = 1._plflt
    yscale = 0.0014_plflt
    yoff = 0.0185_plflt

    !  Do a plot

    digmax = 5
    call plsyax(digmax,  0)
    call plot1()

    call plot2()
    call plot3()

    !  Don't forget to call PLEND to finish off, and then delete the
    !  cairo context.

    call plend()
    call hl_gtk_drawing_area_cairo_destroy(cc)

  end subroutine x01f95

  !======================================================================
  subroutine plot1()

    real(plflt), dimension(1:60) :: x, y
    real(plflt), dimension(1:6)  :: xs, ys
    real(plflt) :: xmin, xmax, ymin, ymax 
    integer :: i

    do i = 1, 60
       x(i) = xoff + xscale * dble(i)/60.0_plflt
       y(i) = yoff + yscale * x(i)**2
    enddo

    xmin = x(1)
    xmax = x(60)
    ymin = y(1)
    ymax = y(60)

    do i = 1, 6
       xs(i) = x((i-1)*10+4)
       ys(i) = y((i-1)*10+4)
    enddo

    !   Set up the viewport and window using PLENV. The range in X is
    !   0.0 to 6.0, and the range in Y is 0.0 to 30.0. The axes are
    !   scaled separately (just = 0), and we just draw a labelled
    !   box (axis = 0).

    call plcol0(1)
    call plenv( xmin, xmax, ymin, ymax, 0, 0 )
    call plcol0(2)
    call pllab( '(x)', '(y)', '#frPLplot Example 1 - y=x#u2' )

    !   Plot the data points

    call plcol0(4)
    call plpoin( xs, ys, 9 )

    !   Draw the line through the data

    call plcol0(3)
    call plline( x, y )

  end subroutine plot1

  !======================================================================
  subroutine plot2()

    real(plflt), dimension(1:100) :: x, y
    integer :: i

    !
    !   Set up the viewport and window using PLENV. The range in X is
    !   -2.0 to 10.0, and the range in Y is -0.4 to 2.0. The axes are
    !   scaled separately (just = 0), and we draw a box with axes
    !   (axis = 1).

    call plcol0(1)
    call plenv(-2.0_plflt, 10.0_plflt, -0.4_plflt, 1.2_plflt, 0, 1 )
    call plcol0(2)
    call pllab( '(x)', 'sin(x)/x', '#frPLplot Example 1 - Sinc Function' )

    !   Fill up the arrays

    do i = 1, 100
       x(i) = (i-20.0_plflt)/6.0_plflt
       y(i) = 1.0_plflt
       if (x(i) .ne. 0.0_plflt) y(i) = sin(x(i)) / x(i)
    enddo

    !   Draw the line

    ! For plplot 5.9.9 or lower comment out the plwidth calls in the
    ! remainder of this subroutine.
    ! For plplot 5.9.10 or higher comment out the plwid calls.

    call plcol0(3)
    call plwid(2)
!    call plwidth(2.0_plflt)
    call plline( x, y )
    call plwid(1)
!    call plwidth(1.0_plflt)

  end subroutine plot2

  !======================================================================
  subroutine plot3()

    !
    !   For the final graph we wish to override the default tick intervals,
    !   and so do not use_ PLENV

    real(plflt), dimension(1:101) :: x, y

    integer i
    call pladv(0)

    !   Use_ standard viewport, and define X range from 0 to 360 degrees,
    !   Y range from -1.2 to 1.2.

    call plvsta()
    call plwind( 0.0_plflt, 360.0_plflt, -1.2_plflt, 1.2_plflt )

    !   Draw a box with ticks spaced 60 degrees apart in X, and 0.2 in Y.

    call plcol0(1)
    call plbox( 'bcnst', 60.0_plflt, 2, 'bcnstv', 0.2_plflt, 2 )

    !   Superimpose a dashed line grid, with 1.5 mm marks and spaces. With
    !   only a single mark and space element, we do not need arrays

    call plstyl( 1, 1500, 1500 )
    call plcol0(2)
    call plbox( 'g', 30.0_plflt, 0, 'g', 0.2_plflt, 0 )
    call plstyl( 0, 0, 0 )

    call plcol0(3)
    call pllab( 'Angle (degrees)', 'sine', '#frPLplot Example 1 - Sine function' )

    do i = 1, 101
       x(i) = 3.6_plflt * (i-1)
       y(i) = sin( x(i) * PI/180.0_plflt )
    enddo

    call plcol0(4)
    call plline( x, y )

  end subroutine plot3

end module plplot_code_ex1

module handlers_ex1

  use common_Ex1

  use gtk_hl
  use gtk_draw_hl

  use iso_c_binding

  implicit none

  integer(kind=c_int) :: run_status = TRUE

  real(kind=c_double), parameter :: pi = 3.14159265358979323846_c_double

contains
  function delete_cb (widget, event, gdata) result(ret)  bind(c)
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata

    call gtk_widget_destroy(window)
    call gtk_main_quit ()
    ret = FALSE
  end function delete_cb

  subroutine quit_cb(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    call gtk_widget_destroy(window)
    call gtk_main_quit ()
  end subroutine quit_cb

end module handlers_ex1

program cairo_plplot_ex1

  use handlers_ex1
  use plplot_code_ex1

  implicit none

  type(c_ptr) :: drawing, scroll_w, base, qbut

  height = 1000
  width = 1200

  call gtk_init()

  window = hl_gtk_window_new("PLplot x01 / gtk-fortran (extcairo)"&
       & //c_null_char, &
       & delete_event = c_funloc(delete_cb))
  base = hl_gtk_box_new()
  call gtk_container_add(window, base)

  drawing = hl_gtk_drawing_area_new(size=(/width, height/), &
       & has_alpha = FALSE, &
       & scroll = scroll_w, &
       & ssize=(/ 600, 500 /))
  call hl_gtk_box_pack(base, scroll_w)

  qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(quit_cb))
  call hl_gtk_box_pack(base, qbut, expand=FALSE)

  call gtk_widget_show_all (window)

  call x01f95(drawing)

  call gtk_main()

  print *, "All done"
end program cairo_plplot_ex1
