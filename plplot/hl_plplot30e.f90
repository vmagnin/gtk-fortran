! This file is part of gtk-fortran, a GTK / Fortran interface library.
! Copyright (C) 2012 The gtk-fortran team
!
! This is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3, or (at your option)
! any later version.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! Under Section 7 of GPL version 3, you are granted additional
! permissions described in the GCC Runtime Library Exception, version
! 3.1, as published by the Free Software Foundation.
!
! You should have received a copy of the GNU General Public License along with
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!------------------------------------------------------------------------------
! Contributed by: James Tappin
! PLplot code derived from PLplot's example 30 by Hazen Babcock and Andrew Ross
! Last modifications: vmagnin 2020-06-10 (GTK 4), 2020-07-13
!------------------------------------------------------------------------------

module common_ex30
  use, intrinsic :: iso_c_binding
  use gtk, only: gtk_window_set_child, gtk_widget_queue_draw, &
       & gtk_widget_show, gtk_window_destroy
  use gtk_draw_hl
  use plplot_extra

  implicit none
  integer(c_int) :: height, width
  type(c_ptr) :: window
  type(c_ptr) :: my_gmainloop
end module common_ex30

module plplot_code_ex30
  use plplot, PI => PL_PI
  use common_ex30

  implicit none
  real(plflt) :: xscale, yscale, xoff, yoff

contains
  subroutine x30f95(area)

    type(c_ptr), intent(in) :: area
    type(c_ptr) :: cc
    character(len=20) :: geometry

    ! needed for use as functions instead of subroutines
    integer :: plparseopts_rc
    integer :: plsetopt_rc

    integer, dimension(4) ::  red, green, blue
    real(plflt), dimension (4) :: alpha, px, py
    real(plflt), dimension (2) :: pos, rcoord, gcoord, bcoord, acoord
    logical, dimension(1) :: rev

    data red   / 127, 255, 0, 0 /
    data green / 127, 0, 255, 0 /
    data blue  / 127, 0, 0, 255 /
    data alpha / 1.0_plflt, 1.0_plflt, 1.0_plflt, 1.0_plflt /
    data px / 0.1_plflt, 0.5_plflt, 0.5_plflt, 0.1_plflt /
    data py / 0.1_plflt, 0.1_plflt, 0.5_plflt, 0.5_plflt /
    data pos / 0.0_plflt, 1.0_plflt /
    data rcoord / 1.0_plflt, 1.0_plflt /
    data gcoord / 0.0_plflt, 0.0_plflt /
    data bcoord / 0.0_plflt, 0.0_plflt /
    data acoord / 0.0_plflt, 1.0_plflt /
    data rev / .false. /

    integer i, j
    integer icol, r, g, b
    real(plflt) :: a

    !  Process command-line arguments
    plparseopts_rc = plparseopts(PL_PARSE_FULL)
    if (plparseopts_rc /= 0) stop "plparseopts error"

    ! Get a cairo context from the drawing area.
    cc = hl_gtk_drawing_area_cairo_new(area)

    !  Initialize plplot
    call plsdev("extcairo")

    ! By default the "extcairo" driver does not reset the background
    ! This is equivalent to the command line option "-drvopt set_background=1"
    plsetopt_rc = plsetopt("drvopt", "set_background=1")
    if (plsetopt_rc /= 0) stop "plsetopt error"

    ! The "extcairo" device doesn't read the size from the context.
    write(geometry, "(I0,'x',I0)") width, height
    plsetopt_rc = plsetopt( 'geometry', geometry)
    if (plsetopt_rc /= 0) stop "plsetopt error"

    call plscmap0n(4)

    call plscmap0a (red, green, blue, alpha)

    !  Divide page into 2 plots
    call plstar(2,1)

    ! Tell the "extcairo" driver where the context is located.
    call pl_cmd(PLESC_DEVINIT, cc)
    !
    ! Page 1:
    !
    ! This is a series of red, green and blue rectangles overlaid
    ! on each other with gradually increasing transparency.
    !

    ! Set up the window
    call pladv (0)
    call plvpor (0.0_plflt, 1.0_plflt, 0.0_plflt, 1.0_plflt)
    call plwind (0.0_plflt, 1.0_plflt, 0.0_plflt, 1.0_plflt)
    call plcol0 (0)
    call plbox ("", 1.0_plflt, 0, "", 1.0_plflt, 0)

    ! Draw the boxes
    do i = 1,9
       icol = mod(i-1,3) + 1

       ! Get a color, change its transparency and
       ! set it as the current color.
       call plgcol0a (icol, r, g, b, a)
       call plscol0a (icol, r, g, b, 1.0_plflt - dble(i-1)/9.0_plflt)
       call plcol0 (icol)

       ! Draw the rectangle
       call plfill (px, py)

       ! Shift the rectangles coordinates
       do j = 1,4
          px(j) = px(j) + 0.5_plflt/9.0_plflt
          py(j) = py(j) + 0.5_plflt/9.0_plflt
       enddo
    enddo
    !
    ! Page 2:
    !
    ! This is a bunch of boxes colored red, green or blue with a single
    ! large (red) box of linearly varying transparency overlaid. The
    ! overlaid box is completely transparent at the bottom and completely
    ! opaque at the top.
    !

    ! Set up the window
    call pladv(0)
    call plvpor(0.1_plflt, 0.9_plflt, 0.1_plflt, 0.9_plflt)
    call plwind(0.0_plflt, 1.0_plflt, 0.0_plflt, 1.0_plflt)

    ! Draw the boxes. There are 25 of them drawn on a 5 x 5 grid.
    do i = 1,5
       ! Set box X position
       px(1) = 0.05_plflt + 0.2_plflt * dble(i-1)
       px(2) = px(1) + 0.1_plflt
       px(3) = px(2)
       px(4) = px(1)

       ! We don't want the boxes to be transparent, so since we changed
       ! the colors transparencies in the first example we have to change
       ! the transparencies back to completely opaque.
       icol = mod(i-1,3) + 1
       call plgcol0a (icol, r, g, b, a)
       call plscol0a (icol, r, g, b, 1.0_plflt)
       call plcol0 (icol)
       do j = 1, 5
          ! Set box y position and draw the box.
          py(1) = 0.05_plflt + 0.2_plflt * dble(j-1)
          py(2) = py(1)
          py(3) = py(1) + 0.1_plflt
          py(4) = py(3)
          call plfill(px, py)
       enddo
    enddo

    ! Create the color map with 128 colors and call plscmap1la to initialize
    ! the color values with a linearly varying red transparency (or alpha)
    call plscmap1n(128)
    call plscmap1la(.true., pos, rcoord, gcoord, bcoord, acoord, rev)

    ! Use that cmap1 to create a transparent red gradient for the whole
    ! window.
    px(1) = 0._plflt
    px(2) = 1._plflt
    px(3) = 1._plflt
    px(4) = 0._plflt

    py(1) = 0._plflt
    py(2) = 0._plflt
    py(3) = 1._plflt
    py(4) = 1._plflt

    call plgradient( px, py, 90._plflt )

    !  Don't forget to call PLEND to finish off!
    call plend()
    call gtk_widget_queue_draw(area)
    call hl_gtk_drawing_area_cairo_destroy(cc)

  end subroutine x30f95
end module plplot_code_ex30

module handlers_ex30

  use common_ex30
  use gtk_hl_container
  use gtk_hl_button
  use gtk_draw_hl
  use, intrinsic :: iso_c_binding

  implicit none
  real(c_double), parameter :: pi = acos(-1.0_c_double)

contains

  subroutine quit_cb(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    call gtk_window_destroy(window)
  end subroutine quit_cb


  subroutine activate(app, gdata) bind(c)
    use plplot_code_ex30
    use gtk, only: gtk_application_window_new
    implicit none
    type(c_ptr), value, intent(in)  :: app, gdata
    ! Pointers toward our GTK widgets:
    type(c_ptr) :: drawing, scroll_w, base, qbut

    height = 600
    width = 1200

    ! Create the window:
    window = gtk_application_window_new(app)

    base = hl_gtk_box_new()
    call gtk_window_set_child(window, base)

    drawing = hl_gtk_drawing_area_new(size=[width, height], &
         & has_alpha = TRUE, &
         & scroll = scroll_w, &
         & ssize=[ 650, 600 ])
    call hl_gtk_box_pack(base, scroll_w)

    qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(quit_cb))
    call hl_gtk_box_pack(base, qbut, expand=FALSE)

    call gtk_widget_show(window)

    call x30f95(drawing)
  end subroutine activate
end module handlers_ex30

program cairo_plplot_ex30
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
  use handlers_ex30
  use gtk_hl_container, only: hl_gtk_application_new
  implicit none
  type(c_ptr) :: my_app

  my_app = hl_gtk_application_new("gtk-fortran.plplot.hl_plplot30e"//c_null_char, &
                             & c_funloc(activate))
end program cairo_plplot_ex30

