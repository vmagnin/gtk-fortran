! Copyright (C) 2012
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
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! gfortran hl_plplot1e.f90 `pkg-config --cflags --libs gtk-fortran plplot-fortran plplot`
! Contributed by: James Tappin
! PLplot code derived from PLplot's example 4 by Alan W. Irwin

module common_ex4
  use iso_c_binding

  !*******
  use cairo, only: cairo_get_target, cairo_image_surface_get_height, &
       & cairo_image_surface_get_width

  use gtk, only: gtk_container_add, gtk_main, gtk_main_quit, &
       & gtk_widget_destroy, gtk_widget_show_all, gtk_init, FALSE

  use gtk_draw_hl
  use gtk_hl

  use plplot_extra

  implicit none

  type(c_ptr) :: window

end module common_ex4

module plplot_code_ex4
  use plplot, PI => PL_PI
  ! use iso_c_binding
  use common_ex4

  implicit none

contains

  subroutine plot_04(area)
!    integer, intent(in) :: type
    type(c_ptr), intent(in), dimension(2) :: area

    type(c_ptr), dimension(2) :: cc, cs
    integer :: i
    character(len=25) :: geometry

    ! needed for use as functions instead of subroutines
    integer :: plparseopts_rc
    integer :: plsetopt_rc

    ! Define colour map 0 to match the "GRAFFER" colour table in
    ! place of the PLPLOT default.
    integer, parameter, dimension(16) :: rval = (/255, 0, 255, &
         & 0, 0, 0, 255, 255, 255, 127, 0, 0, 127, 255, 85, 170/),&
         & gval = (/ 255, 0, 0, 255, 0, 255, 0, 255, 127, 255, 255, 127,&
         & 0, 0, 85, 170/), &
         & bval = (/ 255, 0, 0, 0, 255, 255, 255, 0, 0, 0, 127, 255, 255,&
         & 127, 85, 170/)

    !  Process command-line arguments
    ! call plparseopts(PL_PARSE_FULL)
    plparseopts_rc = plparseopts(PL_PARSE_FULL)
    if (plparseopts_rc .ne. 0) stop "plparseopts error"


    ! Get a cairo context from the drawing area.
    do i=1,2
       cc(i) = hl_gtk_drawing_area_cairo_new(area(i))
       cs(i) = cairo_get_target(cc(i))
    end do

    !  Initialize plplot
    call plscmap0(rval, gval, bval)
    call plsdev("extcairo")

    ! By default the "extcairo" driver does not reset the background
    ! This is equivalent to the command line option "-drvopt set_background=1"
    ! call plsetopt("drvopt", "set_background=1")
    plsetopt_rc = plsetopt("drvopt", "set_background=1")
    if (plsetopt_rc .ne. 0) stop "plsetopt error"

    ! The "extcairo" device doesn't read the size from the context.

    write(geometry, "(I0,'x',I0)") cairo_image_surface_get_width(cs(1)), &
         & cairo_image_surface_get_height(cs(1))
    ! call plsetopt("geometry",  geometry)
    plsetopt_rc = plsetopt( 'geometry', geometry)
    if (plsetopt_rc .ne. 0) stop "plsetopt error"

    ! Initialize

    call plinit

    ! Tell the "extcairo" driver where the context is located. This must be
    ! done AFTER the plstar or plinit call.

    call pl_cmd(PLESC_DEVINIT, cc(1))

    call plfont(2)   ! Roman font
    call plot1(0)

    ! Now continue the plot on the other surface
    call pl_cmd(PLESC_DEVINIT, cc(2))

    call plot1(1)

    call plend

    call hl_gtk_drawing_area_cairo_destroy(cc(1))
    call hl_gtk_drawing_area_cairo_destroy(cc(2))

  end subroutine plot_04

  subroutine plot1(type)
    use plplot, PI => PL_PI
    implicit none
    integer, parameter :: MAX_NLEGEND = 2

    real(kind=plflt) :: freql(0:100),ampl(0:100),phase(0:100), freq, f0
    integer          :: i, type
    integer          :: nlegend

    real(kind=plflt) :: legend_width, legend_height
    integer          :: opt_array(MAX_NLEGEND), text_colors(MAX_NLEGEND), line_colors(MAX_NLEGEND), &
         line_styles(MAX_NLEGEND), symbol_colors(MAX_NLEGEND), symbol_numbers(MAX_NLEGEND)

    ! For plplot 5.9.9 or lower the next declarations should be integers
    ! For 5.9.10 or higher they should be reals
!    real(kind=plflt) :: symbol_scales(MAX_NLEGEND), box_scales(0)
!    integer :: line_widths(MAX_NLEGEND), box_line_widths(0)
    real(kind=plflt) :: symbol_scales(MAX_NLEGEND), box_scales(MAX_NLEGEND)

!    real(kind=plflt) :: line_widths(MAX_NLEGEND), box_line_widths(0)
!    integer          :: box_colors(0), box_patterns(0)

    real(kind=plflt) :: line_widths(MAX_NLEGEND), box_line_widths(MAX_NLEGEND)
    integer          :: box_colors(MAX_NLEGEND), box_patterns(MAX_NLEGEND)

    character(len=20):: text(MAX_NLEGEND)
    character(len=1) :: symbols(MAX_NLEGEND)

    call pladv(0)
    !      Set up data for log plot.
    f0 = 1._plflt
    do i=0,100
       freql(i)= -2.0_plflt + dble (i)/20.0_plflt
       freq=10.0_plflt**freql(i)
       ampl(i)=20.0_plflt*log10(1.0_plflt/sqrt(1.0_plflt+(freq/f0)**2))
       phase(i)=-(180.0_plflt/PI)*atan(freq/f0)
    enddo
    call plvpor(0.15_plflt, 0.85_plflt, 0.1_plflt, 0.9_plflt)
    call plwind(-2.0_plflt, 3.0_plflt, -80.0_plflt, 0.0_plflt)
    call plcol0(1)
    !      Try different axis and labelling styles.
    if (type.eq.0) then
       call plbox('bclnst', 0.0_plflt, 0, 'bnstv', 0.0_plflt, 0)
    elseif (type.eq.1) then
       call plbox('bcfghlnst', 0.0_plflt, 0, 'bcghnstv', 0.0_plflt, 0)
    else
       stop 'plot1: invalid type'
    endif
    !      Plot ampl vs freq.
    call plcol0(2)
    call plline(freql,ampl)
    call plcol0(2)
    call plptex(1.6_plflt, -30.0_plflt, 1.0_plflt, -20.0_plflt, 0.5_plflt, &
         '-20 dB/decade')
    !      Put labels on.
    call plcol0(1)
    call plmtex('b', 3.2_plflt, 0.5_plflt, 0.5_plflt, 'Frequency')
    call plmtex('t', 2.0_plflt, 0.5_plflt, 0.5_plflt, &
         'Single Pole Low-Pass Filter')
    call plcol0(2)
    call plmtex('l', 5.0_plflt, 0.5_plflt, 0.5_plflt, 'Amplitude (dB)')
    nlegend = 1
    !      For the gridless case, put phase vs freq on same plot.
    if (type.eq.0) then
       call plcol0(1)
       call plwind(-2.0_plflt, 3.0_plflt, -100.0_plflt, 0.0_plflt)
       call plbox(' ', 0.0_plflt, 0, 'cmstv', 30.0_plflt, 3)
       call plcol0(3)
       call plline(freql, phase)
       call plstring(freql, phase, '*')
       call plcol0(3)
       call plmtex('r', 5.0_plflt, 0.5_plflt, 0.5_plflt, &
            'Phase shift (degrees)')
       nlegend = 2
    endif

    !     Draw a legend
    !     First legend entry.
    opt_array(1)   = PL_LEGEND_LINE
    text_colors(1) = 2
    text(1)        = 'Amplitude'
    line_colors(1) = 2
    line_styles(1) = 1
    ! For plplot 5.9.9 or lower comment out the real assignment,
    ! for 5.9.10 or higher, comment out the integer assignment.
!    line_widths(1) = 1
    line_widths(1) = 1.0_plflt
    !     note from the above opt_array the first symbol (and box) indices
    !     do not have to be specified

    !     Second legend entry.
    opt_array(2)      = PL_LEGEND_LINE + PL_LEGEND_SYMBOL
    text_colors(2)    = 3
    text(2)           = 'Phase shift'
    line_colors(2)    = 3
    line_styles(2)    = 1
    ! For plplot 5.9.9 or lower comment out the real assignment,
    ! for 5.9.10 or higher, comment out the integer assignment.
!    line_widths(2)    = 1
    line_widths(2)    = 1.0_plflt
    symbol_colors(2)  = 3
    symbol_scales(2)  = 1.0
    symbol_numbers(2) = 4
    symbols(2)        = '*'

    !     from the above opt_arrays we can completely ignore everything
    !     to do with boxes. (Hence the size 0 for the associated arrays)
    !     (note: use the argument nlegend explicitly)

    call plscol0a( 15, 32, 32, 32, 0.70_plflt )
    call pllegend( legend_width, legend_height, &
         PL_LEGEND_BOUNDING_BOX, 0, &
         0.0_plflt, 0.0_plflt, 0.1_plflt, 15, &
         1, 1, 0, 0, &
         opt_array, &
         1.0_plflt, 1.0_plflt, 2.0_plflt, &
         1.0_plflt, &           !
         text_colors, text, &
         box_colors, box_patterns, box_scales, box_line_widths, &
         line_colors, line_styles, line_widths, &
         symbol_colors, symbol_scales, symbol_numbers, symbols )

  end subroutine plot1
end module plplot_code_ex4

module handlers_ex4
  use common_ex4

  implicit none
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

end module handlers_ex4

program cairo_plplot_ex4

  use handlers_ex4
  use plplot_code_ex4
  use common_ex4

  implicit none

  type(c_ptr), dimension(2) :: drawing
  type(c_ptr) :: base, nbook, qbut
  integer :: pno

  call gtk_init()

  window = hl_gtk_window_new("PLplot x04 / gtk-fortran (extcairo)"&
       & //c_null_char, &
       & delete_event = c_funloc(delete_cb))

  base = hl_gtk_box_new()
  call gtk_container_add(window, base)

  nbook = hl_gtk_notebook_new()
  call hl_gtk_box_pack(base, nbook)
  drawing(1) = hl_gtk_drawing_area_new(size=[800,600], &
       & has_alpha=FALSE)

  pno = hl_gtk_notebook_add_page(nbook, drawing(1), &
       & label="Plot 1 (No grid)"//c_null_char)

  drawing(2) = hl_gtk_drawing_area_new(size=[800,600], &
       & has_alpha=FALSE)

  pno= hl_gtk_notebook_add_page(nbook, drawing(2), &
       & label="Plot 2 (With grid)"//c_null_char)

  qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(quit_cb))
  call hl_gtk_box_pack(base, qbut, expand=FALSE)

  call gtk_widget_show_all (window)

  call plot_04(drawing)

  call gtk_main()

  print *, "All done"
end program cairo_plplot_ex4
