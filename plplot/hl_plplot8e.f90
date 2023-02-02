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
! gfortran hl_plplot8.f90 `pkg-config --cflags --libs gtk-fortran plplotd-f95`
! Contributed by: James Tappin
! PLplot code derived from PLplot's example 8 by Alan W. Irwin

module common_ex8
  use iso_c_binding

  ! Gtk includes
  use gtk, only: gtk_container_add, gtk_label_new, gtk_main, gtk_main_quit, &
       & gtk_toggle_button_get_active, gtk_widget_destroy, &
       & gtk_widget_get_allocation, gtk_widget_queue_draw, &
       & gtk_widget_show_all, gtk_init, FALSE, GTK_FILL, GTK_EXPAND

  use gtk_hl
  use gtk_draw_hl
  use gdk_pixbuf_hl
  use plplot_extra

  implicit none

  type(c_ptr) :: window, draw, alt_sl, az_sl, fun_but, col_but, &
       & facet_but, scont_but, bcont_but, qbut

  integer(kind=c_int) :: disp_type=0, ifun=1
  real(kind=c_double) :: alt=30._c_double, az=60._c_double

  integer(kind=c_int) :: width, height

end module common_ex8

module plplot_code_ex8

  use plplot, PI => PL_PI
  use iso_c_binding
  use common_ex8

  implicit none

contains
  subroutine draw_08(area, type, alt, az, rosen)
    type(c_ptr) :: area
    integer, intent(in) :: type, rosen
    real(kind=plflt), intent(in) :: alt, az
    integer i, j, xpts, ypts, xdim, ydim
    !      xdim is the leading dimension of z, xpts <= xdim is the leading
    !      dimension of z that is defined.
    parameter (xdim=99, ydim=100, xpts=35, ypts=46)
    real(kind=plflt) x(xdim), y(ydim), z(xdim,ypts), xx, yy, r

    character(len=80) :: title
    character(len=20) :: geometry
    type(c_ptr) :: cc 

    integer nlevel
    parameter (nlevel = 10)
    real(kind=plflt) zmin, zmax, step, clevel(nlevel)
    !      Process command-line arguments
    call plparseopts(PL_PARSE_FULL)

    write(title, "('#frPLplot Example 8 - Alt=',I3,', Az=',I3)")&
         & nint(alt), nint(az)

    do i = 1,xpts
       x(i) = dble(i-1-(xpts/2))/dble (xpts/2)
       if(rosen.eq.1) x(i) = 1.5_plflt*x(i)
    enddo
    do j = 1,ypts
       y(j) = dble(j-1-(ypts/2))/dble (ypts/2)
       if(rosen.eq.1) y(j) = y(j) + 0.5_plflt
    enddo

    do i=1,xpts
       xx = x(i)
       do j=1,ypts
          yy = y(j)
          if(rosen.eq.1) then
             z(i,j) = (1._plflt - xx)**2 + 100._plflt*(yy - xx**2)**2
             !            The log argument may be zero for just the right grid.
             if(z(i,j).gt.0._plflt) then
                z(i,j) = log(z(i,j))
             else
                z(i,j) = -5._plflt
             endif
          else
             !            sombrero function
             r = sqrt(xx*xx + yy*yy)
             z(i,j) = exp(-r*r) * cos(2.0_plflt*PI*r)
          endif
       enddo
    enddo
    call a2mnmx(z, xpts, ypts, zmin, zmax, xdim)
    step = (zmax-zmin)/(nlevel+1)
    do i = 1, nlevel
       clevel(i) = zmin + step*i
    enddo

    ! Get a cairo context from the drawing area.

    cc = hl_gtk_drawing_area_cairo_new(area)

    !  Initialize plplot
    call plsdev("extcairo")

    ! By default the "extcairo" driver does not reset the background
    ! This is equivalent to the command line option "-drvopt set_background=1"
    call plsetopt("drvopt", "set_background=1")  

    ! The "extcairo" device doesn't read the size from the context.
    write(geometry, "(I0,'x',I0)") width, height
    call plsetopt("geometry",  geometry)
 

    call plinit

    ! Tell the "extcairo" driver where the context is located.
    call pl_cmd(PLESC_DEVINIT, cc)

    call pllightsource(1._plflt, 1._plflt, 1._plflt)

    call pladv(0)
    call plclear()
    call plvpor(0.0_plflt, 1.0_plflt, 0.0_plflt, 0.9_plflt )
    call plwind(-1.0_plflt, 1.0_plflt, -0.9_plflt, 1.1_plflt )
    call plcol0(3)
    call plmtex('t', 1.0_plflt, 0.5_plflt, 0.5_plflt, title)
    call plcol0(1)
    if(rosen.eq.1) then
       call plw3d(1.0_plflt, 1.0_plflt, 1.0_plflt, -1.5_plflt, &
            1.5_plflt, -0.5_plflt, 1.5_plflt, zmin, zmax, alt,az)
    else
       call plw3d(1.0_plflt, 1.0_plflt, 1.0_plflt, -1.0_plflt, &
            1.0_plflt, -1.0_plflt, 1.0_plflt, zmin, zmax, alt,az)
    endif
    call plbox3('bnstu','x axis', 0.0_plflt, 0, &
         'bnstu', 'y axis', 0.0_plflt, 0, &
         'bcdmnstuv','z axis', 0.0_plflt, 0)
    call plcol0(2)

    if(type == 0) then 
       call cmap1_init(1)
    else
       call cmap1_init(0)
    end if

    if (iand(type, ior(ior(BASE_CONT, SURF_CONT), TOP_CONT)) /= 0) then
       call plsurf3d(x(:xpts), y(:ypts), z(:xpts,:ypts), &
            type, clevel)
    else
       call plsurf3d(x(:xpts), y(:ypts), z(:xpts,:ypts), &
            type, clevel(nlevel:1))
    end if
    call plend

    call gtk_widget_queue_draw(area)
    call hl_gtk_drawing_area_cairo_destroy(cc)

  end subroutine draw_08

  !----------------------------------------------------------------------------
  subroutine cmap1_init(gray)
    !      For gray.eq.1, basic grayscale variation from half-dark
    !      to light.  Otherwise, hue variations around the front of the
    !      colour wheel from blue to green to red with constant lightness
    !      and saturation.

    use plplot
    implicit none
    integer gray
    real(kind=plflt) i(0:1), h(0:1), l(0:1), s(0:1)
    !      left boundary
    i(0) = 0._plflt
    !      right boundary
    i(1) = 1._plflt
    if (gray.eq.1) then
       !        hue -- low: red (arbitrary if s=0)
       h(0) = 0.0_plflt
       !        hue -- high: red (arbitrary if s=0)
       h(1) = 0.0_plflt
       !        lightness -- low: half-dark
       l(0) = 0.5_plflt
       !        lightness -- high: light
       l(1) = 1.0_plflt
       !        minimum saturation
       s(0) = 0.0_plflt
       !        minimum saturation
       s(1) = 0.0_plflt
    else
       !        This combination of hues ranges from blue to cyan to green to yellow
       !        to red (front of colour wheel) with constant lightness = 0.6
       !        and saturation = 0.8.

       !        hue -- low: blue
       h(0) = 240._plflt
       !        hue -- high: red
       h(1) = 0.0_plflt
       !        lightness -- low:
       l(0) = 0.6_plflt
       !        lightness -- high:
       l(1) = 0.6_plflt
       !        saturation
       s(0) = 0.8_plflt
       !        minimum saturation
       s(1) = 0.8_plflt
    endif
    call plscmap1n(256)
    call plscmap1l(.false., i, h, l, s)
  end subroutine cmap1_init

  !----------------------------------------------------------------------------
  !      Subroutine a2mnmx
  !      Minimum and the maximum elements of a 2-d array.

  subroutine a2mnmx(f, nx, ny, fmin, fmax, xdim)
    use plplot
    implicit none

    integer   i, j, nx, ny, xdim
    real(kind=plflt)    f(xdim, ny), fmin, fmax

    fmax = f(1, 1)
    fmin = fmax
    do j = 1, ny
       do  i = 1, nx
          fmax = max(fmax, f(i, j))
          fmin = min(fmin, f(i, j))
       enddo
    enddo
  end subroutine a2mnmx
end module plplot_code_ex8

module handlers_ex8
  use common_ex8
  use plplot_code_ex8

  implicit none

contains
  subroutine quit_cb(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    call gtk_widget_destroy(window)
    call gtk_main_quit ()
  end subroutine quit_cb
  subroutine set_azimuth(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    az = real(hl_gtk_slider_get_value(widget), c_double)
    call draw_08(draw, disp_type, alt, az, ifun)
  end subroutine set_azimuth
  subroutine set_altitude(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    alt = real(hl_gtk_slider_get_value(widget), c_double)
    call draw_08(draw, disp_type, alt, az, ifun)
  end subroutine set_altitude
  subroutine set_rosen(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    ifun = gtk_toggle_button_get_active(widget)
    call draw_08(draw, disp_type, alt, az, ifun)
  end subroutine set_rosen

  subroutine set_colour(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    if (gtk_toggle_button_get_active(widget) == 1) then
       disp_type = ior(disp_type, MAG_COLOR)
    else
       disp_type = iand(disp_type, not(MAG_COLOR))
    end if
    call draw_08(draw, disp_type, alt, az, ifun)
  end subroutine set_colour
  subroutine set_facet(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    if (gtk_toggle_button_get_active(widget) == 1) then
       disp_type = ior(disp_type, FACETED)
    else
       disp_type = iand(disp_type, not(FACETED))
    end if
    call draw_08(draw, disp_type, alt, az, ifun)
  end subroutine set_facet
 subroutine set_scont(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    if (gtk_toggle_button_get_active(widget) == 1) then
       disp_type = ior(disp_type, SURF_CONT)
    else
       disp_type = iand(disp_type, not(SURF_CONT))
    end if
    call draw_08(draw, disp_type, alt, az, ifun)
  end subroutine set_scont
  subroutine set_bcont(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    if (gtk_toggle_button_get_active(widget) == 1) then
       disp_type = ior(disp_type, BASE_CONT)
    else
       disp_type = iand(disp_type, not(BASE_CONT))
    end if
    call draw_08(draw, disp_type, alt, az, ifun)
  end subroutine set_bcont

  subroutine resize_area(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    type(gtkallocation), target:: alloc

    call gtk_widget_get_allocation(draw,c_loc(alloc))
    call hl_gtk_drawing_area_resize(draw)
    print*,"resize",alloc%width,alloc%height
    width=alloc%width
    height=alloc%height

    call draw_08(draw, disp_type, alt, az, ifun)

  end subroutine resize_area

  subroutine dump_screen(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    type(c_ptr) :: pixb
    character(len=120), dimension(:), allocatable :: files
    integer(kind=c_int) :: ipick

    ipick = hl_gtk_file_chooser_show(files, current=TRUE, &
         & title="Output image file"//c_null_char, &
         & filter=['image/png ', 'image/jpeg', 'image/tiff'], &
         & parent=window)
    if (c_f_logical(ipick)) then
       pixb = hl_gtk_drawing_area_get_gdk_pixbuf(draw)
       call hl_gdk_pixbuf_save(pixb, files(1))
    end if
  end subroutine dump_screen
end module handlers_ex8


program cairo_plplot_ex8
  use handlers_ex8
  use plplot_code_ex8

  use common_ex8

  implicit none

  type(c_ptr) :: base, btable, junk

  height = 600
  width = 600  ! Must be a multiple of 4

  call gtk_init()

  window = hl_gtk_window_new("PLplot x08 / gtk-fortran (extcairo)"//&
       & c_null_char, &
       & destroy = c_funloc(quit_cb))
  base = hl_gtk_box_new()
  call gtk_container_add(window, base)

  ! The drawing area for the plot
  draw = hl_gtk_drawing_area_new(size=(/width, height/), &
       & has_alpha = FALSE, size_allocate=c_funloc(resize_area))
  call hl_gtk_box_pack(base, draw)

  ! Put the direction settings in a table.
  btable=hl_gtk_table_new(2,2, homogeneous=FALSE)
  call hl_gtk_box_pack(base, btable, expand=FALSE)

  junk=gtk_label_new("Azimuth:"//c_null_char)
  call hl_gtk_table_attach(btable, junk, 0, 0, xopts=0, yopts=0)
  az_sl = hl_gtk_slider_new(0, 360, initial_value=int(az), &
       & value_changed=c_funloc(set_azimuth))
  call hl_gtk_table_attach(btable, az_sl, 1, 0, yopts=0)

  ! N.B. Elevation <0 doesn't seem to work.
  junk=gtk_label_new("Elevation:"//c_null_char)
  call hl_gtk_table_attach(btable, junk, 0, 1, xopts=0, yopts=0)
  alt_sl = hl_gtk_slider_new(0, 90, initial_value=int(alt), &
       & value_changed=c_funloc(set_altitude))
  call hl_gtk_table_attach(btable, alt_sl, 1, 1, yopts=0)

  ! And another table for the selectors
  btable=hl_gtk_table_new(homogeneous=TRUE)
  call hl_gtk_box_pack(base, btable, expand=FALSE)

  fun_but = hl_gtk_check_button_new("Rosen"//c_null_char, &
       & toggled=c_funloc(set_rosen), initial_state=ifun)
  call hl_gtk_table_attach(btable, fun_but, 0, 0, yopts=0, xopts=GTK_FILL)

  col_but=hl_gtk_check_button_new("Colour level"//c_null_char, &
       & toggled=c_funloc(set_colour))
  call hl_gtk_table_attach(btable,col_but, 1, 0, yopts=0, xopts=GTK_FILL)

  facet_but=hl_gtk_check_button_new("Facets"//c_null_char, &
       & toggled=c_funloc(set_facet))
  call hl_gtk_table_attach(btable,facet_but, 2, 0, yopts=0, xopts=GTK_FILL)

  scont_but=hl_gtk_check_button_new("Surface contours"//c_null_char, &
       & toggled=c_funloc(set_scont))
  call hl_gtk_table_attach(btable, scont_but, 0, 1, yopts=0, xopts=GTK_FILL)

  bcont_but=hl_gtk_check_button_new("Base contours"//c_null_char, &
       & toggled=c_funloc(set_bcont))
  call hl_gtk_table_attach(btable, bcont_but, 1, 1, yopts=0, xopts=GTK_FILL)

  junk = hl_gtk_button_new("Dump"//c_new_line//"Screen"//c_null_char, &
       & clicked=c_funloc(dump_screen))
  call hl_gtk_table_attach(btable, junk, 3, 0, yopts=0, &
       & xopts=GTK_FILL, yspan=2)

  qbut=hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(quit_cb))
  call hl_gtk_box_pack(base, qbut, expand=FALSE)

  call gtk_widget_show_all (window)

  call draw_08(draw, disp_type, alt, az, ifun)

  call gtk_main()

end program cairo_plplot_ex8
