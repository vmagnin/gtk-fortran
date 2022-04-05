! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran gtk+ Fortran Interface library.
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
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!------------------------------------------------------------------------------
! Contributed by: James Tappin
! PLplot code derived from PLplot's example 17 by Alan W. Irwin
! Last modifications: vmagnin 2020-06-10 (GTK 4)
!------------------------------------------------------------------------------

module common_ex17
  use gtk, only: gtk_button_new, gtk_drawing_area_new, &
       & gtk_widget_show, gtk_window_new, gtk_init, &
       & gtk_widget_queue_draw, gtk_window_set_child
  use g, only: g_object_get_data, g_usleep, &
             & g_main_context_iteration, g_main_context_pending

  use gtk_draw_hl
  use plplot_extra

  implicit none
  integer(kind=c_int) :: height, width
  integer(kind=c_int) :: run_status = TRUE
  type(c_ptr) :: window
  type(c_ptr) :: my_gmainloop
end module common_ex17

module plplot_code_ex17
  use plplot, PI => PL_PI
  use common_ex17

  implicit none
  integer,  parameter :: nsteps = 1000
  integer, save :: id1, id2, n=0
  logical :: autoy, acc, pl_errcode
  real(kind=plflt) :: y1, y2, y3, y4, ymin, ymax, xlab, ylab
  real(kind=plflt) :: t, tmin, tmax, tjump, dt, noise
  type(c_ptr) :: cc
  integer :: colbox, collab, colline(4), styline(4)
  character(len=20) :: legline(4)
  character(len=20) :: toplab

contains
  subroutine x17f95(area)

    type(c_ptr), intent(in) :: area

    character(len=80) :: errmsg
    character(len=20) :: geometry

    ! needed for use as functions instead of subroutines
    integer :: plparseopts_rc
    integer :: plsetopt_rc

    ! Define colour map 0 to match the "GRAFFER" colour table in
    ! place of the PLPLOT default.
    integer, parameter, dimension(16) :: rval = [255, 0, 255, &
         & 0, 0, 0, 255, 255, 255, 127, 0, 0, 127, 255, 85, 170],&
         & gval = [ 255, 0, 0, 255, 0, 255, 0, 255, 127, 255, 255, 127,&
         & 0, 0, 85, 170], &
         & bval = [ 255, 0, 0, 0, 255, 255, 255, 0, 0, 0, 127, 255, 255,&
         & 127, 85, 170]

    !  Process command-line arguments
    plparseopts_rc = plparseopts(PL_PARSE_FULL)
    if (plparseopts_rc /= 0) stop "plparseopts error"

    ! Get a cairo context from the drawing area.
    cc = hl_gtk_drawing_area_cairo_new(area)

    !  Initialize plplot
    call plscmap0(rval, gval, bval)
    call plsdev("extcairo")

    ! By default the "extcairo" driver does not reset the background
    ! This is equivalent to the command line option "-drvopt set_background=1"
    plsetopt_rc = plsetopt("drvopt", "set_background=1")
    if (plsetopt_rc /= 0) stop "plsetopt error"

    ! The "extcairo" device doesn't read the size from the context.
    write(geometry, "(I0,'x',I0)") width, height
    plsetopt_rc = plsetopt( 'geometry', geometry)
    if (plsetopt_rc /= 0) stop "plsetopt error"

    !      Specify some reasonable defaults for ymin and ymax
    !      The plot will grow automatically if needed (but not shrink)
    ymin = -0.1_plflt
    ymax = 0.1_plflt

    !      Specify initial tmin and tmax -- this determines length of window.
    !      Also specify maximum jump in t
    !      This can accomodate adaptive timesteps
    tmin = 0._plflt
    tmax = 50._plflt
    !      percentage of plot to jump
    tjump = 0.3_plflt

    !      Axes options same as plbox.
    !      Only automatic tick generation and label placement allowed
    !      Eventually I'll make this fancier
    colbox = 1
    collab = 1
    !      pens color and line style
    styline(1) = 1
    colline(1) = 1
    styline(2) = 3
    colline(2) = 3
    styline(3) = 4
    colline(3) = 4
    styline(4) = 5
    colline(4) = 5

    !      pens legend
    legline(1) = 'sum'
    legline(2) = 'sin'
    legline(3) = 'sin*noi'
    legline(4) = 'sin+noi'

    !      legend position
    xlab = 0._plflt
    ylab = 0.25_plflt

    !      autoscale y
    autoy = .true.
    !      scrip, don't accumulate
    acc = .false.

    !      Initialize plplot
    call plinit()
    ! Tell the "extcairo" driver where the context is located.
    call pl_cmd(PLESC_DEVINIT, cc)

    call pladv(0)
    call plvsta()

    !      Register our error variables with PLplot
    !      From here on, we're handling all errors here
    call plstripc(id1, 'bcnst', 'bcnstv', &
         tmin, tmax, tjump, ymin, ymax, &
         xlab, ylab, &
         autoy, acc, &
         colbox, collab, &
         colline, styline, legline, &
         't', '', 'Strip chart demo')

    pl_errcode = .false.
    if ( pl_errcode ) then
       write(*,*) errmsg
       stop
    endif

    !      autoscale y
    autoy = .false.
    !      accumulate
    acc = .true.

    !      This is to represent a loop over time
    !      Let's try a random walk process
    y1 = 0.0_plflt
    y2 = 0.0_plflt
    y3 = 0.0_plflt
    y4 = 0.0_plflt
    dt = 0.1_plflt

    call gtk_widget_queue_draw(area)

  end subroutine x17f95

  subroutine add_point(area)
    type(c_ptr), intent(in) :: area

    n=n+1

    t = dble(n) * dt
    noise = plrandd() - 0.5_plflt
    y1 = y1 + noise
    y2 = sin(t*PI/18._plflt)
    y3 = y2 * noise
    y4 = y2 + noise/3._plflt

    !        There is no need for all pens to have the same number of
    !        points or being equally time spaced.
    if ( mod(n,2) /= 0 ) then
       call plstripa(id1, 0, t, y1)
    endif
    if ( mod(n,3) /= 0 ) then
       call plstripa(id1, 1, t, y2)
    endif
    if ( mod(n,4) /= 0 ) then
       call plstripa(id1, 2, t, y3)
    endif
    if ( mod(n,5) /= 0 ) then
       call plstripa(id1, 3, t, y4)
    end if
    call gtk_widget_queue_draw(area)
  end subroutine add_point

  subroutine close_strip
    !      Destroy strip chart and its memory
    call plstripd(id1)
    call plend()
    call hl_gtk_drawing_area_cairo_destroy(cc)
  end subroutine close_strip
end module plplot_code_ex17

module handlers_ex17

  use common_ex17
  use gtk_hl_container
  use gtk_hl_button
  use gtk_draw_hl
  use, intrinsic :: iso_c_binding
  use plplot_code_ex17

  implicit none

contains
  subroutine delete_cb (widget, event, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, event, gdata

    call close_strip()
    run_status = FALSE
  end subroutine delete_cb

  subroutine quit_cb(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    call close_strip()
    run_status = FALSE
  end subroutine quit_cb

  subroutine pending_events ()
    integer(kind=c_int) :: boolresult
    do while(IAND(g_main_context_pending(c_null_ptr), run_status) /= FALSE)
       ! False for non-blocking:
       boolresult = g_main_context_iteration(c_null_ptr, FALSE)
    end do
  end subroutine pending_events
end module handlers_ex17

program cairo_plplot_ex17
  use handlers_ex17
  use plplot_code_ex17
  use common_ex17

  implicit none
  type(c_ptr) :: drawing, base, qbut

  height = 500
  width = 1000

  call gtk_init()

  window = hl_gtk_window_new("PLplot x17 / gtk-fortran (extcairo)"//c_null_char, &
       & destroy = c_funloc(delete_cb))
  base = hl_gtk_box_new()
  call gtk_window_set_child(window, base)

  drawing = hl_gtk_drawing_area_new(size=[width, height], &
       & has_alpha = FALSE)

  call hl_gtk_box_pack(base, drawing)

  qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(quit_cb))
  call hl_gtk_box_pack(base, qbut, expand=FALSE)

  call gtk_widget_show(window)

  call x17f95(drawing)

  ! Note that here rather than using g_main_loop_run we look for events ourselves
  ! this makes it easy to add a point every 1/10s.
  ! An alternative would be to use g_timeout_add to control the updates
  ! (hl_plplot17e_gto.f90).

  do
     call pending_events()
     if (run_status == FALSE) exit
     call g_usleep(100000_c_long) ! So we don't burn CPU cycles
     call add_point(drawing)
  end do

  print *, "All done"
end program cairo_plplot_ex17
