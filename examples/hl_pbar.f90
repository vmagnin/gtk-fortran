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
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Contributed by James Tappin.

module handlers
  use gtk_hl
  use gtk, only: gtk_button_new, gtk_container_add, gtk_events_pending, gtk_main,&
       & gtk_main_iteration, gtk_main_iteration_do, gtk_main_quit, gtk_widget_destroy,&
       & gtk_progress_bar_new, gtk_widget_show, gtk_widget_show_all, gtk_window_new, &
       & gtk_init
  use g, only: g_usleep

  implicit none

  type(c_ptr) :: win,bar,pbar,qbut, box
  integer(kind=c_int) :: run_status = TRUE

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    print *, "Exit called"
!    call gtk_widget_destroy(win)
!    call gtk_main_quit ()
    run_status = FALSE
  end subroutine my_destroy

  subroutine pending_events ()
    integer(c_int) :: boolresult
    do 
       if (IAND(gtk_events_pending(), run_status) == FALSE) exit
       boolresult = gtk_main_iteration_do(FALSE) ! False for non-blocking
    end do
  end subroutine pending_events

end module handlers

program progress

  ! PROGRESS
  ! Examples of progress bars

  use handlers

  implicit none

  integer :: t0, t1, istep
  real(kind=c_double) :: bval

  ! Initialize gtk & create a window for the heirarchy
  call gtk_init()
  win = hl_gtk_window_new("Progress"//c_null_char, destroy=c_funloc(my_destroy))

  ! Make a column box to contain our widgets and put it in the window
  box=hl_gtk_box_new()
  call gtk_container_add(win, box)

  ! Make 2 horizontal progress bars and put them in the box
  bar = hl_gtk_progress_bar_new()
  call hl_gtk_box_pack(box, bar)
  pbar = hl_gtk_progress_bar_new(step=0.05_c_double)
  call hl_gtk_box_pack(box, pbar)

  ! Make a quit button and put that in the box.
  qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(my_destroy))
  call hl_gtk_box_pack(box, qbut)

  ! Display the window
  call gtk_widget_show_all(Win) 

  ! Get the epoch in milliseconds and start a counter
  call system_clock(t0)
  istep = 0

  ! event loop
  do
     call pending_events()
     if (run_status == FALSE) exit
     call g_usleep(10000_c_long) ! So we don't burn CPU cycles
     istep = istep+1
     call system_clock(t1)
     bval = real(t1-t0,c_double)/10000.
     if (bval > 1._c_double) exit
     call hl_gtk_progress_bar_set(bar, bval, string=TRUE)
     if (mod(istep, 20) == 0) &
          & call hl_gtk_progress_bar_set(pbar, text="Working"//c_null_char)
     ! There's an issue with string arguments in overloaded procedures
  end do
  call gtk_widget_destroy(Win)
end program progress
