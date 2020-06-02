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
! Contributed by James Tappin.
! Last modification: vmagnin 2020-06-02 (GTK 4 version)
!------------------------------------------------------------------------------

module handlers
!  use gth_hl
  use gtk_hl_container
  use gtk_hl_progress
  use gtk_hl_button
  use gtk, only: gtk_button_new, gtk_window_set_child, gtk_window_destroy, &
       & gtk_progress_bar_new, gtk_widget_show, gtk_window_new, &
       & gtk_init
  use g, only: g_usleep, g_main_context_iteration, g_main_context_pending

  implicit none
  integer(kind=c_int) :: run_status = TRUE
  integer(c_int) :: boolresult
  type(c_ptr) :: win,pbar,qbut, box
  type(c_ptr), dimension(10) :: bar

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    print *, "Exit called"
    run_status = FALSE
    call gtk_window_destroy(win)
  end subroutine my_destroy

  ! This function is needed to update the GUI during long computations.
  ! https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html
  subroutine pending_events ()
    do while(IAND(g_main_context_pending(c_null_ptr), run_status) /= FALSE)
      ! FALSE for non-blocking:
      boolresult = g_main_context_iteration(c_null_ptr, FALSE)
    end do
  end subroutine pending_events

end module handlers

program progress
  ! PROGRESS
  ! Examples of progress bars
  ! Note this example is only really interesting if you:
  ! a) Compile with the -fopenmp option and
  ! b) run it on a multicore machine

  use handlers

  implicit none
  integer :: i
  integer(kind=c_int) :: istep, itmax

  ! Initialize gtk & create a window for the heirarchy
  call gtk_init()
  win = hl_gtk_window_new("Progress"//c_null_char, destroy=c_funloc(my_destroy))

  ! Make a column box to contain our widgets and put it in the window
  box=hl_gtk_box_new()
  call gtk_window_set_child(win, box)

  ! Make several horizontal progress bars and put them in the box
  do i=1,10
     bar(i) = hl_gtk_progress_bar_new()
     call hl_gtk_box_pack(box, bar(i))
  end do
  pbar = hl_gtk_progress_bar_new(step=0.05_c_double)
  call hl_gtk_box_pack(box, pbar)

  ! Make a quit button and put that in the box.
  qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(my_destroy))
  call hl_gtk_box_pack(box, qbut)

  ! Display the window
  call gtk_widget_show(Win) 

  itmax=300
  !$omp parallel do default(shared), private(istep)
  do i = 1, 10
     ! event loop
     istep=0
     do
        call pending_events()
        if (run_status == FALSE) exit
        call g_usleep(10000_c_long) ! So we don't burn CPU cycles
        istep = istep+1

        if (istep > itmax) exit
        call hl_gtk_progress_bar_set(bar(i), istep, &
             & itmax, string=TRUE)
        if (mod(istep, 20_c_int) == 0) &
             & call hl_gtk_progress_bar_set(pbar, text="Working"//c_null_char)
     end do
     if (run_status == FALSE) cycle
  end do
  !$omp end parallel do

  print *, "Bye..."

end program progress

