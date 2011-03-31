module handlers
  use fgtk_h_widgets
  
  implicit none

  type(c_ptr) :: Win,pbar,qbut, box
  integer(kind=c_int) :: run_status = TRUE
  type(c_ptr), dimension(10) :: bar
contains
    subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    print *, "Exit called"
!    call gtk_object_destroy(win)
!    call gtk_main_quit ()
    run_status = FALSE
  end subroutine my_destroy

  subroutine pending_events ()
   integer(c_int) :: boolresult
   do while(IAND(gtk_events_pending(), run_status) /= FALSE)
      boolresult = gtk_main_iteration_do(FALSE) ! False for non-blocking
    end do
  end subroutine pending_events

end module handlers

program progress

  ! PROGRESS
  ! Examples of progress bars

  use handlers

  implicit none

  integer :: t0, t1, istep, i, itmax
  real(kind=c_double) :: bval

  ! Initialize gtk & create a window for the heirarchy
  call gtk_init()
  win = f_gtk_window("Progress"//cnull, destroy=c_funloc(my_destroy))

  ! Make a column box to contain our widgets and put it in the window
  box=gtk_vbox_new(FALSE, 0)
  call gtk_container_add(win, box)

  ! Make several horizontal progress bars and put them in the box
  do i=1,10
     bar(i) = f_gtk_progress_bar()
     call gtk_box_pack_start_defaults(box, bar(i))
  end do
  pbar = f_gtk_progress_bar(step=0.05_c_double)
  call gtk_box_pack_start_defaults(box, pbar)

  ! Make a quit button and put that in the box.
  qbut = f_gtk_button("Quit"//cnull, clicked=c_funloc(my_destroy))
  call gtk_box_pack_start_defaults(box, qbut)

  ! Display the window
  call gtk_widget_show_all(Win) 

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
        call f_gtk_progress_bar_set(bar(i), real(istep,c_double), &
             & real(itmax, c_double), string=TRUE)
        if (mod(istep, 20) == 0) &
             & call f_gtk_progress_bar_set(pbar, text="Working"//cnull)
     end do
     if (run_status == FALSE) exit
  end do
  !$omp end parallel do
  call gtk_object_destroy(Win)
end program progress
