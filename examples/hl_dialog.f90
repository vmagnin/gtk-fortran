module handlers
  use gtk_hl
  use gtk, only: gtk_button_new, gtk_container_add, gtk_main, gtk_main_quit, gtk_&
       &object_destroy, gtk_widget_show, gtk_widget_show_all, gtk_window_new, &
       & gtk_init

  implicit none

  type(c_ptr) :: win, box, but, kbut, label

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    print *, "Exit called"
!    call gtk_widget_destroy(win)
    call gtk_main_quit ()
  end subroutine my_destroy

  subroutine msg_alert(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: resp

    character(len=40), dimension(5) :: msg

    msg(1) = "ALERT"
    msg(2) = ""
    msg(3) = "You have pressed an alert button"
    msg(4) = ""
    msg(5) = "You know that's dangerous"

    resp = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_OK, "ALERT"//cnull)
  end subroutine msg_alert

  subroutine msg_quit(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: resp

    character(len=40), dimension(3) :: msg

    msg(1) ="QUIT?"
    msg(2) = ""
    msg(3) = "Do you really want to quit?"

    resp = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_YES_NO, "QUIT"//cnull)
    if (resp == GTK_RESPONSE_YES) call gtk_main_quit()

  end subroutine msg_quit
end module handlers

program dialog_demo
  use handlers
  implicit none

  call gtk_init()
  ! Make a window & put a horizontal box in it
  win = hl_gtk_window_new('Dialogue Demo'//cnull, destroy=c_funloc(my_destroy))
  box = hl_gtk_box_new(horizontal=TRUE)
  call gtk_container_add(win, box)

  ! 2 Buttons one shows a message, the other a confirm exit dialog
  but = hl_gtk_button_new('Alert'//cnull, clicked=c_funloc(msg_alert))
  call hl_gtk_box_pack(box, but)

  kbut = hl_gtk_button_new('Quit'//cnull, clicked=c_funloc(msg_quit))
  call hl_gtk_box_pack(box, kbut)

  ! Display the window
  call gtk_widget_show_all(Win)

  call gtk_main()
end program dialog_demo
