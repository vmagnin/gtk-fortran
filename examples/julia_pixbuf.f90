! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran GTK / Fortran Interface library.
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
! Contributed by Vincent Magnin and Jerry DeLisle
! Last modifications: vmagnin+Ian Harvey 2019-02-21, vmagnin 2021-06-07
!------------------------------------------------------------------------------

module global_widgets
  use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int
  type(c_ptr) :: my_pixbuf, my_drawing_area, spinButton1, spinButton2, spinButton3
  type(c_ptr) :: textView, buffer, scrolled_window, statusBar, combo1
  character(kind=c_char), dimension(:), pointer :: pixel
  integer(kind=c_int) :: nch, rowstride, width, height, pixwidth, pixheight
  logical :: computing = .false.
  character(LEN=80) :: string
end module global_widgets

module handlers
  use gtk, only: gtk_application_window_new, gtk_window_destroy, &
  & g_signal_connect, g_signal_connect_swapped, &
  & gtk_window_set_child, gtk_expander_set_child, gtk_box_append, &
  & gtk_scrolled_window_set_child, gtk_drawing_area_new, &
  & gtk_drawing_area_set_content_width, gtk_drawing_area_set_content_height, &
  & gtk_drawing_area_set_draw_func, &
  & gtk_widget_queue_draw, gtk_widget_show, &
  & gtk_window_set_default_size, gtk_window_set_title, &
  & TRUE, FALSE, GDK_COLORSPACE_RGB, &
  & gtk_grid_new, gtk_grid_attach, gtk_button_new_with_label,&
  & gtk_box_new, gtk_spin_button_new,&
  & gtk_adjustment_new, gtk_spin_button_get_value, gtk_label_new, &
  & gtk_expander_new_with_mnemonic, gtk_expander_set_expanded, &
  & gtk_toggle_button_new_with_label, gtk_toggle_button_get_active, gtk_notebook_new,&
  & gtk_notebook_append_page, gtk_text_view_new, gtk_text_view_get_buffer, &
  & gtk_text_buffer_set_text, gtk_scrolled_window_new, &
  & gtk_text_buffer_insert_at_cursor, gtk_statusbar_new, &
  & gtk_statusbar_push, gtk_statusbar_pop, gtk_statusbar_get_context_id, &
  & gtk_button_new_with_mnemonic, gtk_link_button_new_with_label, &
  & gtk_toggle_button_new_with_mnemonic, gtk_label_new_with_mnemonic, &
  & gtk_window_set_mnemonics_visible, gtk_combo_box_text_new, &
  & gtk_combo_box_text_append_text, gtk_combo_box_text_get_active_text, &
  & gtk_combo_box_text_insert_text, gtk_spin_button_set_value, gtk_spin_button_update,&
  & GTK_ORIENTATION_VERTICAL, gtk_grid_set_column_homogeneous, &
  & gtk_grid_set_row_homogeneous, gtk_statusbar_remove_all, &
  & gtk_widget_set_vexpand

  use g, only: g_usleep, g_main_context_iteration, g_main_context_pending
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_null_ptr, c_null_char, C_NEW_LINE

  implicit none
  type(c_ptr)    :: my_window
  ! run_status is TRUE until the user closes the top window:
  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult

contains
  ! Our callback function before destroying the window:
  recursive subroutine destroy_signal(widget, event, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, event, gdata

    print *, "Your destroy_signal() function has been invoked !"
    ! Some functions and subroutines need to know that it's finished:
    run_status = FALSE
    call gtk_window_destroy(my_window)
  end subroutine destroy_signal

  ! This function is needed to update the GUI during long computations.
  ! https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html
  recursive subroutine pending_events ()
    do while(IAND(g_main_context_pending(c_null_ptr), run_status) /= FALSE)
      ! FALSE for non-blocking:
      boolresult = g_main_context_iteration(c_null_ptr, FALSE)
    end do
  end subroutine pending_events

  ! "It is called whenever GTK needs to draw the contents of the drawing area
  ! to the screen."
  ! https://developer.gnome.org/gtk4/stable/GtkDrawingArea.html#gtk-drawing-area-set-draw-func
  subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)
    use cairo, only: cairo_paint
    use gdk, only: gdk_cairo_set_source_pixbuf
    use global_widgets, only: my_pixbuf

    type(c_ptr), value, intent(in)    :: widget, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: width, height

    ! We redraw the pixbuf:
    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 0d0, 0d0)
    call cairo_paint(my_cairo_context)
  end subroutine my_draw_function

  ! GtkButton signal emitted by the "Compute" button
  ! In this example, this function is declared recursive because it can be
  ! called a second time from the Julia_set() subroutine:
  recursive subroutine firstbutton (widget, gdata ) bind(c)
    use global_widgets

    integer(c_int)                 :: message_id
    type(c_ptr), value, intent(in) :: widget, gdata
    complex(kind(1d0))             :: c
    integer                        :: iterations

    if (.not. computing) then
      ! Get computation parameters:
      c = gtk_spin_button_get_value (spinButton1) + &
          & (0d0, 1d0)*gtk_spin_button_get_value (spinButton2)
      iterations = INT(gtk_spin_button_get_value (spinButton3))

      ! Print them in the text buffer:
      write(string, '("c=",F9.6,"+i*",F9.6,"   ", I8, " iterations")') c, iterations
      call gtk_text_buffer_insert_at_cursor (buffer, string//C_NEW_LINE &
                                           & //c_null_char, -1_c_int)

      message_id = gtk_statusbar_push (statusBar, gtk_statusbar_get_context_id(&
                                     & statusBar, "Julia"//c_null_char),&
                                     & "Computing..."//c_null_char)

      ! Compute the image:
      call Julia_set(-2d0, +2d0, -2d0, +2d0, c, iterations)

      ! If Julia_set() was quitted because of a delete_event, we can not use
      ! the statusBar because it has been destroyed:
      if (run_status == TRUE) then
        message_id = gtk_statusbar_push (statusBar, gtk_statusbar_get_context_id(&
                                       & statusBar, "Julia"//c_null_char),&
                                       & "Finished."//c_null_char)
      end if
    else
      print *, "Already computing !"
    end if
  end subroutine firstbutton

  ! GtkComboBox signal emitted when the user selects predifined c values of
  ! interesting Julia sets in the combo box:
  subroutine firstCombo (widget, gdata ) bind(c)
    use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
    use gtk_sup, only: c_f_string_copy
    use global_widgets

    type(c_ptr), value, intent(in) :: widget, gdata
    real(c_double)                 :: x, y
    integer                        :: choice
    character(len=512)             :: my_string

    ! Get the value selected by the user:
    call c_f_string_copy( gtk_combo_box_text_get_active_text(combo1), my_string)
    read(my_string, *) choice

    select case (choice)
    case(1)
      x = +0d0
      y = +1d0
    case(2)
      x = -1d0
      y = +0d0
    case(3)
      x = -0.8d0
      y = +0.2d0
    case(4)
      x = +0.39d0
      y = +0.60d0
    case(5)
      x = -0.2d0
      y = +0.8d0
    case(6)
      x = -0.8d0
      y = +0.4d0
    case(7)
      x = +0.39d0
      y = +0.00d0
    end select

    ! Update the spin buttons real(c) and imag(c):
    call gtk_spin_button_set_value (spinButton1, x)
    call gtk_spin_button_set_value (spinButton2, y)
  end subroutine firstCombo

  ! GtkButton signal emitted by the button "Save as PNG":
  subroutine secondbutton (widget, gdata) bind(c)
    use gtk_os_dependent, only: gdk_pixbuf_savev
    use global_widgets

    type(c_ptr), value, intent(in) :: widget, gdata
    integer(c_int)                 :: cstatus, message_id

    ! Save the picture if the computation is finished:
    if (.not. computing) then
      ! https://developer.gnome.org/gdk-pixbuf/stable/gdk-pixbuf-File-saving.html
      ! https://mail.gnome.org/archives/gtk-list/2004-October/msg00186.html
      cstatus = gdk_pixbuf_savev(my_pixbuf, "julia.png"//c_null_char, &
                & "png"//c_null_char, c_null_ptr, c_null_ptr, c_null_ptr);

      if (cstatus == TRUE) then
        string = "Successfully saved: julia.png"//c_null_char
      else
        string = "Failed"//c_null_char
      end if

      message_id = gtk_statusbar_push (statusBar, gtk_statusbar_get_context_id(&
                         & statusBar, "Julia"//c_null_char), TRIM(string))
    end if
  end subroutine secondbutton

  ! GtkToggleButton signal emitted when the "Pause" button is clicked.
  ! This function is declared recursive because it will be pressed a first time
  ! to pause and a second time to end pause (from the "do while" loop):
  recursive subroutine firstToggle (widget, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_long
    use global_widgets

    type(c_ptr), value, intent(in) :: widget, gdata
    integer(c_int)                 :: message_id

    ! The button is pressed:
    if (gtk_toggle_button_get_active(widget) == TRUE) then
      ! Print messages:
      call gtk_text_buffer_insert_at_cursor (buffer, &
              & "In pause"//C_NEW_LINE//c_null_char, -1_c_int)
      message_id = gtk_statusbar_push (statusBar, gtk_statusbar_get_context_id(&
                                      &statusBar, "Julia"//c_null_char), &
                                      &"In pause..."//c_null_char)
      ! The Pause loop must handle events in order to avoid the blocking of
      ! the GUI:
      do while (gtk_toggle_button_get_active(widget) == TRUE)
        call pending_events
        if (run_status == FALSE) exit ! Exit if we had a destroy signal
        call g_usleep(50000_c_long)   ! In microseconds
      end do
    else  ! The button is raised:
      call gtk_text_buffer_insert_at_cursor (buffer, &
                            & "Not in pause"//C_NEW_LINE//c_null_char, -1_c_int)
      ! Remove the "In pause..." message from the status bar:
      call gtk_statusbar_pop (statusBar, gtk_statusbar_get_context_id(statusBar,&
                            & "Julia"//c_null_char))
    end if
  end subroutine firstToggle

  ! Callback function for the signal "activate" emitted by g_application_run().
  ! We use a subroutine because it should return void.
  ! The GUI is defined here.
  subroutine activate(app, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_f_pointer, c_null_funptr
    use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, &
                      & gdk_pixbuf_get_rowstride, gdk_pixbuf_new
    use global_widgets
    implicit none
    type(c_ptr), value, intent(in)  :: app, gdata
    ! Pointers toward our GTK widgets:
    type(c_ptr)    :: table, button1, button2, button3, box1
    type(c_ptr)    :: label1, label2, label3, label4
    type(c_ptr)    :: toggle1, expander, notebook, notebookLabel1, notebookLabel2
    type(c_ptr)    :: linkButton
    integer(c_int) :: message_id, firstTab, secondTab

    ! Create the window:
    my_window = gtk_application_window_new(app)
    call g_signal_connect(my_window, "destroy"//c_null_char, &
                        & c_funloc(destroy_signal))
   ! Don't forget that C strings must end with a null char:
    call gtk_window_set_title(my_window, "Julia Set"//c_null_char)
    ! Properties of the main window :
    width  = 700
    height = 700
    call gtk_window_set_default_size(my_window, width, height)

    !******************************************************************
    ! Adding widgets in the window:
    !******************************************************************
    ! The four buttons:
    button1 = gtk_button_new_with_mnemonic ("_Compute"//c_null_char)
    call g_signal_connect (button1, "clicked"//c_null_char, c_funloc(firstbutton))
    button2 = gtk_button_new_with_mnemonic ("_Save as PNG"//c_null_char)
    call g_signal_connect (button2, "clicked"//c_null_char, c_funloc(secondbutton))
    button3 = gtk_button_new_with_mnemonic ("_Exit"//c_null_char)
    call g_signal_connect (button3, "clicked"//c_null_char, c_funloc(destroy_signal))
    toggle1 = gtk_toggle_button_new_with_mnemonic ("_Pause"//c_null_char)
    call g_signal_connect (toggle1, "toggled"//c_null_char, c_funloc(firstToggle))

    ! The spin buttons to set the parameters:
    label1 = gtk_label_new("real(c)"//c_null_char)
    spinButton1 = gtk_spin_button_new (gtk_adjustment_new(-0.835d0,-2d0,+2d0,0.05d0,0.5d0,0d0),0.05d0, 7_c_int)
    label2 = gtk_label_new("imag(c) "//c_null_char)
    spinButton2 = gtk_spin_button_new (gtk_adjustment_new(-0.2321d0, -2d0,+2d0,0.05d0,0.5d0,0d0),0.05d0, 7_c_int)
    label3 = gtk_label_new("iterations"//c_null_char)
    spinButton3 = gtk_spin_button_new (gtk_adjustment_new(100000d0,1d0,+1000000d0,10d0,100d0,0d0),10d0, 0_c_int)

    ! The combo box with predifined values of interesting Julia sets:
    label4 = gtk_label_new("Predefined values:"//c_null_char)
    combo1 = gtk_combo_box_text_new()
    call gtk_combo_box_text_append_text(combo1, "1"//c_null_char)
    call gtk_combo_box_text_append_text(combo1, "2"//c_null_char)
    call gtk_combo_box_text_append_text(combo1, "3"//c_null_char)
    call gtk_combo_box_text_append_text(combo1, "4"//c_null_char)
    call gtk_combo_box_text_append_text(combo1, "5"//c_null_char)
    call gtk_combo_box_text_append_text(combo1, "6"//c_null_char)
    call gtk_combo_box_text_append_text(combo1, "7"//c_null_char)
    call g_signal_connect (combo1, "changed"//c_null_char, c_funloc(firstCombo))

    ! A clickable URL link:
    linkButton = gtk_link_button_new_with_label ( &
                          &"http://en.wikipedia.org/wiki/Julia_set"//c_null_char,&
                          &"More on Julia sets"//c_null_char)

    ! A table container will contain buttons and labels:
    table = gtk_grid_new ()
    call gtk_grid_set_column_homogeneous(table, TRUE)
    call gtk_grid_set_row_homogeneous(table, TRUE)
    call gtk_grid_attach(table, button1, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, button2, 1_c_int, 3_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, button3, 3_c_int, 3_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label1, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label2, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label3, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton1, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton2, 1_c_int, 1_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton3, 1_c_int, 2_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, linkButton, 3_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label4, 2_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, combo1, 2_c_int, 1_c_int, 1_c_int,1_c_int)
    call gtk_grid_attach(table, toggle1, 2_c_int, 3_c_int, 1_c_int,1_c_int)

    ! The table is contained in an expander, which is contained in the vertical box:
    expander = gtk_expander_new_with_mnemonic ("_The parameters:"//c_null_char)
    call gtk_expander_set_child(expander, table)
    call gtk_expander_set_expanded(expander, TRUE)

    ! We create a vertical box container:
    box1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10_c_int);
    call gtk_box_append(box1, expander)

    ! We need a widget where to draw our pixbuf.
    ! The drawing area is contained in the vertical box:
    pixwidth  = 500
    pixheight = 500
    my_drawing_area = gtk_drawing_area_new()
    call gtk_drawing_area_set_content_width(my_drawing_area, pixwidth)
    call gtk_drawing_area_set_content_height(my_drawing_area, pixheight)
    call gtk_drawing_area_set_draw_func(my_drawing_area, &
                     & c_funloc(my_draw_function), c_null_ptr, c_null_funptr)

    ! We define a notebook with two tabs "Graphics" and "Messages":
    notebook = gtk_notebook_new ()
    call gtk_widget_set_vexpand (notebook, TRUE)
    notebookLabel1 = gtk_label_new_with_mnemonic("_Graphics"//c_null_char)
    firstTab = gtk_notebook_append_page (notebook, my_drawing_area, notebookLabel1)

    textView = gtk_text_view_new ()
    buffer = gtk_text_view_get_buffer (textView)
    call gtk_text_buffer_set_text (buffer, "Julia Set"//C_NEW_LINE// &
        & "You can copy this text and even edit it !"//C_NEW_LINE//c_null_char,&
        & -1_c_int)
    scrolled_window = gtk_scrolled_window_new()
    notebookLabel2 = gtk_label_new_with_mnemonic("_Messages"//c_null_char)
    call gtk_scrolled_window_set_child(scrolled_window, textView)
    secondTab = gtk_notebook_append_page (notebook, scrolled_window, notebookLabel2)

    call gtk_box_append(box1, notebook)
    call gtk_widget_set_vexpand (box1, TRUE)

    ! The window status bar can be used to print messages:
    statusBar = gtk_statusbar_new ()
    message_id = gtk_statusbar_push (statusBar, gtk_statusbar_get_context_id(statusBar, &
                & "Julia"//c_null_char), "Waiting..."//c_null_char)
    call gtk_box_append(box1, statusBar)

    ! Let's finalize the GUI:
    call gtk_window_set_child(my_window, box1)
    call gtk_window_set_mnemonics_visible (my_window, TRUE)
    call gtk_widget_show(my_window)

    ! We create a "pixbuffer" to store the pixels of the image:
    my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, pixwidth, pixheight)
    nch = gdk_pixbuf_get_n_channels(my_pixbuf)
    rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
    call c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, [pixwidth*pixheight*nch])
    ! We use char() for "pixel" because we need unsigned integers.
    ! This pixbuffer has no Alpha channel (15% faster), only RGB.
    pixel = char(0)
    !******************************************************************

    ! If you don't show it, nothing will appear on screen...
    call gtk_widget_show(my_window)
  end subroutine activate

  !*********************************************
  ! Julia Set
  ! http://en.wikipedia.org/wiki/Julia_set
  ! The scientific computing is done here
  !*********************************************
  subroutine Julia_set(xmin, xmax, ymin, ymax, c, itermax)
    use, intrinsic :: iso_c_binding
    use global_widgets

    integer    :: i, j, k, p, itermax
    double precision :: x, y, xmin, xmax, ymin, ymax ! coordinates in the complex plane
    complex(kind(1d0)) :: c, z
    double precision :: scx, scy       ! scales
    integer(1) :: red, green, blue     ! rgb color
    double precision :: t0, t1

    computing = .true.
    call cpu_time(t0)

    scx = ((xmax - xmin) / pixwidth)   ! x scale
    scy = ((ymax - ymin) / pixheight)  ! y scale

    ! We compute the colour of each pixel (i,j):
    do i=0, pixwidth-1
      ! We provoke a draw event only once in a while to improve performances:
      if (mod(i,10) == 0) then
        call gtk_widget_queue_draw(my_drawing_area)
      end if

      x = xmin + scx * i
      do j=0, pixheight-1
        y = ymin + scy * j
        z = x + y*(0d0,1d0)   ! Starting point
        k = 1
        do while ((k <= itermax) .and. ((real(z)**2+aimag(z)**2)<4d0))
          z = z*z + c
          k = k + 1
        end do

        if (k>itermax) then
          ! Black pixel:
          red   = 0
          green = 0
          blue  = 0
        else
          ! Colour palette:
          red   = int(min(255, k*2),  KIND=1)
          green = int(min(255, k*5),  KIND=1)
          blue  = int(min(255, k*10), KIND=1)
        end if

        ! We write in the pixbuffer:
        p = i * nch + j * rowstride + 1
        pixel(p)   = char(red)
        pixel(p+1) = char(green)
        pixel(p+2) = char(blue)

        ! This subroutine processes GTK events that occurs during the computation:
        call pending_events()
        if (run_status == FALSE) return ! Exit subroutine if we had a delete event.
      end do
    end do

    ! Final update of the display:
    call gtk_widget_queue_draw(my_drawing_area)

    computing = .false.

    call cpu_time(t1)
    write(string, '("System time = ",F8.3, " s")') t1-t0
    call gtk_text_buffer_insert_at_cursor (buffer, &
                                      & string//C_NEW_LINE//c_null_char, -1_c_int)
  end subroutine Julia_set

end module handlers

!*******************************************************************************
! In the main program, we declare the GTK application, connect it to its
! "activate" function where we will create the GUI,
! and finally call the GLib main loop.
!*******************************************************************************
program julia_pixbuf

  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char, c_null_ptr
  ! We will use those GTK functions and values. The "only" statement can improve
  ! significantly the compilation time:
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use handlers

  implicit none
  integer(c_int)     :: status
  type(c_ptr)        :: app

  ! First, let's create a GTK application (it will initialize GTK).
  ! The application ID must contain at least one point:
  ! https://developer.gnome.org/gio/stable/GApplication.html#g-application-id-is-valid
  app = gtk_application_new("gtk-fortran.examples.julia_pixbuf"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)
  ! The activate signal will be sent by g_application_run().
  ! The c_funloc() function returns the C address of the callback function.
  ! The c_null_ptr means no data is transfered to the callback function.
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)
  ! Now, the whole application will be managed by GLib (=> main loop).
  ! Note that commandline arguments argc, argv are not passed.
  ! https://developer.gnome.org/gio/stable/GApplication.html#g-application-run
  status = g_application_run(app, 0_c_int, [c_null_ptr])

  print *, "You have exited the GLib main loop, bye, bye..."

  ! Memory is freed:
  call g_object_unref(app)

end program julia_pixbuf

