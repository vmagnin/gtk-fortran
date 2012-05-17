module handlers
  use iso_c_binding
  use gtk_hl
  use gtk, only: gtk_button_new, gtk_container_add, gtk_label_new, gtk_main, gtk_&
       &main_quit, gtk_notebook_new, gtk_table_attach, gtk_table_new, gtk_widget_destr&
       &oy, gtk_widget_show, gtk_widget_show_all, gtk_window_new, gtk_init

  implicit none

  type(c_ptr) :: win, base, nbook, qbut, table

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    print *, "Exit called"
    call gtk_widget_destroy(win)
    call gtk_main_quit ()
  end subroutine my_destroy

  subroutine bpress(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int), pointer :: fdata

    call c_f_pointer(gdata, fdata)
    print *, "Pressed button ", fdata
  end subroutine bpress

  subroutine add_row(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    call hl_gtk_table_expand(table, ny=1)

  end subroutine add_row
    subroutine add_col(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    call hl_gtk_table_expand(table, nx=1)

  end subroutine add_col
  
end module handlers

program containers
  ! Containers
  ! Test/demo of tables & notebooks.
  use handlers

  implicit none

  integer(kind=c_int) :: ipos
  type(c_ptr) :: junk, jb
  integer :: i
  integer(kind=c_int), dimension(6), target :: bval = (/ (i, i = 1,6) /)
  character(len=15) :: ltext

  ! Initialize GTK+
  call gtk_init()

  ! Create a window that will hold the widget system
  win=hl_gtk_window_new('Table/notebook'//c_null_char, destroy=c_funloc(my_destroy))

  ! Now make a column box & put it into the window
  base = hl_gtk_box_new()
  call gtk_container_add(win, base)

  ! Make a notebook container
  nbook = hl_gtk_notebook_new()
  call hl_gtk_box_pack(base, nbook)

  ! First page is a 3x6 table
  table=hl_gtk_table_new(6, 3, homogeneous=TRUE)
  ipos = hl_gtk_notebook_add_page(nbook, table, label="Example table"//c_null_char)

  do i = 1, 6
     write(ltext, "('Table row',I2)") i
     junk = gtk_label_new(trim(ltext)//c_null_char)
     call hl_gtk_table_attach(table, junk, 0, i-1, xspan=2)
     junk = hl_gtk_button_new("Press"//c_null_char, clicked=c_funloc(bpress), &
          & data = c_loc(bval(i)))
     call hl_gtk_table_attach(table, junk, 2, i-1)
  end do

  ! Then 4 relocatable dummy pages
  do i = 1,4
     write(ltext, "('Dummy page',I2)") i+1
     junk = gtk_label_new(trim(ltext)//c_null_char)

     ipos = hl_gtk_notebook_add_page(nbook, junk, label=trim(ltext)//c_null_char, &
          & reorderable=TRUE)
  end do

  ! Add rows / columns
  jb = hl_gtk_box_new(horizontal=TRUE)
  junk = hl_gtk_button_new("Add Row"//c_null_char, clicked=c_funloc(add_row))
  call hl_gtk_box_pack(jb, junk)
  junk = hl_gtk_button_new("Add col"//c_null_char, clicked=c_funloc(add_col))
  call hl_gtk_box_pack(jb, junk)
  call hl_gtk_box_pack(base, jb, expand=FALSE)

  ! And a quit button
  junk = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(my_destroy))
  call hl_gtk_box_pack(base, junk, expand=FALSE)

  ! realize the window

  call gtk_widget_show_all(win)

  ! Event loop

  call gtk_main()
end program containers
