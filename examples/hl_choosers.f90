! Demo of file choosers.

module handlers
  use gtk_hl

  use gtk, only: gtk_button_new, gtk_container_add, gtk_main, gtk_main_quit, gtk_&
       &text_view_new, gtk_widget_set_sensitive, gtk_widget_show, gtk_widget_show_all,&
       & gtk_window_new, gtk_init, TRUE, FALSE

  use g, only: alloca

  implicit none

  ! Those widgets that need to be addressed explicitly in the handlers

  type(c_ptr) :: window, sbut, sabut, tedit

  ! Other variables that need to be shared between handlers

  logical, private :: file_is_changed = .FALSE.
  character(len=120), private :: filename

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    integer(kind=c_int) :: ok
    character(len=60), dimension(4) :: msg

    msg(1) = "File is changed"
    msg(2) = ""
    msg(3) = "Quitting now will destroy your changes"
    msg(4) = "Do you really want to quit"

    if (file_is_changed) then
       ok = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_YES_NO, &
            & "Really Quit"//cnull, parent=window)
       if (ok == GTK_RESPONSE_NO) return
    end if

    print *, "Exit called"
    call gtk_main_quit ()
  end subroutine my_destroy

  subroutine open_file(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: isel
    character(len=120), dimension(:), allocatable :: chfile
    character(len=30), dimension(2) :: filters
    character(len=30), dimension(2) :: filtnames
    character(len=200) :: inln
    integer :: ios
    integer :: idxs

    filters(1) = "*.txt,*.lis"
    filters(2) = "*.f90"
    filtnames(1) = "Text files"
    filtnames(2) = "Fortran code"

    isel = hl_gtk_file_chooser_show(chfile, create=FALSE,&
         & title="Select input file"//cnull, filter=filters, &
         & filter_name=filtnames, edit_filters=TRUE, &
         & parent=window)

    if (isel == FALSE) return   ! No selection made

    filename = chfile(1)
    deallocate(chfile)

    open(37, file=filename, action='read')
    call hl_gtk_text_view_delete(tedit)
    do
       read(37,"(A)",iostat=ios) inln
       if (ios /= 0) exit
       call hl_gtk_text_view_insert(tedit, (/ trim(inln)//c_new_line /))
    end do
    close(37)
    idxs = index(filename, '/', .true.)+1
    call gtk_window_set_title(window, trim(filename(idxs:))//cnull)

    ! We manually reset the changed flag as the text box signal handler sets it.

    file_is_changed = .FALSE.
    call gtk_widget_set_sensitive(sabut, TRUE)
    call gtk_widget_set_sensitive(sbut, FALSE)
  end subroutine open_file

  subroutine do_open(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    type(c_ptr) :: c_string
    character(len=200) :: inln
    integer :: ios
    integer :: idxs

    c_string = gtk_file_chooser_get_filename(widget)
    call convert_c_string(c_string, len(filename), filename)
    call g_free(c_string)

    open(37, file=filename, action='read')
    call hl_gtk_text_view_delete(tedit)
    do
       read(37,"(A)",iostat=ios) inln
       if (ios /= 0) exit
       call hl_gtk_text_view_insert(tedit, (/ trim(inln)//c_new_line /))
    end do
    close(37)
    idxs = index(filename, '/', .true.)+1
    call gtk_window_set_title(window, trim(filename(idxs:))//cnull)

    ! We manually reset the changed flag as the text box signal handler sets it.

    file_is_changed = .FALSE.
    call gtk_widget_set_sensitive(sabut, TRUE)
    call gtk_widget_set_sensitive(sbut, FALSE)
  end subroutine do_open

  subroutine save_file(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    character(len=200), dimension(:), allocatable :: text
    integer :: i

    call hl_gtk_text_view_get_text(tedit, text)

    open(37, file=filename, action='write')
    do i = 1, size(text)
       write(37, '(A)') trim(text(i))
    end do
    close(37)
    deallocate(text)

    file_is_changed = .FALSE.
    call gtk_widget_set_sensitive(widget, false)
  end subroutine save_file

  subroutine save_file_as(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: isel
    character(len=120), dimension(:), allocatable :: chfile
    character(len=20), dimension(2) :: filters
    character(len=30), dimension(2) :: filtnames
    character(len=200) :: inln
    integer :: ios, i
    character(len=200), dimension(:), allocatable :: text
    integer :: idxs

    filters(1) = "*.txt,*.lis"
    filters(2) = "*.f90"
    filtnames(1) = "Text files"
    filtnames(2) = "Fortran code"

    isel = hl_gtk_file_chooser_show(chfile, create=TRUE,&
         & title="Select input file"//cnull, filter=filters, &
         & filter_name=filtnames, initial_file=trim(filename)//cnull, &
         & confirm_overwrite=TRUE, all=TRUE, parent=window)

    if (isel == FALSE) return   ! No selection made

    filename = chfile(1)
    deallocate(chfile)
    idxs = index(filename, '/', .true.)+1
    call gtk_window_set_title(window, trim(filename(idxs:))//cnull)
    call hl_gtk_text_view_get_text(tedit, text)

    open(37, file=filename, action='write')
    do i = 1, size(text)
       write(37, '(A)') trim(text(i))
    end do
    close(37)
    deallocate(text)

    file_is_changed = .FALSE.
    call gtk_widget_set_sensitive(sbut, false)
  end subroutine save_file_as

  subroutine file_edited(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    file_is_changed = .true.
    call gtk_widget_set_sensitive(sbut, TRUE)
  end subroutine file_edited
  
end module handlers

program choosers_demo
  use handlers

  implicit none

  ! Widgets that don't need to be global
  type(c_ptr) :: base, jb, junk

  ! Filters for the chooser button
  character(len=30), dimension(3) :: filters
  character(len=30), dimension(3) :: filtnames

  filters(1) = "text/plain"
  filters(2) = "*.f90"
  filters(3) = "*"
  filtnames(1) = "Text files"
  filtnames(2) = "Fortran code"
  filtnames(3) = "All files"

  ! Initialize GTK
  call gtk_init()

  ! Create a window and a column box

  window = hl_gtk_window_new("Choosers Demo"//cnull, &
       & destroy=c_funloc(my_destroy))

  base = hl_gtk_box_new()
  call gtk_container_add(window, base)

  ! A row of buttons

  jb = hl_gtk_box_new(horizontal=TRUE, homogeneous=TRUE)
  call hl_gtk_box_pack(base, jb)
  junk = hl_gtk_button_new("Open"//cnull, clicked=c_funloc(open_file))
  call hl_gtk_box_pack(jb, junk)
  junk = hl_gtk_file_chooser_button_new(title="Alt-open"//cnull, &
       & filter=filters, filter_name=filtnames, file_set=c_funloc(do_open))
  call hl_gtk_box_pack(jb, junk)
  sbut = hl_gtk_button_new("Save"//cnull, clicked=c_funloc(save_file),&
       & sensitive=FALSE)
  call hl_gtk_box_pack(jb, sbut)
  sabut = hl_gtk_button_new("Save as"//cnull, clicked=c_funloc(save_file_as), &
       & sensitive=FALSE)
  call hl_gtk_box_pack(jb, sabut)

  ! A multiline text editor in which to display the file.

  tedit = hl_gtk_text_view_new(jb, editable=TRUE, &
       & changed=c_funloc(file_edited), ssize = (/ 750, 400 /) )
  call hl_gtk_box_pack(base, jb)

  ! A quit button
  junk = hl_gtk_button_new("Quit"//cnull, clicked=c_funloc(my_destroy))
  call hl_gtk_box_pack(base, junk)

  ! Realise & enter event loop

  call gtk_widget_show_all(window)

  call gtk_main()

end program choosers_demo
