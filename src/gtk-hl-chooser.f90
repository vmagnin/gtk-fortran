! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran GTK+ Fortran Interface library.
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
! Contributed by James Tappin
! Last modifications: 2012-12-31, vmagnin 2020-06-19 (GTK 4 version),
!                     2020-08-25
!------------------------------------------------------------------------------

!*
! File Choosers
module gtk_hl_chooser
  ! hl_gtk_file_chooser_button_new implements the GtkFileChooserButton
  ! and its GtkFileChooser options in a convenient package.
  !
  ! hl_gtk_file_chooser_new and _show implement a more general chooser
  ! dialogue via the file_chooser_widget (file_choose_dialog only has
  ! variadic constructors). Unless you need to add extra items to the
  ! dialog it is usually easiest to use the hl_gtk_file_chooser_show function.
  !
  ! Filters may be either patterns (e.g. '*.f90' or '2011*.lis') or mime types
  ! (e.g. 'image/png' or 'text/*'). The constructors recognise the difference by
  ! the presence or absence of a '/' character. Each filter is a
  ! comma-separated list, which may contain any mixture of patterns and mime
  ! types (e.g. '*.png,image/tiff,*.jpg'). If a name is not provided, then
  ! the filter specification is used as the name.
  !/

  use gtk_sup
  use iso_c_binding
  use iso_fortran_env, only: error_unit

  ! Auto generated use's
  use g, only: g_free, g_slist_free, g_slist_length, g_slist_nth_data, &
             & g_main_loop_new, g_main_loop_run, g_main_loop_quit, &
             & g_file_new_for_path, g_file_get_path, g_object_unref
  use gtk, only: gtk_box_append, gtk_dialog_add_button, &
       & gtk_dialog_get_content_area, gtk_dialog_new, &
       & gtk_entry_get_buffer, gtk_entry_buffer_set_text, &
       & gtk_file_chooser_add_filter, &
       & gtk_file_chooser_button_new, gtk_file_chooser_button_set_width_chars, &
       & gtk_file_chooser_get_current_folder, &
       & gtk_file_chooser_set_current_folder, &
       & gtk_file_chooser_set_current_name, gtk_file_chooser_set_file, &
       & gtk_file_chooser_set_select_multiple, &
       & gtk_file_chooser_widget_new, gtk_file_chooser_get_files, &
       & gtk_file_filter_add_mime_type, gtk_file_filter_add_pattern, &
       & gtk_file_filter_new, gtk_file_filter_set_name, gtk_label_new, &
       & gtk_window_destroy, gtk_widget_set_sensitive, &
       & gtk_widget_set_tooltip_text, gtk_widget_show, &
       & gtk_window_set_default_size, gtk_window_set_destroy_with_parent, &
       & gtk_window_set_modal, gtk_window_set_title, &
       & gtk_window_set_transient_for, g_signal_connect, TRUE, FALSE, &
       & GTK_RESPONSE_DELETE_EVENT, GTK_RESPONSE_CANCEL, GTK_RESPONSE_APPLY, &
       & GTK_FILE_CHOOSER_ACTION_OPEN, GTK_FILE_CHOOSER_ACTION_SAVE, &
       & GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER

  ! Building the chooser uses a number of other high-level interfaces.

  use gtk_hl_container
  use gtk_hl_entry
  use gtk_hl_button

  implicit none
  type(c_ptr) :: dialog_gmainloop
  !+
  type, bind(c) :: hl_gtk_chooser_info
     type(c_ptr) :: chooser=C_NULL_PTR, chooser_sel_list=C_NULL_PTR
     type(c_ptr) :: chooser_curdir=C_NULL_PTR, fentry=C_NULL_PTR
     integer(kind=c_int) :: iselect=0
  end type hl_gtk_chooser_info

  ! These items must be shared between the file chooser widget and its event
  ! handler or the filter editor. They are passed to the signal handlers
  ! via the user data argument. Even though it's never used in the C code,
  ! it still has to be bind(c) otherwise c_loc() will croak on it.
  !-

contains
  !+
  function hl_gtk_file_chooser_button_new(directory, title, &
       & width, show_hidden, initial_dir, current, &
       & initial_folder, initial_file, filter, filter_name, file_set, &
       & data, sensitive, tooltip) result(cbutton)

    type(c_ptr) :: cbutton
    integer(kind=c_int), intent(in), optional :: directory
    character(kind=c_char), dimension(*), optional, intent(in) :: title
    integer(kind=c_int), intent(in), optional :: width
    integer(kind=c_int), intent(in), optional :: show_hidden, current
    character(kind=c_char), dimension(*), optional, intent(in) :: &
         & initial_folder, initial_file, initial_dir
    character(len=*), dimension(:), intent(in), optional :: filter
    character(len=*), dimension(:), optional, intent(in) :: filter_name
    type(c_funptr), optional :: file_set
    type(c_ptr), optional :: data
    integer(kind=c_int), intent(in), optional :: sensitive
    character(kind=c_char), dimension(*), optional, intent(in) :: tooltip

    ! Bundled file chooser button
    !
    ! DIRECTORY: boolean: optional: Set to TRUE to select directories rather
    ! 		than files.
    ! TITLE: string: optional: A title for the button.
    ! WIDTH: c_int: optional: A maximum number of characters to show.
    ! SHOW_HIDDEN: boolean: optional: GTK<=3: Set to TRUE to display hidden files.
    ! INITIAL_DIR: string: optional: Use to start the search other than
    ! 		in the current directory. (INITIAL_FOLDER is a deprecated
    ! 		alias).
    ! CURRENT: boolean: optional: Use to force start in current directory.
    ! INITIAL_FILE: string: optional: An initial file selection.
    ! FILTER: string(): optional: An initial list of filename patterns to
    ! 		allow. Each filter is a comma-separated list.
    ! FILTER_NAME: string(): optional: Names for the filters.
    ! FILE_SET: f_funptr: optional: The callback routine for the "file-set"
    ! 		signal.
    ! DATA: c_ptr: optional: User data to pass to the file_Set callback.
    ! SENSITIVE: boolean: optional: Set to FALSE to make the widget start in an
    ! 		insensitive state.
    ! TOOLTIP: string: optional: A tooltip to display when the pointer is
    ! 		held over the widget.
    !-

    integer(kind=c_int) :: mode, lval
    type(c_ptr) :: gfilter
    integer :: i, idx0, idx1

    if (present(directory)) then
       if (directory == TRUE) then
          mode = GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER
       else
          mode = GTK_FILE_CHOOSER_ACTION_OPEN
       end if
    else
       mode = GTK_FILE_CHOOSER_ACTION_OPEN
    end if

    if (present(title)) then
       cbutton = gtk_file_chooser_button_new(title, mode)
    else  if (mode == GTK_FILE_CHOOSER_ACTION_OPEN) then
       cbutton = gtk_file_chooser_button_new("Choose file"//c_null_char, mode)
    else
       cbutton = gtk_file_chooser_button_new("Choose directory"//c_null_char,&
            & mode)
    end if

    if (present(show_hidden)) then
       lval = show_hidden
       print *, "GTK 4: gtk_file_chooser_set_show_hidden removed"
    else
       lval = FALSE
    end if

    if (present(width)) call &
         & gtk_file_chooser_button_set_width_chars(cbutton, width)

    if (present(initial_dir)) then
       lval = gtk_file_chooser_set_current_folder(cbutton, &
            & g_file_new_for_path(initial_dir), c_null_ptr)
    else if (present(initial_folder)) then
       lval = gtk_file_chooser_set_current_folder(cbutton, &
            & g_file_new_for_path(initial_folder), c_null_ptr)
       write(error_unit, *) "HL_GTK_FILE_CHOOSER_BUTTON_NEW:: "// &
            & "INITIAL_FOLDER is deprecated, INITIAL_DIR is preferred"
    else if (present(current)) then
       if (c_f_logical(current)) &
            & lval = gtk_file_chooser_set_current_folder(cbutton, &
                   & g_file_new_for_path("."//c_null_char), c_null_ptr)
    end if
    if (present(initial_file)) &
         lval = gtk_file_chooser_set_file(cbutton, &
              & g_file_new_for_path(initial_file), c_null_ptr)

    if (present(filter)) then
       do i = 1, size(filter)
          gfilter = gtk_file_filter_new()

          idx0 = 1
          do
             idx1 = index(filter(i),',')-2
             if (idx1 < 0) then
                if (index(filter(i)(idx0:), '/') == 0) then
                   call gtk_file_filter_add_pattern(gfilter, &
                        & trim(adjustl(filter(i)(idx0:)))//c_null_char)
                else
                   call gtk_file_filter_add_mime_type(gfilter, &
                        & trim(adjustl(filter(i)(idx0:)))//c_null_char)
                end if
                exit
             else
                if (index(filter(i)(idx0:idx1), '/') == 0) then
                   call gtk_file_filter_add_pattern(gfilter, &
                        & trim(adjustl(filter(i)(idx0:idx1)))//c_null_char)
                else
                   call gtk_file_filter_add_mime_type(gfilter, &
                        & trim(adjustl(filter(i)(idx0:idx1)))//c_null_char)
                end if
                idx0=idx1+2
             end if
          end do
          if (present(filter_name)) then
             call gtk_file_filter_set_name(gfilter, filter_name(i)//c_null_char)
          else
             call gtk_file_filter_set_name(gfilter, &
                  & trim(filter(i))//c_null_char)
          end if
          call gtk_file_chooser_add_filter(cbutton, gfilter)
       end do
    end if

    if (present(file_set)) then
       if (present(data)) then
          call g_signal_connect(cbutton, "file-set"//c_null_char,&
               & file_set, data)
       else
          call g_signal_connect(cbutton, "file-set"//c_null_char, file_set)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(cbutton, &
         & tooltip)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(cbutton, sensitive)
  end function hl_gtk_file_chooser_button_new

  !+
  function hl_gtk_file_chooser_new(chooser_info, cdir, directory, create, &
       & multiple, allow_uri, show_hidden, confirm_overwrite, title, &
       & initial_dir, current, initial_file, filter, filter_name, parent, &
       & all, wsize, edit_filters) result(dialog)

    type(c_ptr) :: dialog
    type(hl_gtk_chooser_info), intent(out), target :: chooser_info
    character(len=*), intent(out), optional :: cdir
    integer(kind=c_int), intent(in), optional :: directory, create, multiple
    integer(kind=c_int), intent(in), optional :: allow_uri, show_hidden
    integer(kind=c_int), intent(in), optional :: confirm_overwrite
    character(kind=c_char), dimension(*), intent(in), optional :: title, initial_dir, initial_file
    integer(kind=c_int), intent(in), optional :: current
    character(len=*), dimension(:), intent(in), optional :: filter
    character(len=*), dimension(:), intent(in), optional :: filter_name
    type(c_ptr), intent(in), optional :: parent
    integer(kind=c_int), intent(in), optional :: all
    integer(kind=c_int), intent(in), dimension(2), optional :: wsize
    integer(kind=c_int), intent(in), optional :: edit_filters

    ! Create a file chooser widget.
    !
    ! CHOOSER_INFO: hl_gtk_chooser_info: required: IDs and flags of various
    ! 		subwidgets needed to process the dialog actions.
    ! CDIR: string: optional: The directory from which they were chosen.
    ! DIRECTORY: boolean: optional: Set to TRUE to select directories
    ! 		instead of files.
    ! CREATE: boolean: optional: Set to FALSE to prohibit creating new files.
    ! MULTIPLE: boolean: optional: Set to TRUE to allow the selection of
    ! 		multiple files.
    ! ALLOW_URI: boolean: optional: GTK<=3: Set to TRUE to allow nonlocal selections.
    ! SHOW_HIDDEN: boolean: optional: GTK<=3: Set to TRUE to show hidden files.
    ! CONFIRM_OVERWRITE: boolean: optional: GTK<=3: Set to TRUE to request
    ! 		confirmation of an overwrite (only used if CREATE
    ! 		is TRUE).
    ! TITLE: string: optional: Title for the window.
    ! INITIAL_DIR: string: optional: Set the initial directory here instead
    ! 		of the current directory.
    ! CURRENT: boolean: optional: Use to force start in current directory.
    ! INITIAL_FILE: string: optional: Set the initial file selection.
    ! FILTER: string(): optional:  The file selection filter. Elements
    ! 		may either be patterns or mime types. Each filter is a
    ! 		comma-separated list of patterns
    ! FILTER_NAME: string(): optional: Names for the filters
    ! PARENT: c_ptr: optional: Parent window for the dialogue.
    ! ALL: boolean: optional: Set to TRUE to add an all-files filter pattern
    ! WSIZE: c_int(2): optional: Set the size for the dialog.
    ! EDIT_FILTERS: boolean: optional: GTK<=3: Set to TRUE to proves an entry window
    ! 		to add extra filters.
    !-

    type(c_ptr) :: content, junk, gfilter
    integer(kind=c_int) :: icreate, idir, action, lval
    integer(kind=c_int) :: i, idx0, idx1

    ! Create a modal dialogue
    dialog = gtk_dialog_new()
    call gtk_window_set_modal(dialog, TRUE)
    if (present(title)) call gtk_window_set_title(dialog, title)
    if (present(wsize)) then
       call gtk_window_set_default_size(dialog, wsize(1),&
            & wsize(2))
    else
       call gtk_window_set_default_size(dialog, 700_c_int, 500_c_int)
    end if

    if (present(parent)) then
       call gtk_window_set_transient_for(dialog, parent)
       call gtk_window_set_destroy_with_parent(dialog, TRUE)
    end if

    ! Attach the action buttonsa to the dialogue
    junk = gtk_dialog_add_button(dialog, "_Open"//C_NULL_CHAR, GTK_RESPONSE_APPLY)
    junk = gtk_dialog_add_button(dialog, "_Cancel"//C_NULL_CHAR, &
         & GTK_RESPONSE_CANCEL)

    ! Decode the action

    if (present(create)) then
       icreate = create
    else
       icreate = TRUE
    end if

    if (present(directory)) then
       idir = directory
    else
       idir = FALSE
    end if

    if (idir == TRUE) then
       action = GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER
    else
       if (icreate == TRUE) then
          ! "Indicates save mode. The file chooser will let the user pick 
          ! an existing file, or type in a new filename."
          action = GTK_FILE_CHOOSER_ACTION_SAVE
       else
          action = GTK_FILE_CHOOSER_ACTION_OPEN
       end if
    end if

    ! Create the chooser & put it in the content area
    content = gtk_dialog_get_content_area(dialog)
    ! https://developer.gnome.org/gtk4/unstable/GtkFileChooserWidget.html
    chooser_info%chooser = gtk_file_chooser_widget_new(action)
    call gtk_box_append(content, chooser_info%chooser)

    ! Multiple selections
    if (present(multiple)) then
       lval = multiple
    else
       lval = FALSE
    end if
    call gtk_file_chooser_set_select_multiple(chooser_info%chooser, lval)

    ! Hidden files
    if (present(show_hidden)) then
       print *, "Not in GTK 4: show_hidden"
       lval = show_hidden
    else
       lval = FALSE
    end if

    ! Confirm overwrite
    if (icreate == TRUE) then
       if (present(confirm_overwrite)) then
          print *, "Not in GTK 4: confirm_overwrite"
          lval = confirm_overwrite
       else
          lval = FALSE
       end if
    end if

    ! Initial directory (precedes file so if file contains a dir it
    ! will overwrite)
    if (present(initial_dir)) then
       lval = gtk_file_chooser_set_current_folder(chooser_info%chooser, &
            & g_file_new_for_path(initial_dir), c_null_ptr)
    else if (present(current)) then
       if (c_f_logical(current)) then
            lval = gtk_file_chooser_set_current_folder(chooser_info%chooser, &
            & g_file_new_for_path("."//c_null_char), c_null_ptr)
       end if
    end if

    ! Initial file

    if (present(initial_file)) then
       if (action == GTK_FILE_CHOOSER_ACTION_SAVE) then
          call gtk_file_chooser_set_current_name(chooser_info%chooser, &
               & initial_file)
       else
          lval = gtk_file_chooser_set_file(chooser_info%chooser, &
               & g_file_new_for_path(initial_file), c_null_ptr)
       end if
    end if

    ! Set up filters
    if (present(filter)) then
       do i = 1, size(filter)
          gfilter = gtk_file_filter_new()

          idx0 = 1
          do
             idx1 = index(filter(i)(idx0:),',')+idx0-2
             if (idx1 < idx0) then
                if (index(filter(i)(idx0:), '/') == 0) then
                   call gtk_file_filter_add_pattern(gfilter, &
                        & trim(adjustl(filter(i)(idx0:)))//c_null_char)
                else
                   call gtk_file_filter_add_mime_type(gfilter, &
                        & trim(adjustl(filter(i)(idx0:)))//c_null_char)
                end if
                exit
             else
                if (index(filter(i)(idx0:idx1), '/') == 0) then
                   call gtk_file_filter_add_pattern(gfilter, &
                        & trim(adjustl(filter(i)(idx0:idx1)))//c_null_char)
                else
                   call gtk_file_filter_add_mime_type(gfilter, &
                        & trim(adjustl(filter(i)(idx0:idx1)))//c_null_char)
                end if
                idx0=idx1+2
             end if
          end do
          if (present(filter_name)) then
             call gtk_file_filter_set_name(gfilter, &
                  & trim(filter_name(i))//c_null_char)
          else
             call gtk_file_filter_set_name(gfilter, &
                  & trim(filter(i))//c_null_char)
          end if
          call gtk_file_chooser_add_filter(chooser_info%chooser, gfilter)
       end do
       if (present(all)) then
          if (all == TRUE) then
             gfilter = gtk_file_filter_new()
             call gtk_file_filter_add_pattern(gfilter, &
                  & "*"//c_null_char)
             call gtk_file_filter_set_name(gfilter, &
                  "All Files"//c_null_char)
             call gtk_file_chooser_add_filter(chooser_info%chooser, gfilter)
          end if
       end if
    end if

    ! Add an entry box for extra filters.
    if (present(edit_filters)) then
       if (edit_filters == TRUE) then
          print *, "Not in GTK4 : edit_filters, gtk_file_chooser_set_extra_widget"
       end if
    end if

    call g_signal_connect(dialog, "response"//c_null_char, &
         & c_funloc(hl_gtk_chooser_resp_cb), c_loc(chooser_info))

  end function hl_gtk_file_chooser_new

  !+
  function hl_gtk_file_chooser_show(files, cdir, directory, create, &
       & multiple, allow_uri, show_hidden, confirm_overwrite, title, &
       & initial_dir, current, initial_file, filter, filter_name, parent, &
       & all, wsize, edit_filters) result(isel)

    integer(kind=c_int) :: isel
    character(len=*), dimension(:), intent(out), allocatable :: files
    character(len=*), intent(out), optional :: cdir
    integer(kind=c_int), intent(in), optional :: directory, create, multiple
    integer(kind=c_int), intent(in), optional :: allow_uri, show_hidden
    integer(kind=c_int), intent(in), optional :: confirm_overwrite
    character(kind=c_char), dimension(*), intent(in), optional :: title, initial_dir, initial_file
    integer(kind=c_int), intent(in), optional :: current
    character(len=*), dimension(:), intent(in), optional :: filter
    character(len=*), dimension(:), intent(in), optional :: filter_name
    type(c_ptr), intent(in), optional :: parent
    integer(kind=c_int), intent(in), optional :: all
    integer(kind=c_int), intent(in), dimension(2), optional :: wsize
    integer(kind=c_int), intent(in), optional :: edit_filters

    ! Create and show a file chooser widget.
    !
    ! FILES: string(): required: The file or files selected.
    ! CDIR: string: optional: The directory from which they were chosen.
    ! DIRECTORY: boolean: optional: Set to TRUE to select directories
    ! 		instead of files.
    ! CREATE: boolean: optional: Set to FALSE to prohibit creating new files.
    ! MULTIPLE: boolean: optional: Set to TRUE to allow the selection of
    ! 		multiple files.
    ! ALLOW_URI: boolean: optional: Set to TRUE to allow nonlocal selections.
    ! SHOW_HIDDEN: boolean: optional: GTK<=3: Set to TRUE to show hidden files.
    ! CONFIRM_OVERWRITE: boolean: optional: GTK<=3: Set to TRUE to request
    ! 		confirmation of an overwrite (only used if CREATE
    ! 		is TRUE).
    ! TITLE: string: optional: Title for the window.
    ! INITIAL_DIR: string: optional: Set the initial directory here instead
    ! 		of the current directory.
    ! CURRENT: boolean: optional: Use to force start in current directory.
    ! INITIAL_FILE: string: optional: Set the initial file selection.
    ! FILTER: string(): optional:  The file selection filter. Elements
    ! 		may either be patterns or mime types. Each filter is a
    ! 		comma-separated list of patterns
    ! FILTER_NAME: string(): optional: Names for the filters
    ! PARENT: c_ptr: optional: Parent window for the dialogue.
    ! ALL: boolean: optional: Set to TRUE to add an all-files filter pattern
    ! WSIZE: c_int(2): optional: Set the size for the dialog.
    ! EDIT_FILTERS: boolean: optional: GTK<=3: Set to TRUE to proves an entry window
    ! 		to add extra filters.
    !
    ! Returns TRUE if one or more files was selected, FALSE otherwise.
    !-

    type(c_ptr) :: dialog, g_file
    type(hl_gtk_chooser_info) :: chooser_info
    integer(kind=c_int) :: i, nsel

    dialog =  hl_gtk_file_chooser_new(chooser_info, cdir, directory, create, &
         & multiple, allow_uri, show_hidden, confirm_overwrite, title, &
         & initial_dir, current, initial_file, filter, filter_name, parent, &
         & all, wsize, edit_filters)

    call gtk_widget_show(dialog)

    ! The callback function is defined in hl_gtk_file_chooser_new()
    dialog_gmainloop = g_main_loop_new(c_null_ptr, FALSE)
    call g_main_loop_run(dialog_gmainloop)
    call gtk_window_destroy(dialog)

    isel = chooser_info%iselect

    if (chooser_info%iselect == TRUE) then
       ! Number of selected files:
       nsel = g_slist_length(chooser_info%chooser_sel_list)
       allocate(files(nsel))
       ! Store the paths of selected files:
       do i = 1, nsel
          ! It's a list of GFiles:
          g_file = g_slist_nth_data(chooser_info%chooser_sel_list, i-1_c_int)
          call convert_c_string(g_file_get_path(g_file), files(i))
          print *, files(i)
          call g_object_unref(g_file)
       end do
       call g_slist_free(chooser_info%chooser_sel_list)

       if (present(cdir)) call convert_c_string(g_file_get_path( &
                                       & chooser_info%chooser_curdir), cdir)
    end if
  end function hl_gtk_file_chooser_show



  !+
  subroutine hl_gtk_chooser_resp_cb(dialog, response, gdata) bind(c)

    type(c_ptr), value :: dialog
    integer(c_int), value :: response
    type(c_ptr), value :: gdata

    ! Callback for the "response" signal of the chooser
    !
    ! DIALOG: c_ptr: required: The dialog sending the response
    ! RESPONSE: c_int: required: The response code.
    ! GDATA: c_ptr: required: User data used to return a select/cancel value
    !
    ! The application developer should never need to use this routine directly.
    !-

    type(hl_gtk_chooser_info), pointer :: chooser_info

    call c_f_pointer(gdata, chooser_info)

    select case (response)
    case (GTK_RESPONSE_DELETE_EVENT)
       chooser_info%iselect = FALSE
    case (GTK_RESPONSE_CANCEL)
       chooser_info%iselect = FALSE
    case (GTK_RESPONSE_APPLY)
       chooser_info%iselect = TRUE
       chooser_info%chooser_sel_list = &
               & gtk_file_chooser_get_files(chooser_info%chooser)
       chooser_info%chooser_curdir = &
            & gtk_file_chooser_get_current_folder(chooser_info%chooser)
    case default
       chooser_info%iselect = FALSE
       write(error_unit,*) &
            & "hl_gtk_chooser_resp_cb:: Invalid response received", response
    end select

    call g_main_loop_quit(dialog_gmainloop)
  end subroutine hl_gtk_chooser_resp_cb

  !+
  subroutine hl_gtk_chooser_filt_cb(widget, gdata) bind(c)

    type(c_ptr), value :: widget
    type(c_ptr), value :: gdata

    ! Callback for the new filter entry.
    !
    ! WIDGET: c_ptr: required: The widget sending the signal
    ! GDATA: c_ptr: required: User data used to return a select/cancel value
    !
    ! The application developer should never need to use this routine directly.
    !-

    type(hl_gtk_chooser_info), pointer :: chooser_info

    character(len=60) :: filter
    type(c_ptr) :: gfilter, buffer
    integer :: idx0, idx1

    call c_f_pointer(gdata, chooser_info)

    call hl_gtk_entry_get_text(chooser_info%fentry, filter)
    if (filter == "") return   ! No filter was given.

    gfilter = gtk_file_filter_new()

    idx0 = 1
    do
       idx1 = index(filter(idx0:),',')+idx0-2
       if (idx1 < idx0) then
          if (index(filter(idx0:), '/') == 0) then
             call gtk_file_filter_add_pattern(gfilter, &
                  & trim(adjustl(filter(idx0:)))//c_null_char)
          else
             call gtk_file_filter_add_mime_type(gfilter, &
                  & trim(adjustl(filter(idx0:)))//c_null_char)
          end if
          exit
       else
          if (index(filter(idx0:idx1), '/') == 0) then
             call gtk_file_filter_add_pattern(gfilter, &
                  & trim(adjustl(filter(idx0:idx1)))//c_null_char)
          else
             call gtk_file_filter_add_mime_type(gfilter, &
                  & trim(adjustl(filter(idx0:idx1)))//c_null_char)
          end if
          idx0=idx1+2
       end if
    end do
    call gtk_file_filter_set_name(gfilter, &
         & trim(filter)//c_null_char)

    call gtk_file_chooser_add_filter(chooser_info%chooser, gfilter)

    buffer = gtk_entry_get_buffer(chooser_info%fentry)
    ! number of caracters = -1 for automatic length:
    call gtk_entry_buffer_set_text (buffer, C_NULL_CHAR, -1)
    
  end subroutine hl_gtk_chooser_filt_cb
end module gtk_hl_chooser
