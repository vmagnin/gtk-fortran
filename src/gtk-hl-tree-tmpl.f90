! Copyright (C) 2011
! Free Software Foundation, Inc.

! This file is part of the gtk-fortran GTK+ Fortran Interface library.

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
! Contributed by James Tappin
! Last modification: 11-21-2011

!!$T Template file for gtk-hl-tree.f90.
!!$T  Make edits to this file, and keep them identical between the
!!$T  GTK2 & GTK3 branches.

!!$T Lines to be used only in the GTK2 tree should be prefixed with !!$2
!!$T Lines to be used only in the GTK3 tree should be prefixed with !!$3
!!$T The mk_gtk_hl.pl script should be used to generate the source file.

module gtk_hl_tree
  !*
  ! Lists and Trees
  ! These functions attempt to hide some of the complexity of the GtkTreeView
  ! system of widgets and object, while still allowing the main functionality
  ! to be accessed. Only "text" displays (this includes numeric values) are
  ! supported.
  !
  ! There are three types of list and tree supported.
  ! 1. listn; A multi-column flat list.
  ! 2. list1; A single-column flat list, that allows only string values.
  !    (This is now implemented by calls to the corresponding listn routines).
  !    Editability is not available for list1.
  ! 3. tree; A tree view (similar to listn but with child rows).
  !/

  use gtk_sup
  use iso_c_binding
  use iso_fortran_env, only: error_unit

  ! Auto generated use's
  use gtk, only: gtk_cell_layout_get_cells,&
       & gtk_cell_renderer_set_alignment,&
       & gtk_cell_renderer_set_padding, gtk_cell_renderer_text_new,&
       & gtk_container_add, gtk_list_store_append,&
       & gtk_list_store_clear, gtk_list_store_insert,&
       & gtk_list_store_move_after, gtk_list_store_move_before,&
       & gtk_list_store_newv, gtk_list_store_remove,&
       & gtk_list_store_reorder, gtk_list_store_set_value,&
       & gtk_list_store_swap, gtk_scrolled_window_new,&
       & gtk_scrolled_window_set_policy,&
       & gtk_tree_model_get_column_type, gtk_tree_model_get_iter,&
       & gtk_tree_model_get_iter_first, gtk_tree_model_get_value,&
       & gtk_tree_model_iter_children, gtk_tree_model_iter_n_children&
       &, gtk_tree_model_iter_next, gtk_tree_model_iter_nth_child,&
       & gtk_tree_model_iter_parent, gtk_tree_path_free,&
       & gtk_tree_path_get_depth, gtk_tree_path_get_indices,&
       & gtk_tree_path_get_indices_with_depth,&
       & gtk_tree_selection_get_selected,&
       & gtk_tree_selection_get_selected_rows,&
       & gtk_tree_selection_select_iter, gtk_tree_selection_set_mode,&
       & gtk_tree_selection_unselect_all, gtk_tree_store_append,&
       & gtk_tree_store_clear, gtk_tree_store_insert,&
       & gtk_tree_store_insert_before, gtk_tree_store_newv,&
       & gtk_tree_store_prepend, gtk_tree_store_remove,&
       & gtk_tree_store_set_value, gtk_tree_view_append_column,&
       & gtk_tree_view_column_add_attribute, gtk_tree_view_column_new&
       &, gtk_tree_view_column_pack_start,&
       & gtk_tree_view_column_set_cell_data_func,&
       & gtk_tree_view_column_set_fixed_width,&
       & gtk_tree_view_column_set_resizable,&
       & gtk_tree_view_column_set_sizing,&
       & gtk_tree_view_column_set_sort_column_id,&
       & gtk_tree_view_column_set_sort_indicator,&
       & gtk_tree_view_column_set_title, gtk_tree_view_get_column,&
       & gtk_tree_view_get_model, gtk_tree_view_get_selection,&
       & gtk_tree_view_new, gtk_tree_view_new_with_model,&
       & gtk_widget_set_sensitive, gtk_widget_set_size_request,&
       & gtk_widget_set_tooltip_text, &
       & GTK_POLICY_AUTOMATIC, GTK_TREE_VIEW_COLUMN_FIXED, &
       & GTK_SELECTION_MULTIPLE, &
       & TRUE, FALSE, g_signal_connect

  use g, only: g_list_foreach, g_list_free, g_list_length, g_list_nth&
       &, g_list_nth_data, g_object_get_data, g_object_set_data,&
       & g_object_set_property, g_value_get_boolean, g_value_get_schar&
       &, g_value_get_double, g_value_get_float, g_value_get_int,&
       & g_value_get_int64, g_value_get_long, g_value_get_string,&
       & g_value_get_uchar, g_value_get_uint, g_value_get_uint64,&
       & g_value_get_ulong, g_value_init, g_value_set_boolean,&
       & g_value_set_schar, g_value_set_double, g_value_set_float,&
       & g_value_set_int, g_value_set_int64, g_value_set_long,&
       & g_value_set_string, g_value_set_uchar, g_value_set_uint,&
       & g_value_set_uint64, g_value_set_ulong

  implicit none

contains
  !+
  function hl_gtk_listn_new(scroll, ncols, types, changed, data, multiple,&
       & width, titles, height, swidth, align, ixpad, iypad, sensitive, &
       & tooltip, sortable, editable, colnos, edited, data_edited) result(list)

    type(c_ptr) :: list
    type(c_ptr), intent(out) :: scroll
    integer(kind=c_int), intent(in), optional :: ncols
    integer(kind=type_kind), dimension(:), intent(in), optional :: types
    type(c_funptr), optional :: changed
    type(c_ptr), intent(in), optional :: data
    integer(kind=c_int), intent(in), optional :: multiple
    integer(kind=c_int), intent(in), optional, dimension(:) :: width
    character(len=*), dimension(:), intent(in), optional :: titles
    integer(kind=c_int), intent(in), optional :: height, swidth
    real(kind=c_float), intent(in), optional, dimension(:) :: align
    integer(kind=c_int), intent(in), optional, dimension(:) :: ixpad, iypad
    integer(kind=c_int), intent(in), optional :: sensitive
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), intent(in), optional, dimension(:) :: sortable, editable
    integer(kind=c_int), dimension(:), allocatable, intent(out), optional, target :: colnos
    type(c_funptr), optional :: edited
    type(c_ptr), optional, intent(in) :: data_edited

    ! Make a multi column list
    !
    ! SCROLL: c_ptr: required: The scrollable widget to contain the list.
    ! 		(This is used to pack the list)
    ! NCOLS: c_int: Optional: The number of columns.
    ! TYPES: GType(): Optional: The types for each column.
    ! CHANGED: c_funptr: optional: Callback function for the "changed"
    ! 		signal to the associated selection object.
    ! DATA: c_ptr: optional: Data to be passed to/from the callback.
    ! MULTIPLE: boolean: optional: Whether multiple selections are allowed.
    ! WIDTH: integer(): optional: The width of the displayed columns.
    ! TITLES: string(): optional: Titles for the visible columns.
    ! HEIGHT: c_int: optional: The height of the display (this is
    !            actually the height of the scroll box).
    ! SWIDTH: c_int: Optional: The width for the scroll box
    ! ALIGN: c_float(): optional: The alignment of the columns
    ! IXPAD: c_int(): optional: The X-padding around the cells.
    ! IYPAD: c_int(): optional: The Y-Padding around the cells.
    ! SENSITIVE: boolean: optional: Whether the widget is intially sensitive.
    ! TOOLTIP: string: optional: Tooltip for the widget
    ! SORTABLE: boolean(): optional: Set whether the list can be sorted
    ! 		on that column.
    ! EDITABLE: boolean(): optional: Set whether the column can be edited.
    ! COLNOS: c_int(): optional: An array of column numbers for the editing
    ! 		callback to use, must be an argument to prevent automatic
    ! 		deallocation, must be present if EDITABLE is present.
    ! EDITED: f_funptr: optional: An alternative callback for the "edited"
    ! 		signal on edited cells. N.B. Only a single callback can be set
    ! 		if different actions are needed for different columns,
    ! 		you must use the column number inside the callback. See
    ! 		hl_gtk_listn_edit_cb for how to access the column numbers.
    ! DATA_EDITED: c_ptr: optional: Data to pass to the edited callback.
    !
    ! At least one of the array arguments or NCOLS must be given.
    ! If TYPES is not given, then strings are assumed.
    !-

    integer(kind=c_int) :: ncols_all, nc, i
    integer(kind=type_kind), dimension(:), allocatable, target :: types_all

    type(c_ptr) :: model, renderer, column, select
    type(gvalue), target :: isedit
    type(c_ptr) :: pisedit

    ! First find how many columns there are (with the index column there's
    ! one more than we ask for)

    if (present(ncols)) then
       ncols_all = ncols
    else if (present(types)) then
       ncols_all = size(types)
    else if (present(titles)) then
       ncols_all = size(titles)
    else if (present(align)) then
       ncols_all = size(align)
    else if (present(width)) then
       ncols_all = size(width)
    else if (present(sortable)) then
       ncols_all = size(sortable)
    else if (present(ixpad)) then
       ncols_all = size(ixpad)
    else if (present(iypad)) then
       ncols_all = size(iypad)
    else if (present(editable)) then
       ncols_all = size(editable)
    else
       write(error_unit,*) "hl_gtk_listn_new: Cannot determine the number of columns"
       list = C_NULL_PTR
       scroll=C_NULL_PTR
       return
    end if

    ! Now determine the column types.
    allocate(types_all(ncols_all))
    if (present(types)) then
       types_all = types
    else
       types_all = (/ (ncols_all-1)*g_type_string /)
    end if

    ! If editable is present, initialize the GValue
    if (present(editable)) then
       if (.not. present(colnos)) then
          write(error_unit,*) "hl_gtk_listn_new: EDITABLE requires COLNOS"
          list=C_NULL_PTR
          scroll=C_NULL_PTR
          return
       end if
       pisedit = c_loc(isedit)
       pisedit = g_value_init(pisedit, G_TYPE_BOOLEAN)
       allocate(colnos(ncols_all))
       colnos = (/ (i-1, i=1, ncols_all) /)
    end if

    ! Create the storage model
    model = gtk_list_store_newv(ncols_all, c_loc(types_all))

    ! Create the list in the scroll box
    scroll = gtk_scrolled_window_new(C_NULL_PTR, C_NULL_PTR)
    call gtk_scrolled_window_set_policy(scroll, GTK_POLICY_AUTOMATIC, &
         & GTK_POLICY_AUTOMATIC)
    list = gtk_tree_view_new_with_model(model)
    call gtk_container_add(scroll, list)
    if (present(height) .and. present(swidth)) then
       call gtk_widget_set_size_request(scroll,swidth,height)
    else if (present(height)) then
       call gtk_widget_set_size_request(scroll,0,height)
    else if (present(swidth)) then
       call gtk_widget_set_size_request(scroll,swidth,0)
    end if


    ! Now the visible columns
    do i = 1, ncols_all
       renderer = gtk_cell_renderer_text_new()
       if (present(ixpad) .and. present(iypad)) then
          call gtk_cell_renderer_set_padding(renderer, &
               & ixpad(i), iypad(i))
       else if (present(ixpad)) then
          call gtk_cell_renderer_set_padding(renderer, &
               & ixpad(i), 0)
       else if (present(iypad)) then
          call gtk_cell_renderer_set_padding(renderer, &
               & 0, iypad(i))
       end if
       if (present(editable)) then
          call g_value_set_boolean(pisedit, editable(i))
          call g_object_set_property(renderer, "editable"//c_null_char, pisedit)
          if (editable(i) == TRUE) then
             call g_object_set_data(renderer, "column-number"//c_null_char, &
                  & c_loc(colnos(i)))
             call g_object_set_data(renderer, "view"//c_null_char, list)
             if (present(edited)) then
                if (present(data_edited)) then
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited, data_edited)
                else
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited)
                end if
             else
                call g_signal_connect(renderer, "edited"//c_null_char, &
                     & c_funloc(hl_gtk_listn_edit_cb))
             endif
          end if
       end if
       if (present(align)) then
          call gtk_cell_renderer_set_alignment(renderer, align(i), 0.)
       else if (types_all(i) == G_TYPE_STRING) then
          call gtk_cell_renderer_set_alignment(renderer, 0., 0.)
       else
          call gtk_cell_renderer_set_alignment(renderer, 1., 0.)
       end if

       column = gtk_tree_view_column_new()
       call gtk_tree_view_column_pack_start(column, renderer, FALSE)

       if (present(titles)) call gtk_tree_view_column_set_title(column, &
            &trim(titles(i))//c_null_char)
       call gtk_tree_view_column_add_attribute(column, renderer, &
            & "text"//C_NULL_CHAR, i-1)
       nc = gtk_tree_view_append_column(list, column)
       if (present(sortable)) then
          if (sortable(i) == TRUE) then
             call gtk_tree_view_column_set_sort_column_id(column, i-1)
             call gtk_tree_view_column_set_sort_indicator(column, TRUE)
          end if
       end if
       if (present(width)) then
          call gtk_tree_view_column_set_sizing (column, &
               & GTK_TREE_VIEW_COLUMN_FIXED)
          call gtk_tree_view_column_set_fixed_width(column, width(i))
       end if
      call gtk_tree_view_column_set_resizable(column,TRUE)
    end do

    ! The event handler is attached to the selection object, as is
    ! the multiple selection property.

    select = gtk_tree_view_get_selection(list)

    if (present(multiple)) then
       if (multiple == TRUE) &
            & call gtk_tree_selection_set_mode(select, GTK_SELECTION_MULTIPLE)
    end if

    if (present(changed)) then
       if (present(data)) then
          call g_signal_connect(select, "changed"//c_null_char, &
               & changed, data)
       else
          call g_signal_connect(select, "changed"//c_null_char, changed)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(list, tooltip)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(list, sensitive)

    deallocate(types_all)
  end function hl_gtk_listn_new

  !+
  subroutine hl_gtk_listn_edit_cb(renderer, path, text, gdata) bind(c)
    type(c_ptr), value :: renderer, path, text, gdata
    ! Default callback for list cell edited.
    !
    ! RENDERER: c_ptr: required: The renderer which sent the signal
    ! PATH: c_ptr: required: The path at which to insert
    ! TEXT: c_ptr: required: The text to insert
    ! GDATA: c_ptr: required: User data, Not used.
    !
    ! The column number is passed via the "column-number" gobject data value.
    ! The treeview containing the cell is passed via the "view" gobject
    ! data value.
    ! The row number is passed as a string in the PATH argument.
    ! This routine is not normally called by the application developer.
    !-

    character(len=200) :: fpath, ftext
    integer(kind=c_int) :: irow
    integer(kind=c_int), pointer :: icol
    integer :: ios
    type(c_ptr) :: pcol, list

    call convert_c_string(path, 200, fpath)
    read(fpath, *) irow
    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)
    call convert_c_string(text, 200, ftext)
    list = g_object_get_data(renderer, "view"//c_null_char)

    call hl_gtk_listn_set_cell(list, irow, icol, &
         & svalue=trim(ftext))
  end subroutine hl_gtk_listn_edit_cb

  !+
  subroutine hl_gtk_listn_ins(list, row)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in), optional :: row

    ! Insert a row into a tabular list.
    !
    ! LIST: c_ptr: required: The list into which to insert the row.
    ! ROW: c_int: optional: The row BEFORE which to insert the row
    ! 		(append if absent)
    !-

    type(c_ptr) :: store
    type(gtktreeiter), target :: iter

    ! Get the ListStore
    store = gtk_tree_view_get_model(list)

    ! Insert the row
    if (present(row)) then
       call gtk_list_store_insert(store, c_loc(iter), row)
    else
       call gtk_list_store_append(store, c_loc(iter))
    end if
  end subroutine hl_gtk_listn_ins

  !+
  subroutine hl_gtk_listn_rem(list, row)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), optional, intent(in) :: row

    ! Remove a row or clear a list
    !
    ! LIST: c_ptr: required: The list to modify
    ! ROW: integer: optional: The row to remove, if absent clear the list
    !-

    integer(kind=c_int) :: valid
    type(c_ptr) :: store
    type(gtktreeiter), target :: iter

    ! Get list store
    store = gtk_tree_view_get_model(list)

    ! If 2 arguments, then remove a row
    if (present(row)) then
       valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), C_NULL_PTR, row)
       if (valid==FALSE) return

       valid = gtk_list_store_remove(store, c_loc(iter))
 
    else   ! 1 argument clear the whole list
       call gtk_list_store_clear(store)
    end if
  end subroutine hl_gtk_listn_rem

  !+
  function hl_gtk_listn_get_selections(list, indices, selection) result(count)

    integer(kind=c_int) :: count
    type(c_ptr), intent(in) :: list
    integer(kind=c_int), dimension(:), allocatable, target, &
         & intent(out), optional :: indices
    type(c_ptr), optional :: selection

    ! Get the indices of the selected rows
    !
    ! LIST: c_ptr: required: The list whose selections are to be found.
    ! INDICES: integer: optional: An allocatable array to return the
    ! 		list of selections. (If count = 0 it will not be allocated).
    ! 		If this argument is not given, then the number of
    ! 		selected rows is returned.
    ! SELECTION: c_ptr: optional: A selection. If this is given then LIST
    !           is ignored. This is most often used in the callback routine
    !           for the changed signal when that needs to find which element(s)
    !           are selected.
    !
    ! Returns the number of selections.
    !-

    type(c_ptr) :: slist, vselection
    type(c_ptr), target :: model
    integer(kind=c_int) :: i
    type(c_ptr) :: cindex
    integer(kind=c_int), pointer :: findex

    if (present(selection)) then
       vselection = selection
    else
       vselection = gtk_tree_view_get_selection(list)
    end if

    slist = gtk_tree_selection_get_selected_rows(vselection, &
         & c_loc(model))

    ! If no selections, then set the count to 0 and return
    if (.not. c_associated(slist)) then
       count=0
       return
    end if

    ! Determine how many rows are selected. Then if no output list was
    ! supplied, return, otherwise go on and make a list.
    count = g_list_length(slist)
    if (.not. present(indices)) return

    allocate(indices(count))

    ! For each of the elements in the selection list, find its index
    ! from the hidden first column
    do i = 1, count
       cindex = gtk_tree_path_get_indices(g_list_nth_data(slist, i-1))
       call c_f_pointer(cindex, findex)
       indices(i) = findex
    end do

    ! Free the selection list.
    call g_list_foreach(slist, c_funloc(gtk_tree_path_free), C_NULL_PTR)
    call g_list_free(slist)

  end function hl_gtk_listn_get_selections


  !+
  subroutine  hl_gtk_listn_set_selection(list, row)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in), optional :: row

    ! Set the selected row in a list (single row only).
    !
    ! LIST: c_ptr: required: The list to work on.
    ! ROW: c_int: optional: The row to select (absent or less than 0 is
    ! 		 clear selection)
    !-

    type(c_ptr) :: selection, store
    type(gtktreeiter), target :: iter
    integer(kind=c_int) :: valid

    ! Get list store and selection
    store = gtk_tree_view_get_model(list)
    selection = gtk_tree_view_get_selection(list)

    if (.not. present(row)) then
       call gtk_tree_selection_unselect_all(selection)
    else if (row < 0) then
       call gtk_tree_selection_unselect_all(selection)
    else
       valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), C_NULL_PTR, row)
       if (valid == FALSE) return
       call gtk_tree_selection_select_iter(selection, c_loc(iter))
    end if
  end subroutine hl_gtk_listn_set_selection

  !+
  subroutine hl_gtk_listn_set_cell(list, row, col, &
       & svalue, fvalue, dvalue, ivalue, lvalue, l64value)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in) :: row, col
    character(len=*), intent(in), optional :: svalue
    real(kind=c_float), intent(in), optional :: fvalue
    real(kind=c_double), intent(in), optional :: dvalue
    integer(kind=c_int), intent(in), optional :: ivalue
    integer(kind=c_long), intent(in), optional :: lvalue
    integer(kind=c_int64_t), intent(in), optional :: l64value

    ! Set the value of a cell.
    !
    ! LIST: c_ptr: required: The list containing the cell.
    ! ROW: c_int: required: The row of the cell
    ! COL: c_int: required: The column of the cell.
    ! SVALUE: string: optional: A string value for the cell.
    ! FVALUE: float: optional: A single precision FP value for the cell.
    ! DVALUE: double: optional: A double precision FP value for the cell.
    ! IVALUE: c_int: optional: A normal integer value for the cell.
    ! LVALUE: c_long: optional: A long integer value for the cell.
    ! L64VALUE: c_int64_t: optional: A 64-bit integer value for the cell.
    !
    ! Note that reasonable conversions are made between types.
    !-

    integer(kind=type_kind) :: ctype
    type(c_ptr) :: store, val
    integer(kind=c_int) :: valid
    type(gtktreeiter), target :: iter
    type(gvalue), target :: value

    character(len=120) :: sconv
    integer(kind=c_int) :: iconv
    integer(kind=c_long) :: lconv
    integer(kind=c_int64_t) :: l64conv
    real(kind=c_float) :: fconv
    real(kind=c_double) :: dconv
    integer :: ios
    ! Get list store
    store = gtk_tree_view_get_model(list)

    ! Find the type for the requested column
    ctype = gtk_tree_model_get_column_type(store, col)

    ! Get the iterator of the row
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), C_NULL_PTR, row)
    if (valid == FALSE) return

    ! Set up the GValue to the right type.
    val = c_loc(value)
    val = g_value_init(val, ctype)

    ! Select according to the cell type
    select case(ctype)
    case(G_TYPE_CHAR)
       if (present(svalue)) then
          call g_value_set_schar(val, ichar(svalue(1:1), c_int8_t))
       else if (present(ivalue)) then
          call g_value_set_schar(val, int(ivalue, c_int8_t))
       else if (present(lvalue)) then
          call g_value_set_schar(val, int(lvalue, c_int8_t))
       else if (present(l64value)) then
          call g_value_set_schar(val, int(l64value, c_int8_t))
       else
          write(error_unit,*) "hl_gtk_listn_set_cell:: Cannot make a 'char' type from given value(s)"
          return
       end if
    case(G_TYPE_UCHAR)
       if (present(svalue)) then
          call g_value_set_uchar(val, svalue(1:1))
       else if (present(ivalue)) then
          call g_value_set_uchar(val, char(ivalue, c_char))
       else if (present(lvalue)) then
          call g_value_set_uchar(val, char(lvalue, c_char))
       else if (present(l64value)) then
          call g_value_set_uchar(val, char(l64value, c_char))
       else
          write(error_unit,*) "hl_gtk_listn_set_cell:: Cannot make a 'char' type from given value(s)"
          return
       end if

    case (G_TYPE_INT)
       if (present(ivalue)) then
          call g_value_set_int(val, ivalue)
       else if (present(lvalue)) then
          call g_value_set_int(val, int(lvalue, c_int))
       else if (present(l64value)) then
          call g_value_set_int(val, int(l64value, c_int))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) iconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'int'"
             return
          end if
          call g_value_set_int(val, iconv)
       else
          write(error_unit,*) "hl_gtk_listn_set_cell:: Cannot make an 'int' type from given value(s)"
          return
       end if
    case (G_TYPE_UINT)
       if (present(ivalue)) then
          call g_value_set_uint(val, ivalue)
       else if (present(lvalue)) then
          call g_value_set_uint(val, int(lvalue, c_int))
       else if (present(l64value)) then
          call g_value_set_uint(val, int(l64value, c_int))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) iconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'int'"
             return
          end if
          call g_value_set_uint(val, iconv)
       else
          write(error_unit,*) "hl_gtk_listn_set_cell:: Cannot make an 'int' type from given value(s)"
          return
       end if
    case (G_TYPE_BOOLEAN)
       if (present(ivalue)) then
          call g_value_set_boolean(val, ivalue)
       else if (present(lvalue)) then
          call g_value_set_boolean(val, int(lvalue, c_int))
       else if (present(l64value)) then
          call g_value_set_boolean(val, int(l64value, c_int))
       else if (present(svalue)) then
          if (svalue=='T' .or. svalue=='t' .or. svalue=='TRUE' .or. &
               & svalue=='true' .or. svalue=='True' .or. svalue=='Y' &
               & .or. svalue=='y' .or. svalue=='YES' .or. svalue=='yes' &
               & .or. svalue=='Yes' .or. svalue=='.TRUE.' .or. &
               & svalue=='.true.') then
             call g_value_set_boolean(val, TRUE)
          else if (svalue=='F' .or. svalue=='f' .or. svalue=='FALSE' .or. &
               & svalue=='false' .or. svalue=='False' .or. svalue=='N' .or. &
               & svalue=='n' .or. svalue=='NO' .or. svalue=='no' .or. &
               & svalue=='No' .or. svalue=='.FALSE.' .or. &
               & svalue=='.false.') then
             call g_value_set_boolean(val, FALSE)
          else
             read(svalue,*,iostat=ios) iconv
             if (ios /= 0) then
                write(error_unit,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'int'"
                return
             end if
             call g_value_set_boolean(val, iconv)
          end if
       else
          write(error_unit,*) "hl_gtk_listn_set_cell:: Cannot make an 'int' type from given value(s)"
          return
       end if

    case (G_TYPE_LONG)
       if (present(lvalue)) then
          call g_value_set_long(val, lvalue)
       else if (present(l64value)) then
          call g_value_set_long(val, int(l64value, c_long))
       else if (present(ivalue)) then
          call g_value_set_long(val, int(ivalue, c_long))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) lconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'long'"
             return
          end if
          call g_value_set_long(val, lconv)
       else
          write(error_unit,*) "hl_gtk_listn_set_cell:: Cannot make a 'long' type from given value(s)"
          return
       end if
    case (G_TYPE_ULONG)
       if (present(lvalue)) then
          call g_value_set_ulong(val, lvalue)
       else if (present(l64value)) then
          call g_value_set_ulong(val, int(l64value, c_long))
       else if (present(ivalue)) then
          call g_value_set_ulong(val, int(ivalue, c_long))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) lconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'long'"
             return
          end if
          call g_value_set_ulong(val, lconv)
       else
          write(error_unit,*) "hl_gtk_listn_set_cell:: Cannot make a 'long' type from given value(s)"
          return
       end if

    case (G_TYPE_INT64)
       if (present(l64value)) then
          call g_value_set_int64(val, l64value)
       else if (present(lvalue)) then
          call g_value_set_int64(val, int(lvalue, c_int64_t))
       else if (present(ivalue)) then
          call g_value_set_int64(val, int(ivalue, c_int64_t))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) l64conv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'int64'"
             return
          end if
          call g_value_set_int64(val, l64conv)
       else
          write(error_unit,*) "hl_gtk_listn_set_cell:: Cannot make an 'int64' type from given value(s)"
          return
       end if
    case (G_TYPE_UINT64)
       if (present(l64value)) then
          call g_value_set_uint64(val, l64value)
       else if (present(lvalue)) then
          call g_value_set_uint64(val, int(lvalue, c_int64_t))
       else if (present(ivalue)) then
          call g_value_set_uint64(val, int(ivalue, c_int64_t))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) l64conv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'int64'"
             return
          end if
          call g_value_set_uint64(val, l64conv)
       else
          write(error_unit,*) "hl_gtk_listn_set_cell:: Cannot make an 'int64' type from given value(s)"
          return
       end if

    case(G_TYPE_FLOAT)
       if (present(fvalue)) then
          call g_value_set_float(val, fvalue)
       else if (present(dvalue)) then
          call g_value_set_float(val, real(dvalue, c_float))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) fconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'float'"
             return
          end if
          call g_value_set_float(val, fconv)
       else
          write(error_unit,*) "hl_gtk_listn_set_cell:: Cannot make a 'float' type from given value(s)"
          return
       end if

    case(G_TYPE_DOUBLE)
       if (present(dvalue)) then
          call g_value_set_double(val, dvalue)
       else if (present(fvalue)) then
          call g_value_set_double(val, real(fvalue, c_double))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) dconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'double'"
             return
          end if
          call g_value_set_double(val, dconv)
       else
          write(error_unit,*) "hl_gtk_listn_set_cell:: Cannot make a 'double' type from given value(s)"
          return
       end if

    case (G_TYPE_STRING)
       if (present(svalue)) then
          call g_value_set_string(val, trim(svalue)//c_null_char)
       else
          if (present(ivalue)) then
             write(sconv,*) ivalue
          else if (present(lvalue)) then
             write(sconv,*) lvalue
          else if (present(l64value)) then
             write(sconv,*) l64value
          else if (present(fvalue)) then
             write(sconv,*) fvalue
          else if (present(dvalue)) then
             write(sconv,*) dvalue
          else
             write(error_unit,*) "hl_gtk_listn_set_cell:: Cannot make a 'string' type from given value(s)"
             return
          end if
          call g_value_set_string(val, trim(sconv)//c_null_char)
       end if

    case default
       write(error_unit,*)  "hl_gtk_listn_set_cell:: Cell type ",ctype," is unknown"
       return
    end select

    call gtk_list_store_set_value(store, c_loc(iter), col, val)

  end subroutine hl_gtk_listn_set_cell

  !+
  subroutine hl_gtk_listn_get_cell(list, row, col, &
    & svalue, fvalue, dvalue, ivalue, lvalue, l64value)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in) :: row, col
    character(len=*), intent(out), optional :: svalue
    real(kind=c_float), intent(out), optional :: fvalue
    real(kind=c_double), intent(out), optional :: dvalue
    integer(kind=c_int), intent(out), optional :: ivalue
    integer(kind=c_long), intent(out), optional :: lvalue
    integer(kind=c_int64_t), intent(out), optional :: l64value

    ! Retrieve the value of a cell.
    !
    ! LIST: c_ptr: required: The list containing the cell.
    ! ROW: c_int: required: The row of the cell
    ! COL: c_int: required: The column of the cell.
    ! SVALUE: string: optional: A string value from the cell.
    ! FVALUE: float: optional: A single precision FP value from the cell.
    ! DVALUE: double: optional: A double precision FP value from the cell.
    ! IVALUE: c_int: optional: A normal integer value from the cell.
    ! LVALUE: c_long: optional: A long integer value from the cell.
    ! L64VALUE: c_int64_t: optional: A 64-bit integer value from the cell.
    !
    ! Note that a similar conversion system to the set_cell routine
    ! except that strings can only be returned to SVALUE.
    !-

    integer(kind=type_kind) :: ctype
    type(c_ptr) :: store, val, cstr
    integer(kind=c_int) :: valid
    type(gtktreeiter), target :: iter
    type(gvalue), target :: value

    ! Get list store
    store = gtk_tree_view_get_model(list)

    ! Find the type for the requested column
    ctype = gtk_tree_model_get_column_type(store, col)

    ! Get the iterator of the row
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), C_NULL_PTR, row)
    if (valid == FALSE) return

    ! Set up the GValue pointer (for convenience) gtk_tree_model_get_value
    ! does the initialization.
    val = c_loc(value)

    ! Get the GValue of the cell.
    call gtk_tree_model_get_value(store, c_loc(iter), col, val)

    ! Now extract the value to a useful form according to the type
    ! of cell.
    select case(ctype)
    case(G_TYPE_CHAR)
       if (present(svalue)) then
          svalue(1:1) = char(g_value_get_schar(val))
       else if (present(ivalue)) then
          ivalue = int(g_value_get_schar(val))
       else if (present(lvalue)) then
          lvalue = int(g_value_get_schar(val), c_long)
       else if (present(l64value)) then
          l64value = int(g_value_get_schar(val), c_int64_t)
       else
          write(error_unit,*) "hl_gtk_listn_get_cell:: Cannot return 'char' type to any available output"
          return
       end if
    case(G_TYPE_UCHAR)
       if (present(svalue)) then
           svalue(1:1)= g_value_get_uchar(val)
       else if (present(ivalue)) then
          ivalue = ichar(g_value_get_uchar(val))
       else if (present(lvalue)) then
          lvalue = ichar(g_value_get_uchar(val))
       else if (present(l64value)) then
          l64value = ichar(g_value_get_uchar(val))
       else
          write(error_unit,*) "hl_gtk_listn_get_cell:: Cannot return 'char' type to any available output"
          return
       end if

    case (G_TYPE_INT)
       if (present(ivalue)) then
          ivalue = g_value_get_int(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_int(val), c_long)
       else if (present(l64value)) then
          l64value = int(g_value_get_int(val), c_int64_t)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_int(val)
       else
          write(error_unit,*) "hl_gtk_listn_get_cell:: Cannot return 'int' type to any available output"
          return
       end if
    case (G_TYPE_UINT)
       if (present(ivalue)) then
          ivalue = g_value_get_uint(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_uint(val), c_long)
       else if (present(l64value)) then
          l64value = int(g_value_get_uint(val), c_int64_t)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_uint(val)
       else
          write(error_unit,*) "hl_gtk_listn_get_cell:: Cannot return 'int' type to any available output"
          return
       end if
    case (G_TYPE_BOOLEAN)
       if (present(ivalue)) then
          ivalue = g_value_get_boolean(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_boolean(val), c_long)
       else if (present(l64value)) then
          l64value = int(g_value_get_boolean(val), c_int64_t)
       else if (present(svalue)) then
          if (g_value_get_boolean(val) == TRUE) then
             svalue = 'True'
          else
             svalue='False'
          end if
       else
          write(error_unit,*) "hl_gtk_listn_get_cell:: Cannot return 'bool' type to any available output"
          return
       end if

    case (G_TYPE_LONG)
       if (present(lvalue)) then
          lvalue = g_value_get_long(val)
       else if (present(l64value)) then
          l64value = int(g_value_get_long(val), c_int64_t)
       else if (present(ivalue)) then
          ivalue = int(g_value_get_long(val), c_int)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_long(val)
       else
          write(error_unit,*) "hl_gtk_listn_get_cell:: Cannot return 'long' type to any available output"
          return
       end if
    case (G_TYPE_ULONG)
       if (present(lvalue)) then
          lvalue = g_value_get_ulong(val)
       else if (present(l64value)) then
          l64value = int(g_value_get_ulong(val), c_int64_t)
       else if (present(ivalue)) then
          ivalue = int(g_value_get_ulong(val), c_int)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_ulong(val)
       else
          write(error_unit,*) "hl_gtk_listn_get_cell:: Cannot return 'long' type to any available output"
          return
       end if

    case (G_TYPE_INT64)
       if (present(l64value)) then
          l64value = g_value_get_int64(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_int64(val), c_long)
       else if (present(ivalue)) then
          ivalue = int(g_value_get_int64(val), c_int)
       else if (present(svalue)) then
          write (svalue,*) g_value_get_int64(val)
       else
          write(error_unit,*) "hl_gtk_listn_get_cell:: Cannot return 'int64' type to any available output"
          return
       end if
    case (G_TYPE_UINT64)
       if (present(l64value)) then
          l64value = g_value_get_uint64(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_uint64(val), c_long)
       else if (present(ivalue)) then
          ivalue = int(g_value_get_uint64(val), c_int)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_uint64(val)
       else
          write(error_unit,*) "hl_gtk_listn_get_cell:: Cannot return 'int64' type to any available output"
          return
       end if

    case(G_TYPE_FLOAT)
       if (present(fvalue)) then
          fvalue = g_value_get_float(val)
       else if (present(dvalue)) then
          dvalue = real(g_value_get_float(val), c_double)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_float(val)
       else
          write(error_unit,*) "hl_gtk_listn_get_cell:: Cannot return 'float' type to any available output"
          return
       end if

    case(G_TYPE_DOUBLE)
       if (present(dvalue)) then
          dvalue = g_value_get_double(val)
       else if (present(fvalue)) then
          fvalue = real(g_value_get_double(val), c_float)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_double(val)
       else
          write(error_unit,*) "hl_gtk_listn_get_cell:: Cannot return 'double' type to any available output"
          return
       end if

    case (G_TYPE_STRING)
       if (present(svalue)) then
          cstr = g_value_get_string(val)
          call convert_c_string(cstr, len(svalue), svalue)
       else
          write(error_unit,*) "hl_gtk_listn_get_cell:: Cannot return 'string' type to any available output"
       end if

    case default
       write(error_unit,*)  "hl_gtk_listn_get_cell:: Cell type ",ctype," is unknown"
       return
    end select
  end subroutine hl_gtk_listn_get_cell

  !+
  subroutine hl_gtk_listn_move_row(list, row1, row2, after)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in) :: row1
    integer(kind=c_int), intent(in), optional :: row2
    integer(kind=c_int), intent(in), optional :: after

    ! Move a row in a list to a new location
    !
    ! LIST: c_ptr: required: The list to work on.
    ! ROW1: c_int: required: The index of the row to move.
    ! ROW2: c_int: optional: The location before which to place
    ! 		the row. (If omitted, then move to start (or end if AFTER
    ! 		is TRUE)).
    ! AFTER: boolean: optional:  Set this to TRUE to put the row after
    ! 		the location instead of before.
    !-

    type(c_ptr) :: store
    type(gtktreeiter), target :: iter1, iter2
    integer(kind=c_int) :: isafter
    integer(kind=c_int) :: valid

    if (present(after)) then
       isafter = after
    else
       isafter = FALSE
    end if

    ! Get list store
    store = gtk_tree_view_get_model(list)

    ! Get the iterator of the row to move
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter1), C_NULL_PTR, row1)
    if (valid == FALSE) return
    ! And of the target location
    if (present(row2)) then
       valid = gtk_tree_model_iter_nth_child(store, c_loc(iter2), C_NULL_PTR, row2)
       if (valid == FALSE) return
    end if

    ! Move it
    if (isafter == TRUE) then
       if (present(row2)) then
          call gtk_list_store_move_after(store, c_loc(iter1), c_loc(iter2))
       else
          call gtk_list_store_move_before(store, c_loc(iter1), C_NULL_PTR)
       end if
    else
       if (present(row2)) then
          call gtk_list_store_move_before(store, c_loc(iter1), c_loc(iter2))
       else
          call gtk_list_store_move_after(store, c_loc(iter1), C_NULL_PTR)
       end if
    end if
  end subroutine hl_gtk_listn_move_row

  !+
  subroutine hl_gtk_listn_swap_rows(list, row1, row2)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in) :: row1, row2

    ! Move a row in a list to a new location
    !
    ! LIST: c_ptr: required: The list to work on.
    ! ROW1: c_int: required: The index of the first row to move.
    ! ROW2: c_int: required: The index of the second row to move
    !-

    type(c_ptr) :: store
    type(gtktreeiter), target :: iter1, iter2
    integer(kind=c_int) :: valid

    ! Get list store
    store = gtk_tree_view_get_model(list)

    ! Get the iterator of the first row to move
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter1), C_NULL_PTR, row1)
    if (valid == FALSE) return
    ! And of the second
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter2), C_NULL_PTR, row2)
    if (valid == FALSE) return

    ! Exchange the rows
    call gtk_list_store_swap(store, c_loc(iter1), c_loc(iter2))

  end subroutine hl_gtk_listn_swap_rows

  !+
  subroutine hl_gtk_listn_reorder(list, indices)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in), dimension(:) :: indices

    ! Move a row in a list to a new location
    !
    ! LIST: c_ptr: required: The list to work on.
    ! INDICES: c_int(): required: The sorting array. The ith element
    ! 		contains the old location of the new (i-1)th row.
    !-

    type(c_ptr) :: store
    integer(kind=c_int), dimension(:), allocatable, target :: idx

    allocate(idx(size(indices)))
    idx = indices

    ! Get list store
    store = gtk_tree_view_get_model(list)

    ! Reorder the list
    call gtk_list_store_reorder(store, c_loc(idx))

    deallocate(idx)

  end subroutine hl_gtk_listn_reorder

  !+
  function hl_gtk_listn_get_n_rows(list) result(nrows)

    integer(kind=c_int) :: nrows
    type(c_ptr), intent(in) :: list

    ! Return the number of rows in a list.
    !
    ! LIST: c_ptr: required: the list to query
    !-

    type(c_ptr) :: store

    ! Get list store
    store = gtk_tree_view_get_model(list)

    ! Find how many rows
    nrows = gtk_tree_model_iter_n_children(store, C_NULL_PTR)

  end function hl_gtk_listn_get_n_rows

  !+
  subroutine hl_gtk_listn_set_cell_data_func(list, colno, func, &
       & data, destroy_notify)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in) :: colno
    type(c_funptr), optional :: func
    type(c_ptr), optional :: data
    type(c_funptr), optional :: destroy_notify

    ! Add a custom rendering function to a column of a list
    !
    ! LIST: c_ptr: required: The list to which to apply the rendering function
    ! COLNO: c_int: required: The column index to which to apply it.
    ! FUNC: c_funptr: optional: The function (actually subroutine)
    ! 		to do the rendering (see GtkTreeCellDataFunc, for
    ! 		details). Omit or set to C_NULL_FUNPTR to remove a function.
    ! DATA: c_ptr: optional: User data to pass to the function.
    ! DESTROY_NOTIFY: c_funptr: optional: A destroy notify subroutine.
    !-

    type(c_funptr) :: funpass, destpass
    type(c_ptr) :: datapass

    type(c_ptr) :: col, renderer, rlist

    if (present(func)) then
       funpass = func
    else
       funpass = C_NULL_FUNPTR
    end if

    if (present(data)) then
       datapass = data
    else
       datapass = C_NULL_PTR
    end if

    if (present(destroy_notify)) then
       destpass = destroy_notify
    else
       destpass = C_NULL_FUNPTR
    end if

    col = gtk_tree_view_get_column(list, colno)
    rlist = gtk_cell_layout_get_cells(col)
    renderer = g_list_nth_data(rlist, 0)
    call g_list_free(rlist)

    call gtk_tree_view_column_set_cell_data_func(col, renderer,&
         & funpass, datapass, destpass)

  end subroutine hl_gtk_listn_set_cell_data_func

  !+
  function hl_gtk_list1_new(scroll, width, changed, data, multiple, &
       & sensitive, tooltip, title, height) result(list)

    type(c_ptr) :: list
    type(c_ptr), intent(out) :: scroll
    integer(kind=c_int), intent(in), optional :: width
    type(c_funptr), intent(in), optional :: changed
    type(c_ptr), intent(in), optional :: data
    integer(kind=c_int), intent(in),  optional :: multiple, sensitive
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    character(len=*), intent(in), optional :: title
    integer(kind=c_int), intent(in), optional :: height

    ! A single column selectable list based on the GTK Tree View
    !
    ! SCROLL: c_ptr: required: The scroll box containing the list
    ! 		(used for packing etc.)
    ! WIDTH: integer: optional: The width of the displayed column.
    ! CHANGED: c_funptr: optional: Callback function for the "changed"
    !           signal to the associated selection object.
    ! DATA: c_ptr: optional: Data to be passed to/from the callback.
    ! MULTIPLE: boolean: optional: Whether multiple selections are allowed.
    ! SENSITIVE: boolean: optional: Whether the widget is intially sensitive.
    ! TOOLTIP: string: optional: Tooltip for the widget
    ! TITLE: string: optional: Title for the visible column.
    ! HEIGHT: integer: optional: The height of the display (this is
    !            actually the height of the scroll box).
    !
    ! If other options (e.g. sortable columns or editable cells are needed,
    ! the use hl_gtk_listn_new with 1 column).
    !-

    integer(kind=type_kind), target, dimension(1) :: types

    ! Create list storage with 2 colums (one is a dummy, to provide an index)

    types = (/ g_type_string /)

    ! This slightly clunky if /else cascade is needed because the attempt to convert
    ! an unset scalar argument to an array causes a segfault.
    if (present(title) .and. present(width)) then
       list = hl_gtk_listn_new(scroll, ncols=1, types=types, changed=changed, &
            & data=data, multiple=multiple, sensitive=sensitive, &
            & tooltip=tooltip, width=(/width/), titles=(/title/), height=height)
    else if (present(title)) then
       list = hl_gtk_listn_new(scroll, ncols=1, types=types, changed=changed, &
            & data=data, multiple=multiple, sensitive=sensitive, &
            & tooltip=tooltip, titles=(/title/), height=height)
    else if (present(width)) then
       list = hl_gtk_listn_new(scroll, ncols=1, types=types, changed=changed, &
            & data=data, multiple=multiple, sensitive=sensitive, &
            & tooltip=tooltip, width=(/width/), height=height)
    else
       list = hl_gtk_listn_new(scroll, ncols=1, types=types, changed=changed, &
            & data=data, multiple=multiple, sensitive=sensitive, &
            & tooltip=tooltip, height=height)
    end if
  end function hl_gtk_list1_new

  !+
  subroutine hl_gtk_list1_ins(list, text, row)

    type(c_ptr), intent(in) :: list
    character(kind=c_char, len=*), intent(in), optional :: text
    integer(kind=c_int), intent(in), optional :: row

    ! Insert a row into a list
    !
    ! LIST: c_ptr: required: The list to insert to.
    ! TEXT: string: optional: The text to insert.
    ! ROW: integer: optional: The row at which to insert the text
    ! 		(omit to append)
    !-

    integer(kind=c_int) :: irow
    type(c_ptr) :: store

    call hl_gtk_listn_ins(list, row)
    if (.not. present(text)) return

    if (present(row)) then
       irow = row
    else
       store = gtk_tree_view_get_model(list)
       irow=gtk_tree_model_iter_n_children (store, C_NULL_PTR)-1
    end if

    call hl_gtk_listn_set_cell(list, irow, 0, svalue=text)

   end subroutine hl_gtk_list1_ins

  !+
  subroutine hl_gtk_list1_rem(list, row)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), optional, intent(in) :: row

    ! Remove a row or clear a list
    !
    ! LIST: c_ptr: required: The list to modify
    ! ROW: integer: optional: The row to remove, if absent clear the list
    !-

    call hl_gtk_listn_rem(list, row)

  end subroutine hl_gtk_list1_rem

  !+
  function hl_gtk_list1_get_selections(list, indices, selection) result(count)

    integer(kind=c_int) :: count
    type(c_ptr), intent(in) :: list
    integer(kind=c_int), dimension(:), allocatable, target, &
         & intent(out), optional :: indices
    type(c_ptr), optional :: selection

    ! Get the indices of the selected rows
    !
    ! LIST: c_ptr: required: The list whose selections are to be found.
    ! INDICES: integer: optional: An allocatable array to return the
    ! 		list of selections. (If count = 0 it will not be allocated).
    ! 		If this argument is not given, then the number of
    ! 		selected rows is returned.
    ! SELECTION: c_ptr: optional: A selection. If this is given then LIST
    !           is ignored. This is most often used in the callback routine
    !           for the changed signal when that needs to find which element(s)
    !           are selected.
    !
    ! Returns the number of selections.
    !-

    count = hl_gtk_listn_get_selections(list, indices, selection)

  end function hl_gtk_list1_get_selections

  !+
  subroutine  hl_gtk_list1_set_selection(list, row)
    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in), optional :: row

    ! Set the selected row in a list (single row only)
    !
    ! LIST: c_ptr: required: The list to work on.
    ! ROW: c_int: optional: The row to select (absent or .lt. 0
    ! 		is clear selection)
    !-

    call hl_gtk_listn_set_selection(list, row)

  end subroutine hl_gtk_list1_set_selection

  !+
  subroutine hl_gtk_list1_set_cell(list, row, svalue)
    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in) :: row
    character(len=*), intent(in) :: svalue

    ! Set a cell in a single column list
    !
    ! LIST: c_ptr: required: The list containing the cell.
    ! ROW: c_int: required: The row of the cell
    ! SVALUE: string: required: A string value for the cell.
    !-

    call hl_gtk_listn_set_cell(list, row, 0, svalue=svalue)

  end subroutine hl_gtk_list1_set_cell

  !+
  subroutine hl_gtk_list1_get_cell(list, row, svalue)
    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in) :: row
    character(len=*), intent(out) :: svalue

    ! Set a cell in a single column list
    !
    ! LIST: c_ptr: required: The list containing the cell.
    ! ROW: c_int: required: The row of the cell
    ! SVALUE: string: required: A string value from the cell.
    !-

    call hl_gtk_listn_get_cell(list, row, 0, svalue=svalue)

  end subroutine hl_gtk_list1_get_cell

  !+
  subroutine hl_gtk_list1_move_row(list, row1, row2, after)
    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in) :: row1
    integer(kind=c_int), intent(in), optional :: row2
    integer(kind=c_int), intent(in), optional :: after

    ! Move a row in a list to a new location
    !
    ! LIST: c_ptr: required: The list to work on.
    ! ROW1: c_int: required: The index of the row to move.
    ! ROW2: c_int: optional: The location before which to place
    ! 		the row.
    ! AFTER: boolean: optional:  Set this to TRUE to put the row after
    ! 		the location instead of before.
    !-

    call hl_gtk_listn_move_row(list, row1, row2, after)

  end subroutine hl_gtk_list1_move_row

  !+
  subroutine hl_gtk_list1_swap_rows(list, row1, row2)
    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in) :: row1, row2

    ! Move a row in a list to a new location
    !
    ! LIST: c_ptr: required: The list to work on.
    ! ROW1: c_int: required: The index of the first row to move.
    ! ROW2: c_int: required: The index of the second row to move
    !-

    call hl_gtk_listn_swap_rows(list, row1, row2)

  end subroutine hl_gtk_list1_swap_rows

  !+
  subroutine hl_gtk_list1_reorder(list, indices)
    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in), dimension(:), target :: indices

    ! Move a row in a list to a new location
    !
    ! LIST: c_ptr: required: The list to work on.
    ! INDICES: c_int(): required: The sorting array. The ith element
    ! 		contains the old location of the new (i-1)th row.
    !-

    call hl_gtk_listn_reorder(list, indices)

  end subroutine hl_gtk_list1_reorder

  !+
  function hl_gtk_list1_get_n_rows(list) result(nrows)

    integer(kind=c_int) :: nrows
    type(c_ptr), intent(in) :: list

    ! Return the number of rows in a list.
    !
    ! LIST: c_ptr: required: the list to query
    !-

    nrows=hl_gtk_listn_get_n_rows(list)
  end function hl_gtk_list1_get_n_rows

  !+
  subroutine hl_gtk_list1_set_cell_data_func(list, func, &
       & data, destroy_notify)

    type(c_ptr), intent(in) :: list
    type(c_funptr), optional :: func
    type(c_ptr), optional :: data
    type(c_funptr), optional :: destroy_notify

    ! Add a custom rendering function to a column of a list
    !
    ! LIST: c_ptr: required: The list to which to apply the rendering function
    ! FUNC: c_funptr: optional: The function (actually subroutine)
    ! 		to do the rendering (see GtkTreeCellDataFunc, for
    ! 		details). Omit or set to C_NULL_FUNPTR to remove a function.
    ! DATA: c_ptr: optional: User data to pass to the function.
    ! DESTROY_NOTIFY: c_funptr: optional: A destroy notify subroutine.
    !-

    call hl_gtk_listn_set_cell_data_func(list, 0, func, &
         & data, destroy_notify)
  end subroutine hl_gtk_list1_set_cell_data_func

  !+
  function hl_gtk_tree_new(scroll, ncols, types, changed, data, multiple,&
       & width, titles, height, swidth, align, ixpad, iypad, sensitive, &
       & tooltip, sortable, editable, colnos, edited, data_edited) result(tree)

    type(c_ptr) :: tree
    type(c_ptr), intent(out) :: scroll
    integer(kind=c_int), intent(in), optional :: ncols
    integer(kind=type_kind), dimension(:), intent(in), optional :: types
    type(c_funptr), optional :: changed
    type(c_ptr), intent(in), optional :: data
    integer(kind=c_int), intent(in), optional :: multiple
    integer(kind=c_int), intent(in), optional, dimension(:) :: width
    character(len=*), dimension(:), intent(in), optional :: titles
    integer(kind=c_int), intent(in), optional :: height, swidth
    real(kind=c_float), intent(in), optional, dimension(:) :: align
    integer(kind=c_int), intent(in), optional, dimension(:) :: ixpad, iypad
    integer(kind=c_int), intent(in), optional :: sensitive
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), intent(in), optional, dimension(:) :: sortable, editable
    integer(kind=c_int), dimension(:), allocatable, intent(out), optional, target :: colnos
    type(c_funptr), optional :: edited
    type(c_ptr), optional, intent(in) :: data_edited

    ! Make a tree view
    !
    ! SCROLL: c_ptr: required: The scrollable widget to contain the tree.
    ! 		(This is used to pack the tree)
    ! NCOLS: c_int: Optional: The number of columns.
    ! TYPES: GType(): Optional: The types for each column.
    ! CHANGED: c_funptr: optional: Callback function for the "changed"
    ! 		signal to the associated selection object.
    ! DATA: c_ptr: optional: Data to be passed to/from the callback.
    ! MULTIPLE: boolean: optional: Whether multiple selections are allowed.
    ! WIDTH: integer(): optional: The width of the displayed columns.
    ! TITLES: string(): optional: Titles for the visible columns.
    ! HEIGHT: c_int: optional: The height of the display (this is
    !            actually the height of the scroll box).
    ! SWIDTH: c_int: Optional: The width for the scroll box
    ! ALIGN: c_float(): optional: The alignment of the columns
    ! IXPAD: c_int(): optional: The X-padding around the cells.
    ! IYPAD: c_int(): optional: The Y-Padding around the cells.
    ! SENSITIVE: boolean: optional: Whether the widget is intially sensitive.
    ! TOOLTIP: string: optional: Tooltip for the widget
    ! SORTABLE: boolean(): optional: Set whether the tree can be sorted
    ! 		on that column.
    ! EDITABLE: boolean(): optional: Set whether the column can be edited.
    ! COLNOS: c_int(): optional: An array of column numbers for the editing
    ! 		callback to use, must be an argument to prevent automatic
    ! 		deallocation, must be present if EDITABLE is present.
    ! EDITED: f_funptr: optional: An alternative callback for the "edited"
    ! 		signal on edited cells. N.B. Only a single callback can be set
    ! 		if different actions are needed for different columns,
    ! 		you must use the column number inside the callback.
    ! DATA_EDITED: c_ptr: optional: Data to pass to the edited callback.
    !
    ! At least one of the array arguments or NCOLS must be given.
    ! If TYPES is not given, then strings are assumed.
    !-

    integer(kind=c_int) :: ncols_all, nc, i
    integer(kind=type_kind), dimension(:), allocatable, target :: types_all

    type(c_ptr) :: model, renderer, column, select
    type(gvalue), target :: isedit
    type(c_ptr) :: pisedit

    ! First find how many columns there are.

    if (present(ncols)) then
       ncols_all = ncols
    else if (present(types)) then
       ncols_all = size(types)
    else if (present(titles)) then
       ncols_all = size(titles)
    else if (present(align)) then
       ncols_all = size(align)
    else if (present(width)) then
       ncols_all = size(width)
    else if (present(sortable)) then
       ncols_all = size(sortable)
    else if (present(ixpad)) then
       ncols_all = size(ixpad)
    else if (present(iypad)) then
       ncols_all = size(iypad)
    else if (present(editable)) then
       ncols_all = size(editable)
    else
       write(error_unit,*) "hl_gtk_tree_new: Cannot determine the number of columns"
       tree = C_NULL_PTR
       scroll=C_NULL_PTR
       return
    end if

    ! Now determine the column types.
    allocate(types_all(ncols_all))
    if (present(types)) then
       types_all = types
    else
       types_all = (/ (ncols_all-1)*g_type_string /)
    end if

    ! If editable is present, initialize the GValue
    if (present(editable)) then
       if (.not. present(colnos)) then
          write(error_unit,*) "hl_gtk_listn_new: EDITABLE requires COLNOS"
          tree=C_NULL_PTR
          scroll=C_NULL_PTR
          return
       end if
       pisedit = c_loc(isedit)
       pisedit = g_value_init(pisedit, G_TYPE_BOOLEAN)
       allocate(colnos(ncols_all))
       colnos = (/ (i-1, i=1, ncols_all) /)
    end if

    ! Create the storage model
    model = gtk_tree_store_newv(ncols_all, c_loc(types_all))

    ! Create the tree in the scroll box
    scroll = gtk_scrolled_window_new(C_NULL_PTR, C_NULL_PTR)
    call gtk_scrolled_window_set_policy(scroll, GTK_POLICY_AUTOMATIC, &
         & GTK_POLICY_AUTOMATIC)
    tree = gtk_tree_view_new_with_model(model)
    call gtk_container_add(scroll, tree)
    if (present(height) .and. present(swidth)) then
       call gtk_widget_set_size_request(scroll,swidth,height)
    else if (present(height)) then
       call gtk_widget_set_size_request(scroll,0,height)
    else if (present(swidth)) then
       call gtk_widget_set_size_request(scroll,swidth,0)
    end if

    ! Set up the columns
    do i = 1, ncols_all
       renderer = gtk_cell_renderer_text_new()
       if (present(ixpad) .and. present(iypad)) then
          call gtk_cell_renderer_set_padding(renderer, &
               & ixpad(i), iypad(i))
       else if (present(ixpad)) then
          call gtk_cell_renderer_set_padding(renderer, &
               & ixpad(i), 0)
       else if (present(iypad)) then
          call gtk_cell_renderer_set_padding(renderer, &
               & 0, iypad(i))
       end if
       if (present(align)) then
          call gtk_cell_renderer_set_alignment(renderer, align(i), 0.)
       else if (types_all(i) == G_TYPE_STRING) then
          call gtk_cell_renderer_set_alignment(renderer, 0., 0.)
       else
          call gtk_cell_renderer_set_alignment(renderer, 1., 0.)
       end if
       if (present(editable)) then
          call g_value_set_boolean(pisedit, editable(i))
          call g_object_set_property(renderer, "editable"//c_null_char, pisedit)
          if (editable(i) == TRUE) then
             call g_object_set_data(renderer, "column-number"//c_null_char, &
                  & c_loc(colnos(i)))
             call g_object_set_data(renderer, "view"//c_null_char, tree)
             if (present(edited)) then
                if (present(data_edited)) then
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited, data_edited)
                else
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited)
                end if
             else
                call g_signal_connect(renderer, "edited"//c_null_char, &
                     & c_funloc(hl_gtk_tree_edit_cb))
             endif
          end if
       end if

       column = gtk_tree_view_column_new()
       call gtk_tree_view_column_pack_start(column, renderer, FALSE)
       if (present(titles)) call gtk_tree_view_column_set_title(column, &
            &trim(titles(i))//c_null_char)
       call gtk_tree_view_column_add_attribute(column, renderer, &
            & "text"//C_NULL_CHAR, i-1)
       nc = gtk_tree_view_append_column(tree, column)
       if (present(sortable)) then
          if (sortable(i) == TRUE) then
             call gtk_tree_view_column_set_sort_column_id(column, i-1)
             call gtk_tree_view_column_set_sort_indicator(column, TRUE)
          end if
       end if
       if (present(width)) then
          call gtk_tree_view_column_set_sizing (column, &
               & GTK_TREE_VIEW_COLUMN_FIXED)
          call gtk_tree_view_column_set_fixed_width(column, width(i))
       end if
       call gtk_tree_view_column_set_resizable(column,TRUE)
    end do

    ! The event handler is attached to the selection object, as is
    ! the multiple selection property.

    select = gtk_tree_view_get_selection(tree)

    if (present(multiple)) then
       if (multiple == TRUE) &
            & call gtk_tree_selection_set_mode(select, GTK_SELECTION_MULTIPLE)
    end if

    if (present(changed)) then
       if (present(data)) then
          call g_signal_connect(select, "changed"//c_null_char, changed, data)
       else
          call g_signal_connect(select, "changed"//c_null_char, changed)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(tree, tooltip)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(tree, sensitive)

    deallocate(types_all)
  end function hl_gtk_tree_new

  !+
  subroutine hl_gtk_tree_edit_cb(renderer, path, text, gdata) bind(c)
    type(c_ptr), value :: renderer, path, text, gdata

    ! Default callback for tree cell edited.
    !
    ! RENDERER: c_ptr: required: The renderer which sent the signal
    ! PATH: c_ptr: required: The path at which to insert
    ! TEXT: c_ptr: required: The text to insert
    ! GDATA: c_ptr: required: User data, not used.
    !
    ! The column number is passed via the "column-number" gobject data value.
    ! The treeview containing the cell is passed via the "view" gobject
    ! data value.
    ! The row number is passed as a string in the PATH argument.
    !
    ! This routine is not normally called by the application developer.
    !-

    character(len=200) :: fpath, ftext
    integer(kind=c_int), allocatable, dimension(:) :: irow
    integer(kind=c_int), pointer :: icol
    integer :: ios, i, n
    type(c_ptr) :: tree, pcol

    call convert_c_string(path, 200, fpath)
    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)
    call convert_c_string(text, 200, ftext)

    n = 0
    do i = 1, len_trim(fpath)
       if (fpath(i:i) == ":") then
          n = n+1
          fpath(i:i) = ' '   ! : is not a separator for a Fortran read
       end if
    end do
    allocate(irow(n+1))
    read(fpath, *) irow
    tree = g_object_get_data(renderer, "view"//c_null_char)

    call hl_gtk_tree_set_cell(tree, irow, icol, &
         & svalue=trim(ftext))

    deallocate(irow)
  end subroutine hl_gtk_tree_edit_cb

  !+
  subroutine hl_gtk_tree_ins(tree, row, absrow)

    type(c_ptr), intent(in) :: tree
    integer(kind=c_int), intent(in), optional, dimension(:) :: row
    integer(kind=c_int), intent(in), optional :: absrow

    ! Insert a row into a tabular tree.
    !
    ! TREE: c_ptr: required: The tree into which to insert the row.
    ! ROW: c_int(): optional: The row BEFORE which to insert the row
    ! 		(append if an element is -1) For example; to put a new row
    ! 		after all other children of the second child of the fifth
    ! 		top-level row use (/ 4, 1, -1 /).
    ! ABSROW: c_int: optional: The row BEFORE which to insert the new row
    ! 		treating the tree as a flat list.
    !-

    type(c_ptr) :: store
    type(gtktreeiter), target :: iter1, iter2
    integer(kind=c_int) :: valid
    integer :: i, ndep

    ! Get the TreeStore
    store = gtk_tree_view_get_model(tree)

    ! Insert the row (we don't use the "scanner" here because of the
    ! special case of -1 for append at a level).
    if (present(row)) then
       ndep = size(row)
       if (ndep == 1) then
          if (row(1) < 0) then
             call gtk_tree_store_append(store, c_loc(iter1), C_NULL_PTR)
          else
             call gtk_tree_store_insert(store, c_loc(iter1), C_NULL_PTR, row(1))
          end if
       else
          do i = 1, size(row)-1
             if (i == 1) then
                valid=gtk_tree_model_iter_nth_child(store, c_loc(iter1), &
                     & C_NULL_PTR, row(1))
             else
                valid=gtk_tree_model_iter_nth_child(store, c_loc(iter1), &
                     & c_loc(iter2), row(i))
             end if
             if (valid == FALSE) then
                write(error_unit,*) "hl_gtk_tree_ins:: Row description does not point to an insertable location"
                return
             end if
             iter2 = iter1
          end do
          call clear_gtktreeiter(iter1)
          if (row(ndep) < 0) then
             call gtk_tree_store_append(store, c_loc(iter1), c_loc(iter2))
          else
             call gtk_tree_store_insert(store, c_loc(iter1), c_loc(iter2), row(ndep))
          end if
       end if
    else if (present(absrow)) then
       if (absrow < 0) then
          call gtk_tree_store_append(store, c_loc(iter1), C_NULL_PTR)
       else if (absrow == 0) then
          call gtk_tree_store_prepend(store, c_loc(iter1), C_NULL_PTR)
       else
          valid = hl_gtk_tree_abs_iter(tree, iter1, absrow)
          if (valid == FALSE) then
             write(error_unit,*) "hl_gtk_tree_ins:: Row description does not point to an insertable location"
             return
          end if
          call clear_gtktreeiter(iter2)
          call gtk_tree_store_insert_before(store, c_loc(iter2), C_NULL_PTR, &
               & c_loc(iter1))
       end if
    end if
  end subroutine hl_gtk_tree_ins

  !+
  function hl_gtk_tree_abs_iter(tree, iter, index, model) result(valid)

    integer(kind=c_int) :: valid
    type(c_ptr), intent(in) :: tree
    type(gtktreeiter), intent(out), target :: iter
    integer(kind=c_int), intent(in) :: index
    type(c_ptr), intent(in), optional :: model

    ! Get the indexth iterator of a tree (treating it as a flat list)
    !
    ! TREE: c_ptr: required: The tree to traverse
    ! ITER: gtktreeiter: required: The iterator found
    ! INDEX: c_int: required:  The location to be identified
    ! MODEL: c_ptr: optional: The tree model (if this is given then TREE is
    ! 		ignored)
    !
    ! Returns TRUE if the search was successful, FALSE otherwise (not usually
    ! called directly by applications).
    !-

    type(gtktreeiter), target :: iter2
    integer(kind=c_int) :: irow
    type(c_ptr) :: store

    ! Get the TreeStore
    if (present(model)) then
       store = model
    else
       store = gtk_tree_view_get_model(tree)
    end if

    irow=0
    ! Get the first iterator
    valid = gtk_tree_model_get_iter_first(store, c_loc(iter))
    if (valid==FALSE .or. index == 0) return
    do
       valid = gtk_tree_model_iter_children(store, c_loc(iter2), c_loc(iter))
       if (valid == FALSE) then  ! no children
          valid = gtk_tree_model_iter_next(store, c_loc(iter))
          if (valid == FALSE) then ! no later sibling
             valid = gtk_tree_model_iter_parent(store, c_loc(iter2),&
                  & c_loc(iter))
             if (valid == FALSE) return ! back to the top level
             iter=iter2
          else
             irow = irow+1
             if (irow == index) return
          end if
       else
          irow = irow+1
          iter = iter2
          if (irow == index) return
       end if
    end do
  end function hl_gtk_tree_abs_iter

  !+
  function hl_gtk_tree_row_iter(tree, iter, row, model) result(valid)

    type(gtktreeiter), target :: iter
    type(c_ptr), intent(in) :: tree
    integer(kind=c_int), intent(in), dimension(:) :: row
    type(c_ptr), intent(in), optional :: model

    ! Get the iterator for a given row of the tree
    !
    ! TREE: c_ptr: required: The tree to traverse
    ! ITER: gtktreeiter: required: The iterator found
    ! ROW: c_int(): required: The row specifier
    ! MODEL: c_ptr: optional: The tree model (if this is given then TREE is
    ! 		ignored)
    !-

    type(gtktreeiter), target :: iter2
    integer :: i, ndep
    type(c_ptr) :: store
    integer(kind=c_int) :: valid

    ! Get the TreeStore
    if (present(model)) then
       store = model
    else
       store = gtk_tree_view_get_model(tree)
    end if

    ndep = size(row)
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), C_NULL_PTR, row(1))

    if (ndep == 1) return
    do i = 2, size(row)
       iter2 = iter
       valid=gtk_tree_model_iter_nth_child(store, c_loc(iter), &
            & c_loc(iter2), row(i))
       if (valid == FALSE) return   ! Invalid specifier
    end do
  end function hl_gtk_tree_row_iter

  !+
  subroutine hl_gtk_tree_rem(tree, row, absrow)

    type(c_ptr), intent(in) :: tree
    integer(kind=c_int), optional, intent(in), dimension(:) :: row
    integer(kind=c_int), intent(in), optional :: absrow

    ! Remove a row or clear a tree
    !
    ! TREE: c_ptr: required: The tree to modify
    ! ROW: integer(): optional: The row to remove, if absent clear the tree
    ! ABSROW: c_int: optional: The row to remove, treating the tree as a
    ! 		flat list.
    !-

    integer(kind=c_int) :: valid
    type(c_ptr) :: store
    type(gtktreeiter), target :: iter

    ! Get tree store
    store = gtk_tree_view_get_model(tree)

    ! If 2 arguments, then remove a row
    if (present(row)) then
       valid = hl_gtk_tree_row_iter(tree, iter, row)
    else if (present(absrow)) then
       valid = hl_gtk_tree_abs_iter(tree, iter, absrow)
    else
       call gtk_tree_store_clear(store)
       return
    end if
    if (valid == FALSE) then
       write(error_unit,*) "hl_gtk_tree_rem: Specified row does not exist"
       return
    end if

    valid = gtk_tree_store_remove(store, c_loc(iter))
  end subroutine hl_gtk_tree_rem

  !+
  function hl_gtk_tree_get_selections(tree, indices, depths, &
       & selection) result(count)

    integer(kind=c_int) :: count
    type(c_ptr), intent(in) :: tree
    integer(kind=c_int), dimension(:,:), allocatable, target, &
         & intent(out), optional :: indices
    integer(kind=c_int), dimension(:), allocatable, target, &
         & intent(out), optional :: depths
    type(c_ptr), optional :: selection

    ! Get the indices of the selected rows
    !
    ! TREE: c_ptr: required: The tree whose selections are to be found.
    ! INDICES: c_int(,): optional: An allocatable array to return the
    ! 		tree of selections. (If count = 0 it will not be allocated).
    ! 		If this argument is not given, then the number of
    ! 		selected rows is returned.
    ! DEPTHS: c_int(): optional: An allocatable array to return the depth
    ! 		of each selection. (Strictly the last meaningful element
    ! 		of each row of INDICES).
    ! SELECTION: c_ptr: optional: A selection. If this is given then TREE
    !           is ignored. This is most often used in the callback routine
    !           for the changed signal when that needs to find which element(s)
    !           are selected.
    !
    ! Returns the number of selections.
    !-

    type(c_ptr) :: slist, vselection
    type(c_ptr), target :: model
    integer(kind=c_int) :: i
    integer(kind=c_int) :: maxdepth
    integer(kind=c_int), dimension(:), pointer :: idxl
    integer(kind=c_int), target :: dep
    type(c_ptr) :: idxlc

    if (present(selection)) then
       vselection = selection
    else
       vselection = gtk_tree_view_get_selection(tree)
    end if

    slist = gtk_tree_selection_get_selected_rows(vselection, &
         & c_loc(model))

    ! If no selections, then set the count to 0 and return
    if (.not. c_associated(slist)) then
       count=0
       return
    end if

    ! Determine how many rows are selected. Then if no output list was
    ! supplied, return, otherwise go on and make a list.
    count = g_list_length(slist)
    if (.not. present(indices)) return

    ! For each of the elements in the selection list, find its index
    ! from the hidden first column
    maxdepth = 0
    do i = 1, count
       maxdepth = max(maxdepth, &
            & gtk_tree_path_get_depth(g_list_nth_data(slist, i-1))+1)
    end do

    allocate(indices(maxdepth,count))
    if (present(depths)) allocate(depths(count))

    do i = 1, count
       idxlc = gtk_tree_path_get_indices_with_depth(g_list_nth_data(slist,i-1),&
            & c_loc(dep))
       call c_f_pointer(idxlc, idxl, (/ dep /) )
       indices(:dep,i) = idxl
       if (present(depths)) depths(i) = dep
    end do

    ! Free the selection list.
    call g_list_foreach(slist, c_funloc(gtk_tree_path_free), C_NULL_PTR)
    call g_list_free(slist)

  end function hl_gtk_tree_get_selections

  !+
  subroutine hl_gtk_tree_set_cell(tree, row, col, absrow, &
       & svalue, fvalue, dvalue, ivalue, lvalue, l64value)

    type(c_ptr), intent(in) :: tree
    integer(kind=c_int), intent(in), optional :: absrow, col
    integer(kind=c_int), intent(in), optional, dimension(:) :: row
    character(len=*), intent(in), optional :: svalue
    real(kind=c_float), intent(in), optional :: fvalue
    real(kind=c_double), intent(in), optional :: dvalue
    integer(kind=c_int), intent(in), optional :: ivalue
    integer(kind=c_long), intent(in), optional :: lvalue
    integer(kind=c_int64_t), intent(in), optional :: l64value

    ! Set the value of a cell.
    !
    ! TREE: c_ptr: required: The tree containing the cell.
    ! ROW: c_int(): optional: The row of the cell
    ! COL: c_int: optional: The column of the cell, (Only optional to
    ! 		allow format similar to the LISTs).
    ! ABSROW: c_int: optional: The row, treating the tree as a flat list.
    ! SVALUE: string: optional: A string value for the cell.
    ! FVALUE: float: optional: A single precision FP value for the cell.
    ! DVALUE: double: optional: A double precision FP value for the cell.
    ! IVALUE: c_int: optional: A normal integer value for the cell.
    ! LVALUE: c_long: optional: A long integer value for the cell.
    ! L64VALUE: c_int64_t: optional: A 64-bit integer value for the cell.
    !
    ! Note that reasonable conversions are made between types.
    !-

    integer(kind=type_kind) :: ctype
    type(c_ptr) :: store, val
    integer(kind=c_int) :: valid, icol
    type(gtktreeiter), target :: iter
    type(gvalue), target :: value

    character(len=120) :: sconv
    integer(kind=c_int) :: iconv
    integer(kind=c_long) :: lconv
    integer(kind=c_int64_t) :: l64conv
    real(kind=c_float) :: fconv
    real(kind=c_double) :: dconv
    integer :: ios

    ! Get tree store
    store = gtk_tree_view_get_model(tree)

    if (present(col)) then
       icol=col
    else
       icol=0
    end if

    ! Find the type for the requested column
    ctype = gtk_tree_model_get_column_type(store, icol)

    ! Get the iterator of the row
    if (present(row)) then
       valid = hl_gtk_tree_row_iter(C_NULL_PTR, iter, row, model=store)
    else if (present(absrow)) then
       valid = hl_gtk_tree_abs_iter(C_NULL_PTR, iter, absrow, model=store)
    else
       valid=FALSE
       return
    end if

    if (valid == FALSE) return

    ! Set up the GValue to the right type.
    val = c_loc(value)
    val = g_value_init(val, ctype)

    ! Select according to the cell type
    select case(ctype)
    case(G_TYPE_CHAR)
       if (present(svalue)) then
          call g_value_set_schar(val, ichar(svalue(1:1), c_int8_t))
       else if (present(ivalue)) then
          call g_value_set_schar(val, int(ivalue, c_int8_t))
       else if (present(lvalue)) then
          call g_value_set_schar(val, int(lvalue, c_int8_t))
       else if (present(l64value)) then
          call g_value_set_schar(val, int(l64value, c_int8_t))
       else
          write(error_unit,*) "hl_gtk_tree_set_cell:: Cannot make a 'char' type from given value(s)"
          return
       end if
    case(G_TYPE_UCHAR)
       if (present(svalue)) then
          call g_value_set_uchar(val, svalue(1:1))
       else if (present(ivalue)) then
          call g_value_set_uchar(val, char(ivalue, c_char))
       else if (present(lvalue)) then
          call g_value_set_uchar(val, char(lvalue, c_char))
       else if (present(l64value)) then
          call g_value_set_uchar(val, char(l64value, c_char))
       else
          write(error_unit,*) "hl_gtk_tree_set_cell:: Cannot make a 'char' type from given value(s)"
          return
       end if

    case (G_TYPE_INT)
       if (present(ivalue)) then
          call g_value_set_int(val, ivalue)
       else if (present(lvalue)) then
          call g_value_set_int(val, int(lvalue, c_int))
       else if (present(l64value)) then
          call g_value_set_int(val, int(l64value, c_int))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) iconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'int'"
             return
          end if
          call g_value_set_int(val, iconv)
       else
          write(error_unit,*) "hl_gtk_tree_set_cell:: Cannot make an 'int' type from given value(s)"
          return
       end if
    case (G_TYPE_UINT)
       if (present(ivalue)) then
          call g_value_set_uint(val, ivalue)
       else if (present(lvalue)) then
          call g_value_set_uint(val, int(lvalue, c_int))
       else if (present(l64value)) then
          call g_value_set_uint(val, int(l64value, c_int))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) iconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'int'"
             return
          end if
          call g_value_set_uint(val, iconv)
       else
          write(error_unit,*) "hl_gtk_tree_set_cell:: Cannot make an 'int' type from given value(s)"
          return
       end if
    case (G_TYPE_BOOLEAN)
       if (present(ivalue)) then
          call g_value_set_boolean(val, ivalue)
       else if (present(lvalue)) then
          call g_value_set_boolean(val, int(lvalue, c_int))
       else if (present(l64value)) then
          call g_value_set_boolean(val, int(l64value, c_int))
       else if (present(svalue)) then
          if (svalue=='T' .or. svalue=='t' .or. svalue=='TRUE' .or. &
               & svalue=='true' .or. svalue=='True' .or. svalue=='Y' &
               & .or. svalue=='y' .or. svalue=='YES' .or. svalue=='yes' &
               & .or. svalue=='Yes' .or. svalue=='.TRUE.' .or. &
               & svalue=='.true.') then
             call g_value_set_boolean(val, TRUE)
          else if (svalue=='F' .or. svalue=='f' .or. svalue=='FALSE' .or. &
               & svalue=='false' .or. svalue=='False' .or. svalue=='N' .or. &
               & svalue=='n' .or. svalue=='NO' .or. svalue=='no' .or. &
               & svalue=='No' .or. svalue=='.FALSE.' .or. &
               & svalue=='.false.') then
             call g_value_set_boolean(val, FALSE)
          else
             read(svalue,*,iostat=ios) iconv
             if (ios /= 0) then
                write(error_unit,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'int'"
                return
             end if
             call g_value_set_boolean(val, iconv)
          end if
       else
          write(error_unit,*) "hl_gtk_tree_set_cell:: Cannot make an 'int' type from given value(s)"
          return
       end if

    case (G_TYPE_LONG)
       if (present(lvalue)) then
          call g_value_set_long(val, lvalue)
       else if (present(l64value)) then
          call g_value_set_long(val, int(l64value, c_long))
       else if (present(ivalue)) then
          call g_value_set_long(val, int(ivalue, c_long))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) lconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'long'"
             return
          end if
          call g_value_set_long(val, lconv)
       else
          write(error_unit,*) "hl_gtk_tree_set_cell:: Cannot make a 'long' type from given value(s)"
          return
       end if
    case (G_TYPE_ULONG)
       if (present(lvalue)) then
          call g_value_set_ulong(val, lvalue)
       else if (present(l64value)) then
          call g_value_set_ulong(val, int(l64value, c_long))
       else if (present(ivalue)) then
          call g_value_set_ulong(val, int(ivalue, c_long))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) lconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'long'"
             return
          end if
          call g_value_set_ulong(val, lconv)
       else
          write(error_unit,*) "hl_gtk_tree_set_cell:: Cannot make a 'long' type from given value(s)"
          return
       end if

    case (G_TYPE_INT64)
       if (present(l64value)) then
          call g_value_set_int64(val, l64value)
       else if (present(lvalue)) then
          call g_value_set_int64(val, int(lvalue, c_int64_t))
       else if (present(ivalue)) then
          call g_value_set_int64(val, int(ivalue, c_int64_t))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) l64conv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'int64'"
             return
          end if
          call g_value_set_int64(val, l64conv)
       else
          write(error_unit,*) "hl_gtk_tree_set_cell:: Cannot make an 'int64' type from given value(s)"
          return
       end if
    case (G_TYPE_UINT64)
       if (present(l64value)) then
          call g_value_set_uint64(val, l64value)
       else if (present(lvalue)) then
          call g_value_set_uint64(val, int(lvalue, c_int64_t))
       else if (present(ivalue)) then
          call g_value_set_uint64(val, int(ivalue, c_int64_t))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) l64conv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'int64'"
             return
          end if
          call g_value_set_uint64(val, l64conv)
       else
          write(error_unit,*) "hl_gtk_tree_set_cell:: Cannot make an 'int64' type from given value(s)"
          return
       end if

    case(G_TYPE_FLOAT)
       if (present(fvalue)) then
          call g_value_set_float(val, fvalue)
       else if (present(dvalue)) then
          call g_value_set_float(val, real(dvalue, c_float))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) fconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'float'"
             return
          end if
          call g_value_set_float(val, fconv)
       else
          write(error_unit,*) "hl_gtk_tree_set_cell:: Cannot make a 'float' type from given value(s)"
          return
       end if

    case(G_TYPE_DOUBLE)
       if (present(dvalue)) then
          call g_value_set_double(val, dvalue)
       else if (present(fvalue)) then
          call g_value_set_double(val, real(fvalue, c_double))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) dconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'double'"
             return
          end if
          call g_value_set_double(val, dconv)
       else
          write(error_unit,*) "hl_gtk_tree_set_cell:: Cannot make a 'double' type from given value(s)"
          return
       end if

    case (G_TYPE_STRING)
       if (present(svalue)) then
          call g_value_set_string(val, trim(svalue)//c_null_char)
       else
          if (present(ivalue)) then
             write(sconv,*) ivalue
          else if (present(lvalue)) then
             write(sconv,*) lvalue
          else if (present(l64value)) then
             write(sconv,*) l64value
          else if (present(fvalue)) then
             write(sconv,*) fvalue
          else if (present(dvalue)) then
             write(sconv,*) dvalue
          else
             write(error_unit,*) "hl_gtk_tree_set_cell:: Cannot make a 'string' type from given value(s)"
             return
          end if
          call g_value_set_string(val, trim(sconv)//c_null_char)
       end if

    case default
       write(error_unit,*)  "hl_gtk_tree_set_cell:: Cell type ",ctype," is unknown"
       return
    end select

    call gtk_tree_store_set_value(store, c_loc(iter), icol, val)

  end subroutine hl_gtk_tree_set_cell

  !+
  subroutine hl_gtk_tree_get_cell(tree, row, col, absrow, &
       & svalue, fvalue, dvalue, ivalue, lvalue, l64value)

    type(c_ptr), intent(in) :: tree
    integer(kind=c_int), intent(in), optional :: absrow, col
    integer(kind=c_int), intent(in), optional, dimension(:) :: row
    character(len=*), intent(out), optional :: svalue
    real(kind=c_float), intent(out), optional :: fvalue
    real(kind=c_double), intent(out), optional :: dvalue
    integer(kind=c_int), intent(out), optional :: ivalue
    integer(kind=c_long), intent(out), optional :: lvalue
    integer(kind=c_int64_t), intent(out), optional :: l64value

    ! Retrieve the value of a cell.
    !
    ! TREE: c_ptr: required: The tree containing the cell.
    ! ROW: c_int(): optional: The row of the cell
    ! COL: c_int: optional: The column of the cell. (Only optional to
    ! 		allow format similar to the LISTs).
    ! ABSROW: c_int: optional: The row of the cell, treating the tree as
    ! 		a flat list.
    ! SVALUE: string: optional: A string value from the cell.
    ! FVALUE: float: optional: A single precision FP value from the cell.
    ! DVALUE: double: optional: A double precision FP value from the cell.
    ! IVALUE: c_int: optional: A normal integer value from the cell.
    ! LVALUE: c_long: optional: A long integer value from the cell.
    ! L64VALUE: c_int64_t: optional: A 64-bit integer value from the cell.
    !
    ! Note that a similar conversion system to the set_cell routine
    ! except that strings can only be returned to SVALUE.
    !-

    integer(kind=type_kind) :: ctype
    type(c_ptr) :: store, val, cstr
    integer(kind=c_int) :: valid, icol
    type(gtktreeiter), target :: iter
    type(gvalue), target :: value

    if (present(col)) then
       icol=col
    else
       icol=0
    end if

    ! Get tree store
    store = gtk_tree_view_get_model(tree)

    ! Find the type for the requested column
    ctype = gtk_tree_model_get_column_type(store, icol)

    ! Get the iterator of the row
    if (present(row)) then
       valid = hl_gtk_tree_row_iter(C_NULL_PTR, iter, row, model=store)
    else if (present(absrow)) then
       valid = hl_gtk_tree_abs_iter(C_NULL_PTR, iter, absrow, model=store)
    else
       valid = FALSE
    end if
    if (valid == FALSE) return

    ! Set up the GValue pointer (for convenience) gtk_tree_model_get_value
    ! does the initialization.
    val = c_loc(value)

    ! Get the GValue of the cell.
    call gtk_tree_model_get_value(store, c_loc(iter), icol, val)

    ! Now extract the value to a useful form according to the type
    ! of cell.

    select case(ctype)
    case(G_TYPE_CHAR)
       if (present(svalue)) then
          svalue(1:1) = char(g_value_get_schar(val))
       else if (present(ivalue)) then
          ivalue = int(g_value_get_schar(val))
       else if (present(lvalue)) then
          lvalue = int(g_value_get_schar(val), c_long)
       else if (present(l64value)) then
          l64value = int(g_value_get_schar(val), c_int64_t)
       else
          write(error_unit,*) "hl_gtk_tree_get_cell:: Cannot return 'char' type to any available output"
          return
       end if
    case(G_TYPE_UCHAR)
       if (present(svalue)) then
          svalue(1:1)= g_value_get_uchar(val)
       else if (present(ivalue)) then
          ivalue = ichar(g_value_get_uchar(val))
       else if (present(lvalue)) then
          lvalue = ichar(g_value_get_uchar(val))
       else if (present(l64value)) then
          l64value = ichar(g_value_get_uchar(val))
       else
          write(error_unit,*) "hl_gtk_tree_get_cell:: Cannot return 'char' type to any available output"
          return
       end if

    case (G_TYPE_INT)
       if (present(ivalue)) then
          ivalue = g_value_get_int(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_int(val), c_long)
       else if (present(l64value)) then
          l64value = int(g_value_get_int(val), c_int64_t)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_int(val)
       else
          write(error_unit,*) "hl_gtk_tree_get_cell:: Cannot return 'int' type to any available output"
          return
       end if
    case (G_TYPE_UINT)
       if (present(ivalue)) then
          ivalue = g_value_get_uint(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_uint(val), c_long)
       else if (present(l64value)) then
          l64value = int(g_value_get_uint(val), c_int64_t)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_uint(val)
       else
          write(error_unit,*) "hl_gtk_tree_get_cell:: Cannot return 'int' type to any available output"
          return
       end if
    case (G_TYPE_BOOLEAN)
       if (present(ivalue)) then
          ivalue = g_value_get_boolean(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_boolean(val), c_long)
       else if (present(l64value)) then
          l64value = int(g_value_get_boolean(val), c_int64_t)
       else if (present(svalue)) then
          if (g_value_get_boolean(val) == TRUE) then
             svalue = 'True'
          else
             svalue='False'
          end if
       else
          write(error_unit,*) "hl_gtk_tree_get_cell:: Cannot return 'bool' type to any available output"
          return
       end if

    case (G_TYPE_LONG)
       if (present(lvalue)) then
          lvalue = g_value_get_long(val)
       else if (present(l64value)) then
          l64value = int(g_value_get_long(val), c_int64_t)
       else if (present(ivalue)) then
          ivalue = int(g_value_get_long(val), c_int)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_long(val)
       else
          write(error_unit,*) "hl_gtk_tree_get_cell:: Cannot return 'long' type to any available output"
          return
       end if
    case (G_TYPE_ULONG)
       if (present(lvalue)) then
          lvalue = g_value_get_ulong(val)
       else if (present(l64value)) then
          l64value = int(g_value_get_ulong(val), c_int64_t)
       else if (present(ivalue)) then
          ivalue = int(g_value_get_ulong(val), c_int)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_ulong(val)
       else
          write(error_unit,*) "hl_gtk_tree_get_cell:: Cannot return 'long' type to any available output"
          return
       end if

    case (G_TYPE_INT64)
       if (present(l64value)) then
          l64value = g_value_get_int64(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_int64(val), c_long)
       else if (present(ivalue)) then
          ivalue = int(g_value_get_int64(val), c_int)
       else if (present(svalue)) then
          write (svalue,*) g_value_get_int64(val)
       else
          write(error_unit,*) "hl_gtk_tree_get_cell:: Cannot return 'int64' type to any available output"
          return
       end if
    case (G_TYPE_UINT64)
       if (present(l64value)) then
          l64value = g_value_get_uint64(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_uint64(val), c_long)
       else if (present(ivalue)) then
          ivalue = int(g_value_get_uint64(val), c_int)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_uint64(val)
       else
          write(error_unit,*) "hl_gtk_tree_get_cell:: Cannot return 'int64' type to any available output"
          return
       end if

    case(G_TYPE_FLOAT)
       if (present(fvalue)) then
          fvalue = g_value_get_float(val)
       else if (present(dvalue)) then
          dvalue = real(g_value_get_float(val), c_double)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_float(val)
       else
          write(error_unit,*) "hl_gtk_tree_get_cell:: Cannot return 'float' type to any available output"
          return
       end if

    case(G_TYPE_DOUBLE)
       if (present(dvalue)) then
          dvalue = g_value_get_double(val)
       else if (present(fvalue)) then
          fvalue = real(g_value_get_double(val), c_float)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_double(val)
       else
          write(error_unit,*) "hl_gtk_tree_get_cell:: Cannot return 'double' type to any available output"
          return
       end if

    case (G_TYPE_STRING)
       if (present(svalue)) then
          cstr = g_value_get_string(val)
          call convert_c_string(cstr, len(svalue), svalue)
       else
          write(error_unit,*) "hl_gtk_tree_get_cell:: Cannot return 'string' type to any available output"
       end if

    case default
       write(error_unit,*)  "hl_gtk_tree_get_cell:: Cell type ",ctype," is unknown"
       return
    end select
  end subroutine hl_gtk_tree_get_cell

  !+
  subroutine hl_gtk_tree_set_cell_data_func(list, colno, func, &
       & data, destroy_notify)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in) :: colno
    type(c_funptr), optional :: func
    type(c_ptr), optional :: data
    type(c_funptr), optional :: destroy_notify

    ! Add a custom rendering function to a column of a tree
    !
    ! LIST: c_ptr: required: The list to which to apply the rendering function
    ! FUNC: c_funptr: optional: The function (actually subroutine)
    ! 		to do the rendering (see GtkTreeCellDataFunc, for
    ! 		details). Omit or set to C_NULL_FUNPTR to remove a function.
    ! DATA: c_ptr: optional: User data to pass to the function.
    ! DESTROY_NOTIFY: c_funptr: optional: A destroy notify subroutine.
    !-

    call hl_gtk_listn_set_cell_data_func(list, colno, func, &
         & data, destroy_notify)
  end subroutine hl_gtk_tree_set_cell_data_func
end module gtk_hl_tree
