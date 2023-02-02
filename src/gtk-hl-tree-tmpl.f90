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
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Contributed by James Tappin
! Last modification: 07-30-2012

!!$T Template file for gtk-hl-tree.f90.
!!$T  Make edits to this file, and keep them identical between the
!!$T  GTK2 & GTK3 branches.

!!$T Lines to appear only in specific versions should be prefixed by
!!$T !!$<lib><op><ver>!
!!$T Where <lib> is GTK or GLIB, <op> is one of < > <= >=
!!$T and <ver> is the version boundary, e.g. !!$GTK<=2.24! to include
!!$T the line in GTK+ version 2.24 and higher. 
!!$T The mk_gtk_hl.pl script should be used to generate the source file.

!*
! Lists and Trees
module gtk_hl_tree
  ! These functions attempt to hide some of the complexity of the GtkTreeView
  ! system of widgets and object, while still allowing the main functionality
  ! to be accessed. Most possible renderers are supported (spinner is not).
  !
  ! There are three types of list and tree supported.
  ! 
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
       & gtk_tree_model_iter_children, gtk_tree_model_iter_n_children,&
       & gtk_tree_model_iter_next, gtk_tree_model_iter_nth_child,&
       & gtk_tree_model_iter_parent, gtk_tree_path_free,&
       & gtk_tree_model_get_string_from_iter, &
       & gtk_tree_model_get_iter_from_string, &
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
       & gtk_tree_view_column_add_attribute, gtk_tree_view_column_new,&
       & gtk_tree_view_column_pack_start,&
       & gtk_tree_view_column_set_cell_data_func,&
       & gtk_tree_view_column_set_fixed_width,&
       & gtk_tree_view_column_set_resizable,&
       & gtk_tree_view_column_set_sizing,&
       & gtk_tree_view_column_set_sort_column_id,&
       & gtk_tree_view_column_set_sort_indicator,&
       & gtk_tree_view_column_set_title, gtk_tree_view_get_column,&
       & gtk_tree_view_get_model, gtk_tree_view_get_selection,&
       & gtk_tree_view_new, gtk_tree_view_new_with_model,&
       & gtk_tree_model_get_type, &
       & gtk_widget_set_sensitive, gtk_widget_set_size_request,&
       & gtk_widget_set_tooltip_text, &
       & gtk_adjustment_set_lower, gtk_adjustment_set_upper, &
       & gtk_adjustment_set_step_increment, gtk_adjustment_new, &
       & gtk_adjustment_set_page_increment, gtk_adjustment_set_value, &
       & gtk_cell_renderer_toggle_new, gtk_cell_renderer_spin_new, &
       & gtk_cell_renderer_toggle_get_active, gtk_cell_renderer_progress_new, &
       & gtk_cell_renderer_set_fixed_size, gtk_cell_renderer_pixbuf_new, &
       & gtk_cell_renderer_combo_new, gtk_combo_box_set_active, &
       & gtk_cell_renderer_toggle_set_radio, &
       & GTK_POLICY_AUTOMATIC, GTK_TREE_VIEW_COLUMN_FIXED, &
       & GTK_SELECTION_MULTIPLE, &
       & TRUE, FALSE, g_signal_connect

  use g, only: g_list_foreach, g_list_free, g_list_length, g_list_nth,&
       & g_list_nth_data, g_object_get_data, g_object_set_data,&
       & g_object_set_property, g_value_get_boolean, &
!!$GLIB< 2.32!       & g_value_get_char, g_value_set_char, &
!!$GLIB>=2.32!       & g_value_get_schar, g_value_set_schar, &
       & g_value_get_double, g_value_get_float, g_value_get_int,&
       & g_value_get_int64, g_value_get_long, g_value_get_string,&
       & g_value_get_uchar, g_value_get_uint, g_value_get_uint64,&
       & g_value_get_ulong, g_value_init, g_value_set_boolean,&
       & g_value_set_double, g_value_set_float,&
       & g_value_set_int, g_value_set_int64, g_value_set_long,&
       & g_value_set_string, g_value_set_uchar, g_value_set_uint,&
       & g_value_set_uint64, g_value_set_ulong, g_value_unset, &
       & g_object_get_property, g_value_get_object, g_value_set_object, &
       & g_value_take_object, g_object_ref

  use gdk_pixbuf, only: gdk_pixbuf_get_type

  implicit none

  ! Interfaces to give suitable names to procedures that are
  ! identical between lists and trees. (Yes I know this is the
  ! reverse of how the construct is usually meant to be used).

  interface hl_gtk_listn_set_cell_data_func
     module procedure hl_gtk_list_tree_set_cell_data_func
  end interface hl_gtk_listn_set_cell_data_func
  interface hl_gtk_tree_set_cell_data_func
     module procedure hl_gtk_list_tree_set_cell_data_func
  end interface hl_gtk_tree_set_cell_data_func

  interface hl_gtk_listn_config_spin
     module procedure hl_gtk_list_tree_config_spin
  end interface hl_gtk_listn_config_spin
  interface hl_gtk_tree_config_spin
     module procedure hl_gtk_list_tree_config_spin
  end interface hl_gtk_tree_config_spin

  interface hl_gtk_listn_config_combo
     module procedure hl_gtk_list_tree_combo_model_config
  end interface hl_gtk_listn_config_combo
  interface hl_gtk_tree_config_combo
     module procedure hl_gtk_list_tree_combo_model_config
  end interface hl_gtk_tree_config_combo

  interface hl_gtk_listn_combo_set_model
     module procedure  hl_gtk_list_tree_combo_model_attach
  end interface hl_gtk_listn_combo_set_model
  interface hl_gtk_tree_combo_set_model
     module procedure  hl_gtk_list_tree_combo_model_attach
  end interface hl_gtk_tree_combo_set_model


  ! Renderer types (convenience constants)

  character(len=10), parameter :: hl_gtk_cell_text = 'text'
  character(len=10), parameter :: hl_gtk_cell_toggle = 'toggle'
  character(len=10), parameter :: hl_gtk_cell_radio = 'radio'
  character(len=10), parameter :: hl_gtk_cell_combo = 'combo'
  character(len=10), parameter :: hl_gtk_cell_spin  = 'spin'
  character(len=10), parameter :: hl_gtk_cell_pixbuf = 'pixbuf'
  character(len=10), parameter :: hl_gtk_cell_progress = 'progress'
  character(len=10), parameter :: hl_gtk_cell_spinner = 'spinner'

  ! Make the code-saver routines private
  private :: hl_gtk_list_tree_add_column, hl_gtk_list_tree_set_gvalue, &
       & hl_gtk_list_tree_get_gvalue, hl_gtk_list_tree_set_cell_data_func, &
       & hl_gtk_list_tree_type_adjust, hl_gtk_list_tree_config_spin, &
       & hl_gtk_list_tree_combo_model_attach, &
       & hl_gtk_list_tree_combo_model_config

contains
  !+
  function hl_gtk_listn_new(scroll, ncols, types, changed, data, multiple,&
       & width, titles, height, swidth, align, ixpad, iypad, renderers, &
       & sensitive, tooltip, sortable, editable, colnos, edited, &
       & data_edited, edited_text, data_edited_text, toggled,&
       & data_toggled, edited_spin, data_edited_spin, &
       & edited_combo, data_edited_combo, changed_combo, data_changed_combo,&
       & toggled_radio, data_toggled_radio,  &
       & hscroll_policy, vscroll_policy) result(list)

    type(c_ptr) :: list
    type(c_ptr), intent(out), optional :: scroll
    integer(kind=c_int), intent(in), optional :: ncols
    integer(kind=type_kind), dimension(:), intent(in), optional :: types
    type(c_funptr), optional :: changed
    type(c_ptr), intent(in), optional :: data
    integer(kind=c_int), intent(in), optional :: multiple
    integer(kind=c_int), intent(in), optional, dimension(:) :: width
    character(len=*), dimension(:), intent(in), optional :: titles, renderers
    integer(kind=c_int), intent(in), optional :: height, swidth
    real(kind=c_float), intent(in), optional, dimension(:) :: align
    integer(kind=c_int), intent(in), optional, dimension(:) :: ixpad, iypad
    integer(kind=c_int), intent(in), optional :: sensitive
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), intent(in), optional, dimension(:) :: sortable, editable
    type(c_funptr), optional :: edited, edited_text, toggled, edited_spin, &
         & edited_combo, changed_combo, toggled_radio
    type(c_ptr), optional, intent(in) :: data_edited, data_edited_text, &
         & data_toggled, data_edited_spin, data_edited_combo, &
         & data_changed_combo, data_toggled_radio
    integer(kind=c_int), dimension(:), allocatable, intent(out), optional, target :: colnos
    integer(kind=c_int), intent(in), optional :: hscroll_policy, vscroll_policy

    ! Make a multi column list
    !
    ! SCROLL: c_ptr: optional: A scrollable widget containing the list.
    ! 		(If present, then this is used to pack the list)
    ! NCOLS: c_int: optional: The number of columns.
    ! TYPES: GType(): optional: The types for each column.
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
    ! RENDERERS: f_string(): optional: List of renderer types.
    ! SENSITIVE: boolean: optional: Whether the widget is intially sensitive.
    ! TOOLTIP: string: optional: Tooltip for the widget
    ! SORTABLE: boolean(): optional: Set whether the list can be sorted
    ! 		on that column.
    ! EDITABLE: boolean(): optional: Set whether the column can be edited.
    ! EDITED: c_funptr: optional: An alternative callback for the "edited"
    ! 		signal on edited cells. N.B. Only a single callback can be set
    ! 		if different actions are needed for different columns,
    ! 		you must use the column number inside the callback. See
    ! 		hl_gtk_listn_edit_cb for how to access the column numbers.
    ! 		Used for all "text" dereived renderers unless overridden
    ! 		by specific callbacks.
    ! DATA_EDITED: c_ptr: optional: Data to pass to the edited callback.
    ! EDITED_TEXT: c_funptr: optional: An alternative callback for
    ! 		text renderers (not applied to derived renderers).
    ! DATA_EDITED_TEXT: c_ptr: optional: Data to pass to the edited_text
    ! 		 callback.
    ! EDITED_SPIN: c_funptr: optional: An alternative callback for
    ! 		spin button.
    ! DATA_EDITED_SPIN: c_ptr: optional: Data to pass to the edited_spin
    ! 		 callback.
    ! EDITED_COMBO: c_funptr: optional: An alternative callback for
    ! 		the "edited" signal from a combo cell
    ! DATA_EDITED_COMBO: c_ptr: optional: Data to pass to the edited_combo
    ! 		 callback.
    ! TOGGLED: c_funptr: optional: An alternative callback for the "toggled"
    ! 		signal from toggle renderers.
    ! DATA_TOGGLED: c_ptr: optional: Data to pass to the toggled callback.
    ! TOGGLED_RADIO: c_funptr: optional: An alternative callback for the
    ! 		"toggled" signal from radio toggle renderers.
    ! DATA_TOGGLED_RADIO: c_ptr: optional: Data to pass to the toggled callback
    ! 		for radio toggle renderers
    ! CHANGED_COMBO: c_funptr: optional: Callback for the "changed" signal from
    ! 		a combo cell. This is not actually all that useful as the
    ! 		edited signal will be emitted as soon as focus leaves the combo.
    ! DATA_CHANGED_COMBO: c_ptr: optional: Data to pass to the changed callback.
    ! HSCROLL_POLICY: int: optional: Horizontal scrolling policy for the
    ! 		containing scroll window (default AUTOMATIC). 
    ! VSCROLL_POLICY: int: optional: Vertical scrolling policy for the
    ! 		containing scroll window (default AUTOMATIC). 
    !
    ! At least one of the array arguments or NCOLS must be given.
    ! If TYPES is not given, then strings are assumed.
    !
    ! The following types of renderer are implemented.
    !
    ! * "text" (hl_gtk_cell_text) A standard text box (can also have
    ! 		numeric types.)
    ! * "toggle" (hl_gtk_cell_toggle) A toggle button (always
    ! 		G_TYPE_BOOLEAN).
    ! * "radio" (hl_gtk_cell_radio) A toggle button with radio button
    ! 		renderering. As far as I can see it is the programmer's
    ! 		responsibility to deal with the exclusivity--there does not
    ! 		appear to be an equivalent of the group for normal
    ! 		radio buttons.
    ! * "combo" (hl_gtk_cell_combo) A combo box--the default model supports
    ! 		strings only but user-defined models are possible.
    ! * "spin" (hl_gtk_cell_spin) A spin button. Always of type G_TYPE_DOUBLE.
    ! * "pixbuf" (hl_gtk_cell_pixbuf) A gdk_pixbuf. Always of type
    ! 		gdk_pixbuf_get_type()
    ! * "progress" (hl_gtk_cell_progress) A progress bar. Always of type
    ! 		G_TYPE_INT, not editable.
    ! * The spinner type is not (yet) implemented.
    !-

    integer(kind=c_int) :: ncols_all, i, hscroll, vscroll
    integer(kind=type_kind), dimension(:), allocatable, target :: types_all

    type(c_ptr) :: model, select

    ! Warn if the obsolete COLNOS argument is present
    if (present(colnos)) write(error_unit, *) "hl_gtk_listn_new: "//&
         & "The COLNOS argument is no longer needed."

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
    else if (present(renderers)) then
       ncols_all = size(renderers)
    else
       write(error_unit,*) &
            & "hl_gtk_listn_new: Cannot determine the number of columns"
       list = C_NULL_PTR
       scroll=C_NULL_PTR
       return
    end if

    ! Now determine the column types.
    allocate(types_all(ncols_all))
    if (present(types)) then
       types_all = types
    else
       types_all(:) = G_TYPE_STRING
    end if
    if (present(renderers)) &
         & call hl_gtk_list_tree_type_adjust(types_all, renderers)

    ! Create the storage model and the list.
    model = gtk_list_store_newv(ncols_all, c_loc(types_all))
    list = gtk_tree_view_new_with_model(model)

    if (present(scroll)) then
       if (present(hscroll_policy)) then
          hscroll = hscroll_policy
       else
          hscroll = GTK_POLICY_AUTOMATIC
       end if
       if (present(vscroll_policy)) then
          vscroll = vscroll_policy
       else
          vscroll = GTK_POLICY_AUTOMATIC
       end if

       ! Pack the list in the scroll box
       scroll = gtk_scrolled_window_new(C_NULL_PTR, C_NULL_PTR)
       call gtk_scrolled_window_set_policy(scroll, hscroll, &
            & vscroll)
       call gtk_container_add(scroll, list)

       if (present(height) .and. present(swidth)) then
          call gtk_widget_set_size_request(scroll,swidth,height)
       else if (present(height)) then
          call gtk_widget_set_size_request(scroll,-1_c_int,height)
       else if (present(swidth)) then
          call gtk_widget_set_size_request(scroll,swidth,1_c_int)
       end if
    else
       if (present(height) .and. present(swidth)) then
          call gtk_widget_set_size_request(list,swidth,height)
       else if (present(height)) then
          call gtk_widget_set_size_request(list,-1_c_int,height)
       else if (present(swidth)) then
          call gtk_widget_set_size_request(list,swidth,1_c_int)
       end if
    end if

    ! Now the visible columns
    do i = 1, ncols_all
       call hl_gtk_list_tree_add_column(i, list, .true., type=types_all(i), &
            & editable=editable, &
            & ixpad=ixpad, iypad=iypad, align=align, titles=titles, &
            & sortable=sortable, width=width, &
            & renderers=renderers, edited=edited, data_edited=data_edited, &
            & edited_text=edited_text, data_edited_text=data_edited_text, &
            & edited_spin=edited_spin, data_edited_spin=data_edited_spin, &
            & toggled=toggled, data_toggled=data_toggled, &
            & edited_combo=edited_combo, data_edited_combo=data_edited_combo, &
            & changed_combo=changed_combo, &
            & data_changed_combo=data_changed_combo, &
            & toggled_radio=toggled_radio, &
            & data_toggled_radio=data_toggled_radio)

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
  subroutine hl_gtk_listn_ins(list, row, count)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in), optional :: row, count
    
    ! Insert a row into a tabular list.
    !
    ! LIST: c_ptr: required: The list into which to insert the row.
    ! ROW: c_int: optional: The row BEFORE which to insert the row
    ! 		(append if absent)
    ! COUNT: c_int: optional: How many rows to add (default 1)
    !-

    type(c_ptr) :: store
    type(gtktreeiter), target :: iter
    integer(kind=c_int) :: i, n

    ! Get the ListStore
    store = gtk_tree_view_get_model(list)

    if (present(count)) then
       n = count
    else
       n = 1
    end if

    ! Insert the row(s)
    if (present(row)) then
       do i = 1, n
          call gtk_list_store_insert(store, c_loc(iter), row)
       end do
    else
       do i = 1, n
          call gtk_list_store_append(store, c_loc(iter))
       end do
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
       valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), &
            & C_NULL_PTR, row)
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
    integer(kind=c_int) :: i
    type(c_ptr) :: cindex
    integer(kind=c_int), pointer :: findex

    if (present(selection)) then
       vselection = selection
    else
       vselection = gtk_tree_view_get_selection(list)
    end if

    slist = gtk_tree_selection_get_selected_rows(vselection, &
         & c_null_ptr)

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
       cindex = gtk_tree_path_get_indices(g_list_nth_data(slist, i-1_c_int))
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
       valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), &
            & C_NULL_PTR, row)
       if (valid == FALSE) return
       call gtk_tree_selection_select_iter(selection, c_loc(iter))
    end if
  end subroutine hl_gtk_listn_set_selection

  !+
  subroutine hl_gtk_listn_set_cell(list, row, col, &
       & svalue, fvalue, dvalue, ivalue, lvalue, l64value, logvalue, &
       & i8value, pbvalue)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in) :: row, col
    character(len=*), intent(in), optional :: svalue
    real(kind=c_float), intent(in), optional :: fvalue
    real(kind=c_double), intent(in), optional :: dvalue
    integer(kind=c_int), intent(in), optional :: ivalue
    integer(kind=c_long), intent(in), optional :: lvalue
    integer(kind=c_int64_t), intent(in), optional :: l64value
    logical, intent(in), optional :: logvalue
    integer(kind=c_int8_t), intent(in), optional :: i8value
    type(c_ptr), intent(in), optional :: pbvalue

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
    ! LOGVALUE: logical: optional: A logical value for the cell.
    ! I8VALUE: int8_t: optional: An 8-bit integer value for the cell.
    ! PBVALUE: c_ptr: optional: A pixbuf pointer value for the cell.
    !
    ! Note that reasonable conversions are made between types.
    !-

    integer(kind=type_kind) :: ctype
    type(c_ptr) :: store, val
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

    ! Set up the GValue to the right type.
    val = c_loc(value)

    call hl_gtk_list_tree_set_gvalue(val, ctype, svalue=svalue, fvalue=fvalue, &
         & dvalue=dvalue, ivalue=ivalue, lvalue=lvalue, l64value=l64value, &
         & logvalue=logvalue, i8value=i8value, pbvalue=pbvalue)

    call gtk_list_store_set_value(store, c_loc(iter), col, val)

  end subroutine hl_gtk_listn_set_cell

  !+
  subroutine hl_gtk_listn_get_cell(list, row, col, &
       & svalue, fvalue, dvalue, ivalue, lvalue, l64value, logvalue, &
       & i8value, pbvalue)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in) :: row, col
    character(len=*), intent(out), optional :: svalue
    real(kind=c_float), intent(out), optional :: fvalue
    real(kind=c_double), intent(out), optional :: dvalue
    integer(kind=c_int), intent(out), optional :: ivalue
    integer(kind=c_long), intent(out), optional :: lvalue
    integer(kind=c_int64_t), intent(out), optional :: l64value
    logical, intent(out), optional :: logvalue
    integer(kind=c_int8_t), intent(out), optional :: i8value
    type(c_ptr), intent(out), optional :: pbvalue

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
    ! LOGVALUE: logical : optional: A logical value from the cell.
    ! I8VALUE: int8_t: optional: An 8-bit integer value from the cell.
    ! PBVALUE: c_ptr: optional: A pixbuf pointer from the cell.
    !
    ! Note that a similar conversion system to the set_cell routine
    ! except that strings can only be returned to SVALUE.
    !-

    integer(kind=type_kind) :: ctype
    type(c_ptr) :: store, val
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

    call hl_gtk_list_tree_get_gvalue(val, ctype, &
         & svalue=svalue, fvalue=fvalue, dvalue=dvalue, ivalue=ivalue, &
         & lvalue=lvalue, l64value=l64value, logvalue=logvalue, &
         & i8value=i8value, pbvalue=pbvalue)

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
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter1), &
         & C_NULL_PTR, row1)
    if (valid == FALSE) return
    ! And of the target location
    if (present(row2)) then
       valid = gtk_tree_model_iter_nth_child(store, c_loc(iter2), &
            & C_NULL_PTR, row2)
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
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter1), &
         & C_NULL_PTR, row1)
    if (valid == FALSE) return
    ! And of the second
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter2), &
         & C_NULL_PTR, row2)
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
  function hl_gtk_list1_new(scroll, width, changed, data, multiple, &
       & sensitive, tooltip, title, height) result(list)

    type(c_ptr) :: list
    type(c_ptr), intent(out), optional :: scroll
    integer(kind=c_int), intent(in), optional :: width
    type(c_funptr), intent(in), optional :: changed
    type(c_ptr), intent(in), optional :: data
    integer(kind=c_int), intent(in),  optional :: multiple, sensitive
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    character(len=*), intent(in), optional :: title
    integer(kind=c_int), intent(in), optional :: height

    ! A single column selectable list based on the GTK Tree View
    !
    ! SCROLL: c_ptr: optional: A scroll box that will contain the list
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

    types = (/ G_TYPE_STRING /)

    ! This slightly clunky if /else cascade is needed because the attempt to convert
    ! an unset scalar argument to an array causes a segfault.
    if (present(title) .and. present(width)) then
       list = hl_gtk_listn_new(scroll, ncols=1_c_int, types=types, &
            & changed=changed, &
            & data=data, multiple=multiple, sensitive=sensitive, &
            & tooltip=tooltip, width=(/width/), titles=(/title/), height=height)
    else if (present(title)) then
       list = hl_gtk_listn_new(scroll, ncols=1_c_int, types=types, &
            & changed=changed, &
            & data=data, multiple=multiple, sensitive=sensitive, &
            & tooltip=tooltip, titles=(/title/), height=height)
    else if (present(width)) then
       list = hl_gtk_listn_new(scroll, ncols=1_c_int, types=types, &
            & changed=changed, &
            & data=data, multiple=multiple, sensitive=sensitive, &
            & tooltip=tooltip, width=(/width/), height=height)
    else
       list = hl_gtk_listn_new(scroll, ncols=1_c_int, types=types, &
            & changed=changed, &
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

    call hl_gtk_listn_set_cell(list, irow, 0_c_int, svalue=text)

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

    call hl_gtk_listn_set_cell(list, row, 0_c_int, svalue=svalue)

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

    call hl_gtk_listn_get_cell(list, row, 0_c_int, svalue=svalue)

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

    call hl_gtk_listn_set_cell_data_func(list, 0_c_int, func, &
         & data, destroy_notify)
  end subroutine hl_gtk_list1_set_cell_data_func

  !+
  function hl_gtk_tree_new(scroll, ncols, types, changed, data, multiple,&
       & width, titles, height, swidth, align, ixpad, iypad, renderers, &
       & sensitive, tooltip, sortable, editable, colnos, edited, data_edited, &
       & edited_text, data_edited_text, toggled, data_toggled, &
       & edited_spin, data_edited_spin, &
       & edited_combo, data_edited_combo, changed_combo, data_changed_combo, &
       & toggled_radio, data_toggled_radio,  &
       & hscroll_policy, vscroll_policy) result(tree)

    type(c_ptr) :: tree
    type(c_ptr), intent(out), optional :: scroll
    integer(kind=c_int), intent(in), optional :: ncols
    integer(kind=type_kind), dimension(:), intent(in), optional :: types
    type(c_funptr), optional :: changed
    type(c_ptr), intent(in), optional :: data
    integer(kind=c_int), intent(in), optional :: multiple
    integer(kind=c_int), intent(in), optional, dimension(:) :: width
    character(len=*), dimension(:), intent(in), optional :: titles, renderers
    integer(kind=c_int), intent(in), optional :: height, swidth
    real(kind=c_float), intent(in), optional, dimension(:) :: align
    integer(kind=c_int), intent(in), optional, dimension(:) :: ixpad, iypad
    integer(kind=c_int), intent(in), optional :: sensitive
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), intent(in), optional, dimension(:) :: sortable, editable
    integer(kind=c_int), dimension(:), allocatable, intent(out), optional, target :: colnos
    type(c_funptr), optional :: edited, edited_text, toggled, edited_spin, &
         & edited_combo, changed_combo, toggled_radio
    type(c_ptr), optional, intent(in) :: data_edited, data_edited_text,&
         & data_toggled, data_edited_spin, data_edited_combo, &
         & data_changed_combo, data_toggled_radio
    integer(kind=c_int), intent(in), optional :: hscroll_policy, vscroll_policy

    ! Make a tree view
    !
    ! SCROLL: c_ptr: optional: A scrollable widget to contain the tree.
    ! 		(This is used to pack the tree)
    ! NCOLS: c_int: optional: The number of columns.
    ! TYPES: GType(): optional: The types for each column.
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
    ! RENDERERS: f_string(): List of renderer types.
    ! SENSITIVE: boolean: optional: Whether the widget is intially sensitive.
    ! TOOLTIP: string: optional: Tooltip for the widget
    ! SORTABLE: boolean(): optional: Set whether the tree can be sorted
    ! 		on that column.
    ! EDITABLE: boolean(): optional: Set whether the column can be edited.
    ! EDITED: f_funptr: optional: An alternative callback for the "edited"
    ! 		signal on edited cells. N.B. Only a single callback can be set
    ! 		if different actions are needed for different columns,
    ! 		you must use the column number inside the callback.
    ! DATA_EDITED: c_ptr: optional: Data to pass to the edited callback.
    ! EDITED_TEXT: c_funptr: optional: An alternative callback for
    ! 		text renderers (not applied to derived renderers).
    ! DATA_EDITED_TEXT: c_ptr: optional: Data to pass to the edited_text
    ! 		 callback.
    ! EDITED_SPIN: c_funptr: optional: An alternative callback for
    ! 		spin button.
    ! DATA_EDITED_SPIN: c_ptr: optional: Data to pass to the edited_spin
    ! 		 callback.
    ! EDITED_COMBO: c_funptr: optional: An alternative callback for
    ! 		the "edited" signal from a combo cell
    ! DATA_EDITED_COMBO: c_ptr: optional: Data to pass to the edited_combo
    ! 		 callback.
    ! TOGGLED: c_funptr: optional: An alternative callback for the "toggled"
    ! 		signal from toggle renderers.
    ! DATA_TOGGLED: c_ptr: optional: Data to pass to the toggled callback.
    ! TOGGLED_RADIO: c_funptr: optional: An alternative callback for the
    ! 		"toggled" signal from radio toggle renderers.
    ! DATA_TOGGLED_RADIO: c_ptr: optional: Data to pass to the toggled callback
    ! 		for radio toggle renderers
    ! CHANGED_COMBO: c_funptr: optional: Callback for the "changed" signal from
    ! 		a combo cell. This is not actually all that useful as the
    ! 		edited signal will be emitted as soon as focus leaves the combo.
    ! DATA_CHANGED_COMBO: c_ptr: optional: Data to pass to the changed callback.
    ! HSCROLL_POLICY: int: optional: Horizontal scrolling policy for the
    ! 		containing scroll window (default AUTOMATIC). 
    ! VSCROLL_POLICY: int: optional: Vertical scrolling policy for the
    ! 		containing scroll window (default AUTOMATIC). 
    !
    ! At least one of the array arguments or NCOLS must be given.
    ! If TYPES is not given, then strings are assumed.
    !
    ! For renderer types see HL_GTK_LISTN_NEW.
    !-

    integer(kind=c_int) :: ncols_all, i, hscroll, vscroll
    integer(kind=type_kind), dimension(:), allocatable, target :: types_all

    type(c_ptr) :: model, select

    ! Warn if the obsolete COLNOS argument is present
    if (present(colnos)) write(error_unit, *) "hl_gtk_tree_new: "//&
         & "The COLNOS argument is no longer needed."

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
    else if (present(renderers)) then
       ncols_all = size(renderers)
    else
       write(error_unit,*) &
            & "hl_gtk_tree_new: Cannot determine the number of columns"
       tree = C_NULL_PTR
       scroll=C_NULL_PTR
       return
    end if

    ! Now determine the column types.
    allocate(types_all(ncols_all))
    if (present(types)) then
       types_all = types
    else
       types_all(:) = G_TYPE_STRING
    end if
    if (present(renderers)) &
         & call hl_gtk_list_tree_type_adjust(types_all, renderers)


    ! Create the storage model
    model = gtk_tree_store_newv(ncols_all, c_loc(types_all))
    tree = gtk_tree_view_new_with_model(model)

    if (present(scroll)) then
       if (present(hscroll_policy)) then
          hscroll = hscroll_policy
       else
          hscroll = GTK_POLICY_AUTOMATIC
       end if
       if (present(vscroll_policy)) then
          vscroll = vscroll_policy
       else
          vscroll = GTK_POLICY_AUTOMATIC
       end if


       ! Pack the tree in the scroll box
       scroll = gtk_scrolled_window_new(C_NULL_PTR, C_NULL_PTR)
       call gtk_scrolled_window_set_policy(scroll, hscroll, vscroll)

       call gtk_container_add(scroll, tree)

       if (present(height) .and. present(swidth)) then
          call gtk_widget_set_size_request(scroll,swidth,height)
       else if (present(height)) then
          call gtk_widget_set_size_request(scroll,-1_c_int,height)
       else if (present(swidth)) then
          call gtk_widget_set_size_request(scroll,swidth,-1_c_int)
       end if
    else
       if (present(height) .and. present(swidth)) then
          call gtk_widget_set_size_request(tree,swidth,height)
       else if (present(height)) then
          call gtk_widget_set_size_request(tree,-1_c_int,height)
       else if (present(swidth)) then
          call gtk_widget_set_size_request(tree,swidth,-1_c_int)
       end if
    end if

    ! Set up the columns
    do i = 1, ncols_all
       call hl_gtk_list_tree_add_column(i, tree, .false., type=types_all(i), &
            & editable=editable, &
            & ixpad=ixpad, iypad=iypad, align=align, titles=titles, &
            & sortable=sortable, width=width, &
            & renderers=renderers, edited=edited, data_edited=data_edited, &
            & edited_text=edited_text, data_edited_text=data_edited_text, &
            & edited_spin=edited_spin, data_edited_spin=data_edited_spin, &
            & toggled=toggled, data_toggled=data_toggled, &
            & edited_combo=edited_combo, data_edited_combo=data_edited_combo, &
            & changed_combo=changed_combo, &
            & data_changed_combo=data_changed_combo, &
            & toggled_radio=toggled_radio, &
            & data_toggled_radio=data_toggled_radio)
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
  subroutine hl_gtk_tree_ins(tree, row, absrow, count)

    type(c_ptr), intent(in) :: tree
    integer(kind=c_int), intent(in), optional, dimension(:) :: row
    integer(kind=c_int), intent(in), optional :: absrow, count

    ! Insert a row into a tabular tree.
    !
    ! TREE: c_ptr: required: The tree into which to insert the row.
    ! ROW: c_int(): optional: The row BEFORE which to insert the row
    ! 		(append if an element is -1) For example; to put a new row
    ! 		after all other children of the second child of the fifth
    ! 		top-level row use (/ 4, 1, -1 /).
    ! ABSROW: c_int: optional: The row BEFORE which to insert the new row
    ! 		treating the tree as a flat list.
    ! COUNT: c_int: optional: How many rows to add (default 1)
    !-

    type(c_ptr) :: store
    type(gtktreeiter), target :: iter1, iter2
    integer(kind=c_int) :: valid
    integer :: i, ndep, n

    if (present(count)) then
       n = count
    else
       n = 1
    end if

    ! Get the TreeStore
    store = gtk_tree_view_get_model(tree)

    ! Insert the row (we don't use the "scanner" here because of the
    ! special case of -1 for append at a level).

    if (present(row)) then
       ndep = size(row)
       if (ndep == 1) then
          if (row(1) < 0) then
             do i = 1, n
                call gtk_tree_store_append(store, c_loc(iter1), C_NULL_PTR)
             end do
          else
             do i = 1, n
                call gtk_tree_store_insert(store, c_loc(iter1), C_NULL_PTR, &
                     & row(1))
             end do
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
                write(error_unit,*) "hl_gtk_tree_ins:: "//&
                     & "Row description does not point to an insertable location"
                return
             end if
             iter2 = iter1
          end do
          call clear_gtktreeiter(iter1)
          if (row(ndep) < 0) then
             do i = 1, n
                call gtk_tree_store_append(store, c_loc(iter1), c_loc(iter2))
             end do
          else
             do i = 1, n
                call gtk_tree_store_insert(store, c_loc(iter1), &
                     & c_loc(iter2), row(ndep))
             end do
          end if
       end if
    else if (present(absrow)) then
       if (absrow < 0) then
          do i = 1, n
             call gtk_tree_store_append(store, c_loc(iter1), C_NULL_PTR)
          end do
       else if (absrow == 0) then
          do i = 1, n
             call gtk_tree_store_prepend(store, c_loc(iter1), C_NULL_PTR)
          end do
       else
          valid = hl_gtk_tree_abs_iter(tree, iter1, absrow)
          if (valid == FALSE) then
             write(error_unit,*) "hl_gtk_tree_ins:: "//&
                  & "Row description does not point to an insertable location"
             return
          end if
          do i = 1, n
             call clear_gtktreeiter(iter2)
             call gtk_tree_store_insert_before(store, c_loc(iter2), &
                  & C_NULL_PTR, c_loc(iter1))
          end do
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
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), &
         & C_NULL_PTR, row(1))

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
    type(c_ptr), intent(in), optional :: selection

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
         & c_null_ptr)

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
            & gtk_tree_path_get_depth(g_list_nth_data(slist, i-1_c_int)))
    end do

    allocate(indices(maxdepth,count))
    if (present(depths)) allocate(depths(count))

    do i = 1, count
       idxlc = gtk_tree_path_get_indices_with_depth( &
            & g_list_nth_data(slist, i-1_c_int), &
            & c_loc(dep))
       call c_f_pointer(idxlc, idxl, (/ int(dep) /) )
       indices(:dep,i) = idxl
       if (present(depths)) depths(i) = dep
    end do

    ! Free the selection list.
    call g_list_foreach(slist, c_funloc(gtk_tree_path_free), C_NULL_PTR)
    call g_list_free(slist)

  end function hl_gtk_tree_get_selections

  !+
  subroutine hl_gtk_tree_set_cell(tree, row, col, absrow, &
       & svalue, fvalue, dvalue, ivalue, lvalue, l64value, &
       & logvalue, i8value, pbvalue)

    type(c_ptr), intent(in) :: tree
    integer(kind=c_int), intent(in), optional :: absrow, col
    integer(kind=c_int), intent(in), optional, dimension(:) :: row
    character(len=*), intent(in), optional :: svalue
    real(kind=c_float), intent(in), optional :: fvalue
    real(kind=c_double), intent(in), optional :: dvalue
    integer(kind=c_int), intent(in), optional :: ivalue
    integer(kind=c_long), intent(in), optional :: lvalue
    integer(kind=c_int64_t), intent(in), optional :: l64value
    logical, intent(in), optional :: logvalue
    integer(kind=c_int8_t), intent(in), optional :: i8value
    type(c_ptr), intent(in), optional :: pbvalue

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
    ! LOGVALUE: logical: optional: A logical value for the cell.
    ! I8VALUE: int8_t: optional: An 8-bit integer value for the cell.
    ! PBVALUE: c_ptr: optional: A pixbuf pointer value for the cell.
    !
    ! Note that reasonable conversions are made between types.
    !-

    integer(kind=type_kind) :: ctype
    type(c_ptr) :: store, val
    integer(kind=c_int) :: valid, icol
    type(gtktreeiter), target :: iter
    type(gvalue), target :: value

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

    call hl_gtk_list_tree_set_gvalue(val, ctype, svalue=svalue, fvalue=fvalue, &
         & dvalue=dvalue, ivalue=ivalue, lvalue=lvalue, l64value=l64value, &
         & logvalue=logvalue, i8value=i8value, pbvalue=pbvalue)

    call gtk_tree_store_set_value(store, c_loc(iter), icol, val)

  end subroutine hl_gtk_tree_set_cell

  !+
  subroutine hl_gtk_tree_get_cell(tree, row, col, absrow, &
       & svalue, fvalue, dvalue, ivalue, lvalue, l64value, logvalue, &
       & i8value, pbvalue)

    type(c_ptr), intent(in) :: tree
    integer(kind=c_int), intent(in), optional :: absrow, col
    integer(kind=c_int), intent(in), optional, dimension(:) :: row
    character(len=*), intent(out), optional :: svalue
    real(kind=c_float), intent(out), optional :: fvalue
    real(kind=c_double), intent(out), optional :: dvalue
    integer(kind=c_int), intent(out), optional :: ivalue
    integer(kind=c_long), intent(out), optional :: lvalue
    integer(kind=c_int64_t), intent(out), optional :: l64value
    logical, intent(out), optional :: logvalue
    integer(kind=c_int8_t), intent(out), optional :: i8value
    type(c_ptr), intent(out), optional :: pbvalue

    ! Retrieve the value of a cell.
    !
    ! TREE: c_ptr: required: The tree containing the cell.
    ! ROW: c_int(): optional: The row of the cell
    ! COL: c_int: optional: The column of the cell. (Only optional to
    ! 		allow format similar to the LISTs i.e. tree, row, column).
    ! ABSROW: c_int: optional: The row of the cell, treating the tree as
    ! 		a flat list.
    ! SVALUE: string: optional: A string value from the cell.
    ! FVALUE: float: optional: A single precision FP value from the cell.
    ! DVALUE: double: optional: A double precision FP value from the cell.
    ! IVALUE: c_int: optional: A normal integer value from the cell.
    ! LVALUE: c_long: optional: A long integer value from the cell.
    ! L64VALUE: c_int64_t: optional: A 64-bit integer value from the cell.
    ! LOGVALUE: logical : optional: A logical value from the cell.
    ! I8VALUE: int8_t: optional: An 8-bit integer value from the cell.
    ! PBVALUE: c_ptr: optional: A pixbuf pointer from the cell.
    !
    ! Note that a similar conversion system to the set_cell routine
    ! except that strings can only be returned to SVALUE.
    !-

    integer(kind=type_kind) :: ctype
    type(c_ptr) :: store, val
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
    call hl_gtk_list_tree_get_gvalue(val, ctype, &
         & svalue=svalue, fvalue=fvalue, dvalue=dvalue, ivalue=ivalue, &
         & lvalue=lvalue, l64value=l64value, logvalue=logvalue, &
         & i8value=i8value, pbvalue=pbvalue)

  end subroutine hl_gtk_tree_get_cell

  !+
  subroutine hl_gtk_listn_combo_set_select(view, row, colno, selection)
    type(c_ptr), intent(in) :: view
    integer(kind=c_int), intent(in) :: row, colno, selection

    ! Set the selected item in a combo cell renderer.
    !
    ! VIEW: c_ptr: required: The list view containing the cell.
    ! ROW: int: required: The row number of the cell
    ! COLNO: int: required: The column number with the cell
    ! SELECTION: int: required: The element of the combo to set.
    !-

    type(c_ptr) :: store, pstring, col, rlist, renderer, pmodel, model
    type(gvalue), target :: stringv, modelv
    type(gtktreeiter), target :: viter, citer
    integer(kind=c_int) :: valid

    ! Get list store
    store = gtk_tree_view_get_model(view)

    ! Get the iterator of the row
    call clear_gtktreeiter(viter)
    valid = gtk_tree_model_iter_nth_child(store, c_loc(viter), C_NULL_PTR, row)
    if (.not. c_f_logical(valid)) return
    
    ! Find the renderer for the column
    col = gtk_tree_view_get_column(view, colno)
    rlist = gtk_cell_layout_get_cells(col)
    renderer = g_list_nth_data(rlist, 0_c_int)
    call g_list_free(rlist)

    ! Find the model for the combobox
    pmodel = c_loc(modelv)
    pmodel = g_value_init(pmodel, gtk_tree_model_get_type())
    call g_object_get_property(renderer, "model"//c_null_char, pmodel)
    model = g_value_get_object(pmodel)
    
    call clear_gtktreeiter(citer)
    valid = gtk_tree_model_iter_nth_child(model, c_loc(citer), &
         & c_null_ptr, selection)
    if (c_f_logical(valid)) then
       pstring = c_loc(stringv)
       pstring = g_value_init(pstring, G_TYPE_STRING)
       call g_value_unset(pstring)
       call gtk_tree_model_get_value(model, c_loc(citer), 0_c_int, pstring)
       call gtk_list_store_set_value(store, c_loc(viter), colno, pstring)
    end if
  end subroutine hl_gtk_listn_combo_set_select
  !+
  subroutine hl_gtk_tree_combo_set_select(view, row, colno, absrow, selection)
    type(c_ptr), intent(in) :: view
    integer(kind=c_int), dimension(:), intent(in), optional :: row
    integer(kind=c_int), intent(in), optional ::  colno, absrow, selection

    ! Set the selected item in a combo cell renderer.
    !
    ! VIEW: c_ptr: required: The list view containing the cell.
    ! ROW: int(): optional: The row number of the cell
    ! COLNO: int: optional: The column number with the cell (Only optional to
    ! 		allow format similar to the LISTs).
    ! ABSROW: c_int: optional: The row, treating the tree as a flat list.
    ! SELECTION: int: required: The element of the combo to set.
    !-

    type(c_ptr) :: store, pstring, col, rlist, renderer, pmodel, model
    type(gvalue), target :: modelv
    type(gtktreeiter), target :: viter, citer
    integer(kind=c_int) :: valid

    ! Get list store
    store = gtk_tree_view_get_model(view)

    ! Get the iterator of the row
    if (present(row)) then
       valid = hl_gtk_tree_row_iter(C_NULL_PTR, viter, row, model=store)
    else if (present(absrow)) then
       valid = hl_gtk_tree_abs_iter(C_NULL_PTR, viter, absrow, model=store)
    else
       valid=FALSE
    end if
    if (.not. c_f_logical(valid)) return
    
    ! Find the renderer for the column
    col = gtk_tree_view_get_column(view, colno)
    rlist = gtk_cell_layout_get_cells(col)
    renderer = g_list_nth_data(rlist, 0_c_int)
    call g_list_free(rlist)

    ! Find the model for the combobox
    pmodel = c_loc(modelv)
    pmodel = g_value_init(pmodel, gtk_tree_model_get_type())
    call g_object_get_property(renderer, "model"//c_null_char, pmodel)
    model = g_value_get_object(pmodel)
    
    call clear_gtktreeiter(citer)
    valid = gtk_tree_model_iter_nth_child(model, c_loc(citer), &
         & c_null_ptr, selection)
    if (c_f_logical(valid)) then
       call g_value_unset(pstring)
       pstring = g_value_init(pstring, G_TYPE_STRING)
       call gtk_tree_model_get_value(model, c_loc(citer), 0_c_int, pstring)
       call gtk_list_store_set_value(store, c_loc(viter), colno, pstring)
    end if
  end subroutine hl_gtk_tree_combo_set_select

  ! ================================================================
  ! Default Callback routines for editable cells in lists & trees
  !=================================================================

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
    type(c_ptr) :: pcol, list

    call convert_c_string(path, fpath)
    read(fpath, *) irow
    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)
    call convert_c_string(text, ftext)
    list = g_object_get_data(renderer, "view"//c_null_char)
    call hl_gtk_listn_set_cell(list, irow, icol, &
         & svalue=trim(ftext))
  end subroutine hl_gtk_listn_edit_cb

  !+
  subroutine hl_gtk_listn_toggle_cb(renderer, path, gdata) bind(c)
    type(c_ptr), value :: renderer, path, gdata

    ! Default call back for a toggle button in a list
    !
    ! RENDERER: c_ptr: required: The renderer which sent the signal
    ! PATH: c_ptr: required: The path at which to insert
    ! GDATA: c_ptr: required: User data, Not used.
    !
    ! The column number is passed via the "column-number" gobject data value.
    ! The treeview containing the cell is passed via the "view" gobject
    ! data value.
    ! The row number is passed as a string in the PATH argument.
    ! This routine is not normally called by the application developer.
    !-
    character(len=200) :: fpath
    integer(kind=c_int) :: irow
    integer(kind=c_int), pointer :: icol
    type(c_ptr) :: pcol, list
    logical :: state

    call convert_c_string(path, fpath)
    read(fpath, *) irow

    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)

    list = g_object_get_data(renderer, "view"//c_null_char)
    state = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))
    call hl_gtk_listn_set_cell(list, irow, icol, &
         & logvalue= .not. state)
  end subroutine hl_gtk_listn_toggle_cb

  !+
  subroutine hl_gtk_listn_rtoggle_cb(renderer, path, gdata) bind(c)
    type(c_ptr), value :: renderer, path, gdata

    ! Default callback for a toggle button in a list
    !
    ! RENDERER: c_ptr: required: The renderer which sent the signal
    ! PATH: c_ptr: required: The path at which to insert
    ! GDATA: c_ptr: required: User data, Not used.
    !
    ! The column number is passed via the "column-number" gobject data value.
    ! The treeview containing the cell is passed via the "view" gobject
    ! data value.
    ! The row number is passed as a string in the PATH argument.
    ! This routine is not normally called by the application developer.
    !-
    character(len=200) :: fpath
    integer(kind=c_int) :: irow
    integer(kind=c_int), pointer :: icol
    integer(kind=c_int) :: i
    type(c_ptr) :: pcol, list
    logical :: state
    integer(kind=c_int) :: nrows

    call convert_c_string(path, fpath)
    read(fpath, *) irow

    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)

    list = g_object_get_data(renderer, "view"//c_null_char)
    state = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))
    if (state) return ! Don't act on an unset

    ! Find the first iterator
    nrows = gtk_tree_model_iter_n_children (gtk_tree_view_get_model(list), &
         & c_null_ptr)
    do i = 0,nrows-1
       call hl_gtk_listn_set_cell(list, i, icol, &
            & logvalue= i == irow)
    end do
  end subroutine hl_gtk_listn_rtoggle_cb

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
    integer :: i, n
    type(c_ptr) :: tree, pcol

    call convert_c_string(path, fpath)
    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)
    call convert_c_string(text, ftext)

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
  subroutine hl_gtk_tree_toggle_cb(renderer, path, gdata) bind(c)
    type(c_ptr), value :: renderer, path, gdata

    ! Default call back for a toggle button in a tree
    !
    ! RENDERER: c_ptr: required: The renderer which sent the signal
    ! PATH: c_ptr: required: The path at which to insert
    ! GDATA: c_ptr: required: User data, Not used.
    !
    ! The column number is passed via the "column-number" gobject data value.
    ! The treeview containing the cell is passed via the "view" gobject
    ! data value.
    ! The row number is passed as a string in the PATH argument.
    ! This routine is not normally called by the application developer.
    !-
    character(len=200) :: fpath
    integer(kind=c_int), allocatable, dimension(:) :: irow
    integer(kind=c_int), pointer :: icol
    integer :: i, n
    type(c_ptr) :: pcol, tree
    logical :: state

    call convert_c_string(path, fpath)
    n = 0
    do i = 1, len_trim(fpath)
       if (fpath(i:i) == ":") then
          n = n+1
          fpath(i:i) = ' '   ! : is not a separator for a Fortran read
       end if
    end do
    allocate(irow(n+1))
    read(fpath, *) irow

    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)

    tree = g_object_get_data(renderer, "view"//c_null_char)

    state = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))
    call hl_gtk_tree_set_cell(tree, irow, icol, &
         & logvalue= .not. state)
  end subroutine hl_gtk_tree_toggle_cb
  !+
  subroutine hl_gtk_tree_rtoggle_cb(renderer, path, gdata) bind(c)
    type(c_ptr), value :: renderer, path, gdata

    ! Default call back for a toggle button in a tree with the radio
    ! option set.
    !
    ! RENDERER: c_ptr: required: The renderer which sent the signal
    ! PATH: c_ptr: required: The path at which to insert
    ! GDATA: c_ptr: required: User data, Not used.
    !
    ! The column number is passed via the "column-number" gobject data value.
    ! The treeview containing the cell is passed via the "view" gobject
    ! data value.
    ! The row number is passed as a string in the PATH argument.
    ! This routine is not normally called by the application developer.
    ! The routine clears the settings on all buttons at the same level as
    ! the button that was pressed.
    !-
    character(len=200) :: fpath
    integer(kind=c_int), allocatable, dimension(:) :: irow, jrow
    integer(kind=c_int), pointer :: icol
    integer :: i, n
    type(c_ptr) :: pcol, tree, ipath, tree_model
    logical :: state
    type(gtktreeiter), target :: iter, piter
    integer(kind=c_int) :: valid

    call convert_c_string(path, fpath)
    n = 0
    do i = 1, len_trim(fpath)
       if (fpath(i:i) == ":") then
          n = n+1
          fpath(i:i) = ' '   ! : is not a separator for a Fortran read
       end if
    end do
    allocate(irow(n+1))
    read(fpath, *) irow

    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)

    tree = g_object_get_data(renderer, "view"//c_null_char)
    tree_model = gtk_tree_view_get_model(tree)

    state = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))
    if (state) return ! Don't act on a release of a selected toggle

    ! Clear all the siblings of the chosen 
    valid = gtk_tree_model_get_iter_from_string(tree_model, c_loc(iter), fpath)
    valid = gtk_tree_model_iter_parent (tree_model, c_loc(piter), c_loc(iter))
    call clear_gtktreeiter(iter)
    if (c_f_logical(valid)) then
       valid = gtk_tree_model_iter_children (tree_model, c_loc(iter),&
            & c_loc(piter))
    else
       valid = gtk_tree_model_iter_children (tree_model, c_loc(iter),&
            & c_null_ptr)
    end if

    do
       ipath = gtk_tree_model_get_string_from_iter (tree_model, c_loc(iter))
       call convert_c_string(ipath, fpath)
       n = 0
       do i = 1, len_trim(fpath)
          if (fpath(i:i) == ":") then
             n = n+1
             fpath(i:i) = ' '   ! : is not a separator for a Fortran read
          end if
       end do
       allocate(jrow(n+1))
       read(fpath, *) jrow
       call hl_gtk_tree_set_cell(tree, jrow, icol, logvalue=.false.)
       deallocate(jrow)
       valid = gtk_tree_model_iter_next (tree_model, c_loc(iter))
       if (.not. c_f_logical(valid)) exit
    end do
    call hl_gtk_tree_set_cell(tree, irow, icol, &
         & logvalue= .true.)
  end subroutine hl_gtk_tree_rtoggle_cb


  ! ================================================================
  ! The routines from here to the end are, private subroutines that
  ! are used to prevent excessive code duplication between lists
  ! and trees.
  ! ================================================================

  !+
  subroutine hl_gtk_list_tree_set_cell_data_func(list, colno, func, &
       & data, destroy_notify)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in) :: colno
    type(c_funptr), optional :: func
    type(c_ptr), optional :: data
    type(c_funptr), optional :: destroy_notify

    ! Add a custom rendering function to a column of a list or tree
    !
    ! LIST: c_ptr: required: The list to which to apply the rendering function
    ! COLNO: c_int: required: The column index to which to apply it.
    ! FUNC: c_funptr: optional: The function (actually subroutine)
    ! 		to do the rendering (see GtkTreeCellDataFunc, for
    ! 		details). Omit or set to C_NULL_FUNPTR to remove a function.
    ! DATA: c_ptr: optional: User data to pass to the function.
    ! DESTROY_NOTIFY: c_funptr: optional: A destroy notify subroutine.
    !
    ! This routine is always accessed by one of the interfaces,
    ! hl_gtk_tree_set_cell_data_func or hl_gtk_listn_set_cell_data_func.
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
    renderer = g_list_nth_data(rlist, 0_c_int)
    call g_list_free(rlist)

    call gtk_tree_view_column_set_cell_data_func(col, renderer,&
         & funpass, datapass, destpass)

  end subroutine hl_gtk_list_tree_set_cell_data_func

  !+
  subroutine hl_gtk_list_tree_add_column(icol, view, is_list, type, &
       & editable, ixpad, iypad, align, titles, sortable, width,&
       & renderers, edited, data_edited, edited_text, data_edited_text, &
       & toggled, data_toggled, edited_spin, data_edited_spin, edited_combo, &
       & data_edited_combo, changed_combo, data_changed_combo, toggled_radio, &
       & data_toggled_radio)
    integer(kind=c_int), intent(in) :: icol
    type(c_ptr), intent(in), target :: view
    logical, intent(in) :: is_list
    integer(kind=type_kind), intent(in) :: type
    integer(kind=c_int), dimension(:), optional, intent(in) :: editable, &
         & ixpad, iypad, sortable, width
    real(kind=c_float), intent(in), optional, dimension(:) :: align
    character(len=*), dimension(:), intent(in), optional :: titles, renderers
    type(c_funptr), optional :: edited, edited_text, toggled, edited_spin, &
         & edited_combo, changed_combo, toggled_radio
    type(c_ptr), optional, intent(in) :: data_edited, data_edited_text, &
         & data_toggled, data_edited_spin, data_edited_combo, &
         & data_changed_combo, data_toggled_radio

    ! Add a column to a list or tree view. (private)
    !
    ! ICOL: int: required: The column number
    ! VIEW: c_ptr: required: The list or tree view to modify
    ! IS_LIST: logical: required: .true. for a list, .false. for a tree.
    !
    ! The optional arguments are identical to the corresponding arguments in
    ! hl_gtk_listn_new and hl_gtk_tree_new.
    !
    ! This routine is not called by the application developer, but exists to
    ! reduce code duplication.
    !-

    type(c_ptr) :: renderer, column
    integer(kind=c_int), pointer :: coldat
    type(gvalue), target :: isedit, adjval, crate
    type(c_ptr) :: pisedit, padjval, pcrate
    integer(kind=c_int) :: nc
    character(len=20) :: render_id
    character(len=16) :: editable_property
    type(c_ptr) :: adjust

    if (present(renderers)) then
       render_id = renderers(icol)
    else
       render_id = hl_gtk_cell_text
    end if

    select case (render_id)
    case(hl_gtk_cell_text)
       renderer = gtk_cell_renderer_text_new()
       editable_property = "editable"//c_null_char

    case(hl_gtk_cell_toggle)
       renderer = gtk_cell_renderer_toggle_new()
       editable_property = "activatable"//c_null_char

    case(hl_gtk_cell_radio)
       renderer = gtk_cell_renderer_toggle_new()
       call gtk_cell_renderer_toggle_set_radio(renderer, TRUE)
       editable_property = "activatable"//c_null_char

    case(hl_gtk_cell_spin)
       renderer = gtk_cell_renderer_spin_new()
       ! We make the adjustment with default parameters, these can be
       ! adjusted with hl_gtk_listn_config_spin or hl_gtk_tree_config_spin.
       adjust = gtk_adjustment_new(0._c_double, 0._c_double, 1._c_double, &
            & 0.01_c_double, 0.1_c_double, 0._c_double)
       padjval = c_loc(adjval)
       padjval = g_value_init(padjval, G_TYPE_OBJECT)
       call g_value_take_object(padjval, adjust)
       call g_object_set_property(renderer, "adjustment"//c_null_char, &
            & padjval)
       editable_property = "editable"//c_null_char

       pcrate = c_loc(crate)
       pcrate = g_value_init(pcrate, G_TYPE_DOUBLE)
       call g_value_set_double(pcrate, 1.0_c_double)
       call g_object_set_property(renderer, "climb-rate"//c_null_char, &
            & pcrate)

    case(hl_gtk_cell_progress)
       renderer = gtk_cell_renderer_progress_new()
       editable_property = ""

    case(hl_gtk_cell_pixbuf)
       renderer = gtk_cell_renderer_pixbuf_new()
       editable_property = ""

    case(hl_gtk_cell_combo)
       renderer = gtk_cell_renderer_combo_new()
       call hl_gtk_list_tree_combo_model_attach(renderer)
       editable_property = "editable"//c_null_char

    case(hl_gtk_cell_spinner)
       write(error_unit, *) "hl_gtk_list_tree_add_column: "//&
            & "Renderer type ",trim(render_id)," not yet implemented"
       return

    case default
       write(error_unit, *) "hl_gtk_list_tree_add_column: "//&
            & "Renderer type ",trim(render_id)," is not valid."
       return
    end select

    if (present(ixpad) .and. present(iypad)) then
       call gtk_cell_renderer_set_padding(renderer, &
            & ixpad(icol), iypad(icol))
    else if (present(ixpad)) then
       call gtk_cell_renderer_set_padding(renderer, &
            & ixpad(icol), 0_c_int)
    else if (present(iypad)) then
       call gtk_cell_renderer_set_padding(renderer, &
            & 0_c_int, iypad(icol))
    end if

    if (present(editable) .and. editable_property /= '') then
       pisedit = c_loc(isedit)
       pisedit = g_value_init(pisedit, G_TYPE_BOOLEAN)
       call g_value_set_boolean(pisedit, editable(icol))
       call g_object_set_property(renderer, editable_property, pisedit)

       if (editable(icol) == TRUE) then
          allocate(coldat)
          coldat = icol-1
          call g_object_set_data(renderer, "column-number"//c_null_char, &
               & c_loc(coldat))

          call g_object_set_data(renderer, "view"//c_null_char, view)

          select case (render_id)
          case (hl_gtk_cell_text)
             if (present(edited_text)) then
                if (present(data_edited_text)) then
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited_text, data_edited_text)
                else
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited_text)
                end if
             else if (present(edited)) then
                if (present(data_edited)) then
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited, data_edited)
                else
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited)
                end if
             else if (is_list) then
                call g_signal_connect(renderer, "edited"//c_null_char, &
                     & c_funloc(hl_gtk_listn_edit_cb))
             else
                call g_signal_connect(renderer, "edited"//c_null_char, &
                     & c_funloc(hl_gtk_tree_edit_cb))
             endif

          case (hl_gtk_cell_toggle)
             if (present(toggled)) then
                if (present(data_toggled)) then
                   call g_signal_connect(renderer, "toggled"//c_null_char, &
                        & toggled, data_toggled)
                else
                   call g_signal_connect(renderer, "toggled"//c_null_char, &
                        & toggled)
                end if
             else if (is_list) then
                call g_signal_connect(renderer, "toggled"//c_null_char, &
                     & c_funloc(hl_gtk_listn_toggle_cb))
             else
                call g_signal_connect(renderer, "toggled"//c_null_char, &
                     & c_funloc(hl_gtk_tree_toggle_cb))
             end if

          case (hl_gtk_cell_radio)
             if (present(toggled_radio)) then
                if (present(data_toggled_radio)) then
                   call g_signal_connect(renderer, "toggled"//c_null_char, &
                        & toggled_radio, data_toggled_radio)
                else
                   call g_signal_connect(renderer, "toggled"//c_null_char, &
                        & toggled_radio)
                end if
             else if (is_list) then
                call g_signal_connect(renderer, "toggled"//c_null_char, &
                     & c_funloc(hl_gtk_listn_rtoggle_cb))
             else
                call g_signal_connect(renderer, "toggled"//c_null_char, &
                     & c_funloc(hl_gtk_tree_rtoggle_cb))
             end if

          case (hl_gtk_cell_spin)
             if (present(edited_spin)) then
                if (present(data_edited_spin)) then
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited_spin, data_edited_spin)
                else
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited_spin)
                end if
             else if (present(edited)) then
                if (present(data_edited)) then
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited, data_edited)
                else
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited)
                end if
             else if (is_list) then
                call g_signal_connect(renderer, "edited"//c_null_char, &
                     & c_funloc(hl_gtk_listn_edit_cb))
             else
                call g_signal_connect(renderer, "edited"//c_null_char, &
                     & c_funloc(hl_gtk_tree_edit_cb))
             endif

          case(hl_gtk_cell_combo)
             if (present(edited_combo)) then
                if (present(data_edited_combo)) then
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited_combo, data_edited_combo)
                else
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited_combo)
                end if
             else if (present(edited)) then
                if (present(data_edited)) then
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited, data_edited)
                else
                   call g_signal_connect(renderer, "edited"//c_null_char, &
                        & edited)
                end if
             else if (is_list) then
                call g_signal_connect(renderer, "edited"//c_null_char, &
                     & c_funloc(hl_gtk_listn_edit_cb))
             else
                call g_signal_connect(renderer, "edited"//c_null_char, &
                     & c_funloc(hl_gtk_tree_edit_cb))
             endif
             if (present(changed_combo)) then
                if (present(data_changed_combo)) then
                   call g_signal_connect(renderer, "changed"//c_null_char, &
                        & changed_combo, data_changed_combo)
                else
                   call g_signal_connect(renderer, "changed"//c_null_char, &
                        & changed_combo)
                end if
             end if

          case(hl_gtk_cell_pixbuf, hl_gtk_cell_progress, hl_gtk_cell_spinner)
             write(error_unit,*) "hl_gtk_list_tree_add_column: "//&
                  & "Column type ", trim(render_id), &
                  & "cannot be editable."

          case default
             write(error_unit,*) "Invalid renderer type"
          end select
       end if
    end if

    if (present(align)) then
       call gtk_cell_renderer_set_alignment(renderer, align(icol), 0.)
    else if (type == G_TYPE_STRING) then
       call gtk_cell_renderer_set_alignment(renderer, 0., 0.)
    else
       call gtk_cell_renderer_set_alignment(renderer, 1., 0.)
    end if

    column = gtk_tree_view_column_new()
    call gtk_tree_view_column_pack_start(column, renderer, FALSE)

    if (present(titles)) call gtk_tree_view_column_set_title(column, &
         &trim(titles(icol))//c_null_char)

    select case (render_id)
    case(hl_gtk_cell_text, hl_gtk_cell_spin)
       call gtk_tree_view_column_add_attribute(column, renderer, &
            & "text"//C_NULL_CHAR, icol-1_c_int)
    case(hl_gtk_cell_toggle, hl_gtk_cell_radio)
       call gtk_tree_view_column_add_attribute(column, renderer, &
            & "active"//C_NULL_CHAR, icol-1_c_int)
    case(hl_gtk_cell_progress)
       call gtk_tree_view_column_add_attribute(column, renderer, &
            & "value"//c_null_char, icol-1_c_int)
       call gtk_tree_view_column_add_attribute(column, renderer, &
            & "text"//C_NULL_CHAR, icol-1_c_int)
    case(hl_gtk_cell_pixbuf)
       call gtk_tree_view_column_add_attribute(column, renderer, &
            & "pixbuf"//c_null_char, icol-1_c_int)
    case(hl_gtk_cell_combo)
       call gtk_tree_view_column_add_attribute(column, renderer, &
            & "text"//c_null_char, icol-1_c_int)
    end select

    nc = gtk_tree_view_append_column(view, column)
    if (present(sortable)) then
       if (sortable(icol) == TRUE) then
          call gtk_tree_view_column_set_sort_column_id(column, icol-1_c_int)
          call gtk_tree_view_column_set_sort_indicator(column, TRUE)
       end if
    end if
    if (present(width)) then
       if (width(icol) > 0) then
          call gtk_tree_view_column_set_sizing (column, &
               & GTK_TREE_VIEW_COLUMN_FIXED)
          call gtk_tree_view_column_set_fixed_width(column, width(icol))
          call gtk_cell_renderer_set_fixed_size(renderer, width(icol), &
               & -1_c_int)
       end if
    end if
    call gtk_tree_view_column_set_resizable(column,TRUE)

  end subroutine hl_gtk_list_tree_add_column

  !+
  subroutine hl_gtk_list_tree_set_gvalue(val, ctype, svalue, fvalue, dvalue, &
       & ivalue, lvalue, l64value, logvalue, i8value, pbvalue)
    type(c_ptr), intent(inout) :: val
    integer(kind=type_kind) :: ctype
    character(len=*), intent(in), optional :: svalue
    real(kind=c_float), intent(in), optional :: fvalue
    real(kind=c_double), intent(in), optional :: dvalue
    integer(kind=c_int), intent(in), optional :: ivalue
    integer(kind=c_long), intent(in), optional :: lvalue
    integer(kind=c_int64_t), intent(in), optional :: l64value
    logical, intent(in), optional :: logvalue
    integer(kind=c_int8_t), intent(in), optional :: i8value
    type(c_ptr), intent(in), optional :: pbvalue

    ! Set a gvalue to the appropriate type and value to set a list or
    ! tree cell. (private)
    !
    ! VAL: c_ptr: required: The C address of the gvalue.
    ! CTYPE: int(kind_type) : required: The type of the cell.
    !
    ! The optional arguments are the same as for the set_cell routines.
    !-

    character(len=120) :: sconv
    integer(kind=c_int) :: iconv
    integer(kind=c_long) :: lconv
    integer(kind=c_int64_t) :: l64conv
    real(kind=c_float) :: fconv
    real(kind=c_double) :: dconv
    integer :: ios

    val = g_value_init(val, ctype)

    ! Select according to the cell type
    select case(ctype)
    case(G_TYPE_CHAR)
       if (present(svalue)) then
!!$GLIB< 2.32!          call g_value_set_char(val, int(ichar(svalue(1:1)), c_int8_t))
!!$GLIB>=2.32!          call g_value_set_schar(val, int(ichar(svalue(1:1)), c_int8_t))
       else if (present(i8value)) then
!!$GLIB< 2.32!          call g_value_set_char(val, i8value)
!!$GLIB>=2.32!          call g_value_set_schar(val, i8value)
       else if (present(ivalue)) then
!!$GLIB< 2.32!          call g_value_set_char(val, int(ivalue, c_int8_t))
!!$GLIB>=2.32!          call g_value_set_schar(val, int(ivalue, c_int8_t))
       else if (present(lvalue)) then
!!$GLIB< 2.32!          call g_value_set_char(val, int(lvalue, c_int8_t))
!!$GLIB>=2.32!          call g_value_set_schar(val, int(lvalue, c_int8_t))
       else if (present(l64value)) then
!!$GLIB< 2.32!          call g_value_set_char(val, int(l64value, c_int8_t))
!!$GLIB>=2.32!          call g_value_set_schar(val, int(l64value, c_int8_t))
       else
          write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
               & "Cannot make a 'char' type from given value(s)"
          return
       end if
    case(G_TYPE_UCHAR)
       if (present(svalue)) then
          call g_value_set_uchar(val, int(ichar(svalue(1:1)), c_int8_t))
       else if (present(i8value)) then
          call g_value_set_uchar(val, int(i8value, c_int8_t))
       else if (present(ivalue)) then
          call g_value_set_uchar(val, int(ivalue, c_int8_t))
       else if (present(lvalue)) then
          call g_value_set_uchar(val, int(lvalue, c_int8_t))
       else if (present(l64value)) then
          call g_value_set_uchar(val, int(l64value, c_int8_t))
       else
          write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
               & "Cannot make a 'uchar' type from given value(s)"
          return
       end if

    case (G_TYPE_INT)
       if (present(ivalue)) then
          call g_value_set_int(val, ivalue)
       else if (present(lvalue)) then
          call g_value_set_int(val, int(lvalue, c_int))
       else if (present(l64value)) then
          call g_value_set_int(val, int(l64value, c_int))
       else if (present(i8value)) then
          call g_value_set_int(val, int(i8value, c_int))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) iconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "// &
                  & "Failed to convert string to 'int'"
             return
          end if
          call g_value_set_int(val, iconv)
       else
          write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "// &
               & "Cannot make an 'int' type from given value(s)"
          return
       end if
    case (G_TYPE_UINT)
       if (present(ivalue)) then
          call g_value_set_uint(val, ivalue)
       else if (present(lvalue)) then
          call g_value_set_uint(val, int(lvalue, c_int))
       else if (present(l64value)) then
          call g_value_set_uint(val, int(l64value, c_int))
       else if (present(i8value)) then
          call g_value_set_uint(val, int(i8value, c_int))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) iconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
                  & "Failed to convert string to 'uint'"
             return
          end if
          call g_value_set_uint(val, iconv)
       else
          write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "// &
               & "Cannot make an 'uint' type from given value(s)"
          return
       end if
    case (G_TYPE_BOOLEAN)
       if (present(logvalue)) then
          call g_value_set_boolean(val, f_c_logical(logvalue))
       else if (present(i8value)) then
          call g_value_set_boolean(val, int(i8value, c_int))
       else if (present(ivalue)) then
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
                write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
                     & "Failed to convert string to 'int'"
                return
             end if
             call g_value_set_boolean(val, iconv)
          end if
       else
          write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "// &
               & "Cannot make an 'int' type from given value(s)"
          return
       end if

    case (G_TYPE_LONG)
       if (present(lvalue)) then
          call g_value_set_long(val, lvalue)
       else if (present(l64value)) then
          call g_value_set_long(val, int(l64value, c_long))
       else if (present(ivalue)) then
          call g_value_set_long(val, int(ivalue, c_long))
       else if (present(i8value)) then
          call g_value_set_long(val, int(i8value, c_long))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) lconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
                  & "Failed to convert string to 'long'"
             return
          end if
          call g_value_set_long(val, lconv)
       else
          write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
               & "Cannot make a 'long' type from given value(s)"
          return
       end if
    case (G_TYPE_ULONG)
       if (present(lvalue)) then
          call g_value_set_ulong(val, lvalue)
       else if (present(l64value)) then
          call g_value_set_ulong(val, int(l64value, c_long))
       else if (present(ivalue)) then
          call g_value_set_ulong(val, int(ivalue, c_long))
       else if (present(i8value)) then
          call g_value_set_ulong(val, int(i8value, c_long))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) lconv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
                  & "Failed to convert string to 'long'"
             return
          end if
          call g_value_set_ulong(val, lconv)
       else
          write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
               & "Cannot make a 'ulong' type from given value(s)"
          return
       end if

    case (G_TYPE_INT64)
       if (present(l64value)) then
          call g_value_set_int64(val, l64value)
       else if (present(lvalue)) then
          call g_value_set_int64(val, int(lvalue, c_int64_t))
       else if (present(ivalue)) then
          call g_value_set_int64(val, int(ivalue, c_int64_t))
       else if (present(i8value)) then
          call g_value_set_int64(val, int(i8value, c_int64_t))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) l64conv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
                  & "Failed to convert string to 'int64'"
             return
          end if
          call g_value_set_int64(val, l64conv)
       else
          write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
               & "Cannot make an 'int64' type from given value(s)"
          return
       end if
    case (G_TYPE_UINT64)
       if (present(l64value)) then
          call g_value_set_uint64(val, l64value)
       else if (present(lvalue)) then
          call g_value_set_uint64(val, int(lvalue, c_int64_t))
       else if (present(ivalue)) then
          call g_value_set_uint64(val, int(ivalue, c_int64_t))
       else if (present(i8value)) then
          call g_value_set_uint64(val, int(i8value, c_int64_t))
       else if (present(svalue)) then
          read(svalue,*,iostat=ios) l64conv
          if (ios /= 0) then
             write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
                  & "Failed to convert string to 'int64'"
             return
          end if
          call g_value_set_uint64(val, l64conv)
       else
          write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
               & "Cannot make an 'int64' type from given value(s)"
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
             write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
                  & "Failed to convert string to 'float'"
             return
          end if
          call g_value_set_float(val, fconv)
       else
          write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
               & "Cannot make a 'float' type from given value(s)"
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
             write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
                  & "Failed to convert string to 'double'"
             return
          end if
          call g_value_set_double(val, dconv)
       else
          write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
               & "Cannot make a 'double' type from given value(s)"
          return
       end if

    case (G_TYPE_STRING)
       if (present(svalue)) then
          call g_value_set_string(val, trim(svalue)//c_null_char)
       else
          if (present(ivalue)) then
             write(sconv,*) ivalue
          else if (present(i8value)) then
             write(sconv,*) i8value
          else if (present(lvalue)) then
             write(sconv,*) lvalue
          else if (present(l64value)) then
             write(sconv,*) l64value
          else if (present(fvalue)) then
             write(sconv,*) fvalue
          else if (present(dvalue)) then
             write(sconv,*) dvalue
          else if (present(logvalue)) then
             write(sconv,*) logvalue
          else
             write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
                  & "Cannot make a 'string' type from given value(s)"
             return
          end if
          call g_value_set_string(val, trim(sconv)//c_null_char)
       end if


    case default
       if  (ctype == gdk_pixbuf_get_type()) then
          if (present(pbvalue)) then
             call g_value_set_object(val, pbvalue)
          else
             write(error_unit,*) "hl_gtk_list_tree_set_gvalue:: "//&
                  & "Cannot make a 'pixbuf' type from given value(s)"
             return
          end if
       else
          write(error_unit,*)  "hl_gtk_list_tree_set_gvalue:: "//&
               & "Cell type ",ctype," is unknown"
          return
       end if
    end select
  end subroutine hl_gtk_list_tree_set_gvalue

  !+
  subroutine hl_gtk_list_tree_get_gvalue(val, ctype, &
       & svalue, fvalue, dvalue, ivalue, lvalue, l64value, logvalue, &
       & i8value, pbvalue)
    type(c_ptr), intent(in) :: val
    integer(kind=type_kind), intent(in) :: ctype
    character(len=*), intent(out), optional :: svalue
    real(kind=c_float), intent(out), optional :: fvalue
    real(kind=c_double), intent(out), optional :: dvalue
    integer(kind=c_int), intent(out), optional :: ivalue
    integer(kind=c_long), intent(out), optional :: lvalue
    integer(kind=c_int64_t), intent(out), optional :: l64value
    logical, intent(out), optional :: logvalue
    integer(kind=c_int8_t), intent(out), optional :: i8value
    type(c_ptr), intent(out), optional :: pbvalue

    ! Get the contents of a Gvalue (private)
    !
    ! VAL: c_ptr: required: The GValue
    ! CTYPE: int(type_kind): required: The type of value it contains.
    !
    ! The output arguments are identical to the get_cell routines.
    !-

    type(c_ptr) ::  cstr

    select case(ctype)
    case(G_TYPE_CHAR)
       if (present(i8value)) then
!!$GLIB< 2.32!          ivalue = g_value_get_char(val)
!!$GLIB>=2.32!          ivalue = g_value_get_schar(val)
       else if (present(svalue)) then
!!$GLIB< 2.32!          svalue(1:1) = char(g_value_get_char(val))
!!$GLIB>=2.32!          svalue(1:1) = char(g_value_get_schar(val))
       else if (present(ivalue)) then
!!$GLIB< 2.32!          ivalue = int(g_value_get_char(val), c_int)
!!$GLIB>=2.32!          ivalue = int(g_value_get_schar(val), c_int)
       else if (present(lvalue)) then
!!$GLIB< 2.32!          ivalue = int(g_value_get_char(val), c_long)
!!$GLIB>=2.32!          lvalue = int(g_value_get_schar(val), c_long)
       else if (present(l64value)) then
!!$GLIB< 2.32!          l64value = int(g_value_get_char(val), c_int64_t)
!!$GLIB>=2.32!          l64value = int(g_value_get_schar(val), c_int64_t)
       else
          write(error_unit,*) "hl_gtk_list_tree_get_gvalue:: "//&
               & "Cannot return 'char' type to any available output"
          return
       end if
    case(G_TYPE_UCHAR)
       if (present(i8value)) then
          i8value = g_value_get_uchar(val)
       else if (present(svalue)) then
          svalue(1:1)= char(g_value_get_uchar(val))
       else if (present(ivalue)) then
          ivalue = int(g_value_get_uchar(val), c_int)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_uchar(val), c_long)
       else if (present(l64value)) then
          l64value = int(g_value_get_uchar(val), c_int64_t)
       else
          write(error_unit,*) "hl_gtk_list_tree_get_gvalue:: "// &
               & "Cannot return 'uchar' type to any available output"
          return
       end if

    case (G_TYPE_INT)
       if (present(ivalue)) then
          ivalue = g_value_get_int(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_int(val), c_long)
       else if (present(l64value)) then
          l64value = int(g_value_get_int(val), c_int64_t)
       else if (present(i8value)) then
          i8value = int(g_value_get_int(val), c_int8_t)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_int(val)
       else
          write(error_unit,*) "hl_gtk_list_tree_get_gvalue:: "//&
               & "Cannot return 'int' type to any available output"
          return
       end if
    case (G_TYPE_UINT)
       if (present(ivalue)) then
          ivalue = g_value_get_uint(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_uint(val), c_long)
       else if (present(l64value)) then
          l64value = int(g_value_get_uint(val), c_int64_t)
       else if (present(i8value)) then
          i8value = int(g_value_get_uint(val), c_int8_t)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_uint(val)
       else
          write(error_unit,*) "hl_gtk_list_tree_get_gvalue:: "//&
               & "Cannot return 'int' type to any available output"
          return
       end if
    case (G_TYPE_BOOLEAN)
       if (present(logvalue)) then
          logvalue = c_f_logical(g_value_get_boolean(val))
       else if (present(i8value)) then
          i8value = int(g_value_get_boolean(val), c_int8_t)
       else if (present(ivalue)) then
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
          write(error_unit,*) "hl_gtk_list_tree_get_gvalue:: "//&
               & "Cannot return 'bool' type to any available output"
          return
       end if

    case (G_TYPE_LONG)
       if (present(lvalue)) then
          lvalue = g_value_get_long(val)
       else if (present(l64value)) then
          l64value = int(g_value_get_long(val), c_int64_t)
       else if (present(ivalue)) then
          ivalue = int(g_value_get_long(val), c_int)
       else if (present(i8value)) then
          i8value = int(g_value_get_long(val), c_int8_t)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_long(val)
       else
          write(error_unit,*) "hl_gtk_list_tree_get_gvalue:: "//&
               & "Cannot return 'long' type to any available output"
          return
       end if
    case (G_TYPE_ULONG)
       if (present(lvalue)) then
          lvalue = g_value_get_ulong(val)
       else if (present(l64value)) then
          l64value = int(g_value_get_ulong(val), c_int64_t)
       else if (present(ivalue)) then
          ivalue = int(g_value_get_ulong(val), c_int)
       else if (present(i8value)) then
          i8value = int(g_value_get_ulong(val), c_int8_t)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_ulong(val)
       else
          write(error_unit,*) "hl_gtk_list_tree_get_gvalue:: "//&
               & "Cannot return 'long' type to any available output"
          return
       end if

    case (G_TYPE_INT64)
       if (present(l64value)) then
          l64value = g_value_get_int64(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_int64(val), c_long)
       else if (present(ivalue)) then
          ivalue = int(g_value_get_int64(val), c_int)
       else if (present(i8value)) then
          i8value = int(g_value_get_int64(val), c_int8_t)
       else if (present(svalue)) then
          write (svalue,*) g_value_get_int64(val)
       else
          write(error_unit,*) "hl_gtk_list_tree_get_gvalue:: "//&
               & "Cannot return 'int64' type to any available output"
          return
       end if
    case (G_TYPE_UINT64)
       if (present(l64value)) then
          l64value = g_value_get_uint64(val)
       else if (present(lvalue)) then
          lvalue = int(g_value_get_uint64(val), c_long)
       else if (present(ivalue)) then
          ivalue = int(g_value_get_uint64(val), c_int)
       else if (present(i8value)) then
          i8value = int(g_value_get_uint64(val), c_int8_t)
       else if (present(svalue)) then
          write(svalue,*) g_value_get_uint64(val)
       else
          write(error_unit,*) "hl_gtk_list_tree_get_gvalue:: "//&
               & "Cannot return 'int64' type to any available output"
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
          write(error_unit,*) "hl_gtk_list_tree_get_gvalue:: "//&
               & "Cannot return 'float' type to any available output"
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
          write(error_unit,*) "hl_gtk_list_tree_get_gvalue:: "//&
               & "Cannot return 'double' type to any available output"
          return
       end if

    case (G_TYPE_STRING)
       if (present(svalue)) then
          cstr = g_value_get_string(val)
          call convert_c_string(cstr, svalue)
       else
          write(error_unit,*) "hl_gtk_list_tree_get_gvalue:: "//&
               & "Cannot return 'string' type to any available output"
       end if

    case default
       if (ctype == gdk_pixbuf_get_type()) then
          if (present(pbvalue)) then
             pbvalue = g_value_get_object(val)
          else
             write(error_unit,*) "hl_gtk_list_tree_get_gvalue:: "//&
                  & "Cannot return 'object' type to any available output"
          end if
       else
          write(error_unit,*)  "hl_gtk_list_tree_get_gvalue:: "//&
               & "Cell type ",ctype," is unknown"
          return
       end if
    end select
  end subroutine hl_gtk_list_tree_get_gvalue

  !+
  subroutine hl_gtk_list_tree_type_adjust(types, renderers)
    integer(kind=type_kind), dimension(:), intent(inout) :: types
    character(len=*), dimension(:), intent(in) :: renderers

    ! Ensure that the types are appropriate to the renderers.
    !
    ! TYPES: type_kind: required: The list of types (updated as needed)
    ! RENDERERS: f_string(): required: The list of renderers.
    !-

    integer :: i, n

    n = size(types)

    do i = 1, n
       select case(renderers(i))
       case(hl_gtk_cell_text)
          if (types(i) == G_TYPE_OBJECT .or. &
               & types(i) == gdk_pixbuf_get_type()) types(i) = G_TYPE_STRING
       case(hl_gtk_cell_toggle, hl_gtk_cell_radio)
          types(i) = G_TYPE_BOOLEAN
       case(hl_gtk_cell_pixbuf)
          types(i) = gdk_pixbuf_get_type()
       case(hl_gtk_cell_spin)
          types(i) = G_TYPE_DOUBLE
       case(hl_gtk_cell_progress)
          types(i) = G_TYPE_INT
       end select
    end do
  end subroutine hl_gtk_list_tree_type_adjust

  !+
  subroutine hl_gtk_list_tree_config_spin(view, colno, vmin, vmax, &
       & step, bigstep, value, digits, rate)
    type(c_ptr), intent(in) :: view
    integer(kind=c_int), intent(in) :: colno
    real(kind=c_double), intent(in), optional :: vmin, vmax, step, &
         & bigstep, value
    integer(kind=c_int), intent(in), optional :: digits
    real(kind=c_double), intent(in), optional :: rate

    ! Set up a spin button in a list or tree.
    !
    ! VIEW: c_ptr: required: The treeview containing the spin button.
    ! COLNO: int: required: The column with the spin button.
    ! MIN: double: optional: The minimum value for the spin button
    ! MAX: double: optional: The maximum value for the spin button.
    ! STEP: double: optional: The step size for the spin button.
    ! BIGSTEP: double: optional: The "jump" size for the spin button (not
    ! 		actually very useful).
    ! VALUE: double: optional: The value for the spin button.
    ! DIGITS: int: optional: How many digits to show.
    ! RATE: double: optional: The climb rate.
    !
    ! This routine is always called through the interfaces,
    ! hl_gtk_listn_config_spin or  hl_gtk_tree_config_spin
    !-

    type(c_ptr) :: col, rlist, renderer, adjust, padjv
    type(gvalue), target :: adjv

    padjv=c_loc(adjv)

    ! Extract the renderer
    col = gtk_tree_view_get_column(view, colno)
    rlist = gtk_cell_layout_get_cells(col)
    renderer = g_list_nth_data(rlist, 0_c_int)
    call g_list_free(rlist)

    ! Get the adjustment from the renderer.
    padjv = g_value_init(padjv, G_TYPE_OBJECT)
    call g_object_get_property(renderer, "adjustment"//c_null_char, &
         & padjv)
    adjust = g_value_get_object(padjv)
    adjust = g_object_ref(adjust)
    if (present(vmin)) call gtk_adjustment_set_lower(adjust, vmin)
    if (present(vmax)) call gtk_adjustment_set_upper(adjust, vmax)
    if (present(step)) call gtk_adjustment_set_step_increment(adjust, step)
    if (present(bigstep)) &
         & call gtk_adjustment_set_page_increment(adjust, bigstep)
    if (present(value)) call gtk_adjustment_set_value(adjust, value)

    call g_value_unset(padjv)

    if (present(digits)) then 
       padjv = g_value_init(padjv, G_TYPE_UINT)
       call g_value_set_uint(padjv, digits)
       call g_object_set_property(renderer, "digits"//c_null_char, &
            & padjv)
       call g_value_unset(padjv)
    end if
    if (present(rate)) then 
       padjv = g_value_init(padjv, G_TYPE_DOUBLE)
       call g_value_set_double(padjv, rate)
       call g_object_set_property(renderer, "climb-rate"//c_null_char, &
            & padjv)
       call g_value_unset(padjv)
    end if
  end subroutine hl_gtk_list_tree_config_spin

  !+
  subroutine hl_gtk_list_tree_combo_model_attach(renderer, cmodel, colno)
    type(c_ptr), intent(in) :: renderer
    type(c_ptr), intent(in), optional :: cmodel
    integer(kind=c_int), optional, intent(in) :: colno

    ! Create a tree model suitable for a GtkCellRendererCombo and attach
    ! it to the renderer.
    !
    ! RENDERER: c_ptr: required: The renderer to which the model
    ! 		will be attached.
    ! CMODEL: c_ptr: optional: A custom model for the combobox
    ! COLNO: int: optional: Which column of the custom model contains
    ! 		the text.
    ! 
    ! This routine is automatically called by the list/tree constructor if needed.
    ! To explicitly set a different model, use one of its aliases
    ! hl_gtk_listn_combo_set_model or hl_gtk_tree_combo_set_model.
    ! The default model is a 1-column list with the strings (indices can
    ! be obtained from the PATH argument in the edited hander.
    !-

    integer(kind=c_int), parameter :: ncols=1
    integer(kind=type_kind), dimension(ncols), target :: coltypes = &
         & [G_TYPE_STRING]
    type(gvalue), target :: modelv, columnv
    type(c_ptr) :: pmodel, pcolumn, model
    integer(kind=c_int) :: icol

    ! Create the model
    if (present(cmodel)) then
       model = cmodel
       if (present(colno)) then
          icol = colno
       else
          icol = 0
       end if
    else
       model = gtk_list_store_newv(ncols, c_loc(coltypes))
       icol = 0
    end if

    ! Attach it to the renderer.
    pmodel = c_loc(modelv)
    pmodel = g_value_init(pmodel, gtk_tree_model_get_type())
    call g_value_set_object(pmodel, model)
    call g_object_set_property(renderer, "model"//c_null_char, pmodel)

    ! Tell the renderer that the text is in column 0
    pcolumn = c_loc(columnv)
    pcolumn = g_value_init(pcolumn, G_TYPE_INT)
    call g_value_set_int(pcolumn, icol)
    call g_object_set_property(renderer, "text-column"//c_null_char, &
         & pcolumn)
  end subroutine hl_gtk_list_tree_combo_model_attach

  !+
  subroutine  hl_gtk_list_tree_combo_model_config(view, colno, vals, &
       & append, has_entry)
    type(c_ptr), intent(in) :: view
    integer(kind=c_int), intent(in) :: colno
    character(len=*), intent(in), dimension(:), optional :: vals
    integer(kind=c_int), intent(in), optional :: append, has_entry

    ! Set the strings in the combo model.
    !
    ! VIEW: c_ptr: required: The tree view that contains the combo
    ! 		cell renderer.
    ! COLNO: c_int: required: The column number of the combo renderer.
    ! VALS: f_string(): required: The strings for the combo box.
    ! APPEND: c_int: optional: Set to TRUE to add the strings to those
    ! 		already present, otherwise the strings are replaced.
    ! HAS_ENTRY: c_int: optional: Set to TRUE to add an entry field to
    ! 		the combo. Set to FALSE to remove an entry field.
    !
    ! This routine is always accessed by its aliases hl_gtk_tree_config_combo
    ! or hl_gtk_listn_config_combo
    !-

    type(c_ptr) :: col, rlist, renderer
    type(gvalue), target :: modelv, stringv, entryv
    type(c_ptr) :: pmodel, model, pstring, pentry
    logical :: iappend
    integer(kind=c_int) :: nvals, i, valid
    type(gtktreeiter), target :: iter

    if (present(append)) then
       iappend = c_f_logical(append)
    else
       iappend = .false.
    end if

    ! Find the renderer for the column
    col = gtk_tree_view_get_column(view, colno)
    rlist = gtk_cell_layout_get_cells(col)
    renderer = g_list_nth_data(rlist, 0_c_int)
    call g_list_free(rlist)

    ! Find the model for the combobox
    pmodel = c_loc(modelv)
    pmodel = g_value_init(pmodel, gtk_tree_model_get_type())
    call g_object_get_property(renderer, "model"//c_null_char, pmodel)
    model = g_value_get_object(pmodel)

    pstring = c_loc(stringv)
    pstring = g_value_init(pstring, G_TYPE_STRING)

    if (present(vals)) then
       if (iappend) then
          nvals = gtk_tree_model_iter_n_children(model, c_null_ptr)
          valid = gtk_tree_model_iter_nth_child(model, c_loc(iter), &
               & c_null_ptr, nvals-1_c_int)
          do i = 1, size(vals)
             call clear_gtktreeiter(iter)
             call gtk_list_store_append(model, c_loc(iter))
             call g_value_set_string(pstring, trim(vals(i))//c_null_char)
             call gtk_list_store_set_value(model, c_loc(iter), 0_c_int, pstring)
          end do
       else
          call gtk_list_store_clear(model)
          do i = 1, size(vals)
             call clear_gtktreeiter(iter)
             call gtk_list_store_append(model, c_loc(iter))
             call g_value_set_string(pstring, trim(vals(i))//c_null_char)
             call gtk_list_store_set_value(model, c_loc(iter), 0_c_int, pstring)
          end do
       end if
    end if

    if (present(has_entry)) then
       pentry = c_loc(entryv)
       pentry = g_value_init(pentry, G_TYPE_BOOLEAN)
       call g_value_set_boolean(pentry, has_entry)
       call g_object_set_property(renderer, "has-entry"//c_null_char, pentry)
    end if

  end subroutine hl_gtk_list_tree_combo_model_config
end module gtk_hl_tree
