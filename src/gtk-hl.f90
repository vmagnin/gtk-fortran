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
! Last modification: 03-30-2011

module gtk_hl
  ! A bunch of procedures to implement higher level creators for
  ! the gtk-fortran widgets. Some settings and operations are also
  ! provided for the more intricate widgets.
  !
  ! To date this is very incomplete.
  !
  ! Many ideas in this module were taken from the pilib gtk<->fortran
  ! interface.

  ! Currently included:
  ! hl_gtk_window_new: A top-level window.
  ! hl_gtk_button_new: A simple button
  ! hl_gtk_check_button_new: A check button
  ! hl_gtk_radio_button_new: A radio button with group.
  !     hl_gtk_radio_group_get_select: Which member of a radio group
  !     	is selected.
  !     hl_gtk_radio_group_set_select: Select a member of a radio group.
  ! hl_gtk_entry_new: A 1-line text box
  ! hl_gtk_list1_new: A single column list with indexing
  !     hl_gtk_list1_get_selections: Get the selected row(s) from a list.
  !     hl_gtk_list1_ins: Insert a row into a list
  !     hl_gtk_list1_rem: Delete a row from a list, or clear the list.
  ! hl_gtk_menu_new: Create a menubar.
  ! hl_gtk_menu_submenu: Add a submenu to a menu
  ! hl_gtk_menu_item: Add a button to a menu
  ! hl_gtk_progress_bar_new: A progress bar.
  ! hl_gtk_progress_bar_set: Set the value of a progress bar.

  ! The iso_c_binding & gtk modules are implicitly included in the
  ! gtk_sup -- speeds compilation to omit them here.
  ! use iso_c_binding
  ! use gtk
  use gtk_sup

  implicit none

  interface hl_gtk_progress_bar_set
     module procedure  hl_gtk_progress_bar_set_f
     module procedure  hl_gtk_progress_bar_set_ii
  end interface hl_gtk_progress_bar_set

contains

  function hl_gtk_window_new(title, destroy, delete_event, border, wsize,&
       & sensitive) result(win)
    ! Higher-level interface to make a gtk_window
    !
    ! TITLE: String: optional: Title for the window
    ! DESTROY: c_funptr: optional: Callback for the "destroy" signal
    ! DELETE_EVENT: c_funptr: optional: Callback for the "delete-event" signal
    ! BORDER: integer: optional: Size of the window border
    ! WSIZE: integer(2): optional: Size of the window
    ! SENSITIVE: logical: optional: Whether the widget should initially
    ! 		be sensitive or not.

    type(c_ptr) :: win
    character(kind=c_char), dimension(*), intent(in), optional :: title
    type(c_funptr), optional :: destroy, delete_event
    integer, optional, intent(in) :: border
    integer, optional, intent(in), dimension(2) :: wsize
    integer(kind=c_int), intent(in), optional :: sensitive

    win = gtk_window_new (GTK_WINDOW_TOPLEVEL)
    call gtk_window_set_title(win, title)

    if (present(border)) call gtk_container_set_border_width(win, border)
    if (present(wsize)) &
         & call gtk_window_set_default_size(win, wsize(1), wsize(2))
    if (present(delete_event)) &
         & call g_signal_connect(win, "delete-event"//CNULL, delete_event)
    if (present(destroy)) &
         & call g_signal_connect(win, "destroy"//CNULL, destroy)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(win, sensitive)

  end function hl_gtk_window_new

  function hl_gtk_button_new(label, clicked, data, tooltip, sensitive) &
       & result(but)
    ! Higher-level button
    !
    ! LABEL: string: required: The label on the button
    ! CLICKED: c_funloc: optional: callback routine for the "clicked" signal
    ! DATA: c_loc: optional: Data to be passed to the clicked callback
    ! TOOLTIP: string: optional: tooltip to be displayed when the pointer
    ! 		is held over the button.
    ! SENSITIVE: logical: optional: Whether the widget should initially
    ! 		be sensitive or not.

    type(c_ptr) :: but
    character(kind=c_char), dimension(*), intent(in) :: label
    type(c_funptr), optional :: clicked
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), intent(in), optional :: sensitive

    but=gtk_button_new_with_label(label)

    if (present(clicked)) then
       if (present(data)) then
          call g_signal_connect(but, "clicked"//CNULL, &
               & clicked, data)
       else
          call g_signal_connect(but, "clicked"//CNULL, &
               & clicked)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(but, tooltip)
    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(but, sensitive)

  end function hl_gtk_button_new

  function hl_gtk_check_button_new(label, toggled, data, tooltip, &
       & initial_state, sensitive) result(but)
    ! Higher level check box.
    !
    ! LABEL: string: required:  The label on the button.
    ! TOGGLED: c_funloc: optional: Callback function for the "toggled" signal.
    ! DATA: c_loc: optional: Data to pass to/from the toggled callback.
    ! TOOLTIP: string: optional: A tooltip for the check_button.
    ! INITIAL_STATE: integer: optional: set the initial state of the
    !               check_button.
    ! SENSITIVE: logical: optional: Whether the widget should initially
    ! 		be sensitive or not.

    type(c_ptr) :: but
    character(kind=c_char), dimension(*), intent(in) :: label
    type(c_funptr), optional :: toggled
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), intent(in), optional :: initial_state
    integer(kind=c_int), intent(in), optional :: sensitive

    but = gtk_check_button_new_with_label(label)

    if (present(toggled)) then
       if (present(data)) then
          call g_signal_connect(but, "toggled"//cnull, toggled, data)
       else
          call g_signal_connect(but, "toggled"//cnull, toggled)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(but, tooltip)

    if (present(initial_state)) &
         & call gtk_toggle_button_set_active(but, initial_state)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(but, sensitive)

  end function hl_gtk_check_button_new

  function hl_gtk_radio_button_new(group, label, toggled, data, tooltip, &
       & sensitive) result(but)
    ! Radio button
    !
    ! GROUP: c_loc: required: The group to which the button belongs.
    ! 		This is an INOUT argument so it must be a variable
    ! 		of type(c_ptr). To start a new group (menu) initialize
    ! 		the variable to CNULL, to add a new button use the value
    ! 		returned from the last call to hl_gtk_radio_button. This
    ! 		is the variable which you use to do things like setting the
    ! 		selection.
    ! LABEL: string: required: The label for the button.
    ! TOGGLED: c_funloc: optional: call back to be executed when the
    ! 		button is toggled
    ! DATA: c_loc: optional: Data to pass to/from the "toggled" callback.
    ! TOOLTIP: string: optional: A tooltip for the radio button
    ! SENSITIVE: logical: optional: Whether the widget should initially
    ! 		be sensitive or not.

    type(c_ptr) :: but
    type(c_ptr), intent(inout) :: group
    character(kind=c_char), dimension(*), intent(in) :: label
    type(c_funptr), optional :: toggled
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), intent(in), optional :: sensitive

    but = gtk_radio_button_new_with_label(group, label)
    group = gtk_radio_button_get_group(but)

    if (present(toggled)) then
       if (present(data)) then
          call g_signal_connect(but, "toggled"//cnull, toggled, data)
       else
          call g_signal_connect(but, "toggled"//cnull, toggled)
       end if
    end if
    if (present(tooltip)) call gtk_widget_set_tooltip_text(but, tooltip)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(but, sensitive)

  end function hl_gtk_radio_button_new

  subroutine hl_gtk_radio_group_set_select(group, index)
    ! Set the indexth button of a radio group
    !
    ! GROUP: c_loc: required: The group of the last button added to
    ! 		the radio menu
    ! INDEX: integer: required: The index of the button to set
    ! 		(starting from the first as 0).

    type(c_ptr), intent(in) :: group
    integer(kind=c_int), intent(in) :: index

    integer(kind=c_int) :: nbuts
    type(c_ptr) :: datan

    nbuts = g_slist_length(group)

    ! Note that GROUP actually points to the last button added and to the
    ! group of the next to last & so on

    datan= g_slist_nth_data(group, nbuts-index-1)
    call gtk_toggle_button_set_active(datan, TRUE)

  end subroutine hl_gtk_radio_group_set_select

  function hl_gtk_radio_group_get_select(group) result(index)
    ! Find the selected button in a radio group.

    integer(kind=c_int) :: index
    type(c_ptr), intent(in) :: group

    integer(kind=c_int) :: nbuts, i
    type(c_ptr) :: but

    nbuts = g_slist_length(group)
    index=-1

    do i = 1, nbuts
       but = g_slist_nth_data(group, nbuts-i)
       if (.not. c_associated(but)) exit

       if (gtk_toggle_button_get_active(but)==TRUE) then
          index = i-1
          return
       end if
    end do
  end function hl_gtk_radio_group_get_select

  function hl_gtk_entry_new(len, editable, activate, data, tooltip, value, &
       & sensitive) result(entry)
    ! Higher level text entry box
    !
    ! LEN: integer: optional: The maximum length of the entry field.
    ! EDITABLE: logical: optional: whether the entry box can be edited
    ! 		by the user
    ! ACTIVATE: c_funloc: optional: Callback function for the "activate" signal
    ! DATA: c_loc: optional: Data to be passed to the activate callback
    ! TOOLTIP: string: optional: tooltip to be displayed when the pointer
    ! 		is held over the button.
    ! VALUE: string: optional: An initial value for the entry box.
    ! SENSITIVE: logical: optional: Whether the widget should initially
    ! 		be sensitive or not.

    type(c_ptr) :: entry
    integer, intent(in), optional :: len
    integer(c_int), intent(in), optional :: editable
    type(c_funptr), optional :: activate
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip, value
    integer(kind=c_int), intent(in), optional :: sensitive

    entry = gtk_entry_new()
    call gtk_entry_set_activates_default(entry, TRUE)

    if (present(len)) call gtk_entry_set_max_length(entry, len)

    if (present(editable)) &
         & call gtk_editable_set_editable(entry, editable)

    if (present(activate)) then
       if (present(data)) then
          call g_signal_connect(entry, &
               & "activate"//CNULL, activate, data)
       else
          call g_signal_connect(entry, &
               & "activate"//CNULL, activate)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(entry, tooltip)

    if (present(value)) call gtk_entry_set_text(entry, value)
    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(entry, sensitive)

  end function hl_gtk_entry_new

  function hl_gtk_list1_new(scroll, width, changed, data, multiple, &
       & sensitive, tooltip, title, height) result(list)
    ! A single column selectable list based on the GTK Tree View
    !
    ! SCROLL: c_ptr: required: The scroll box containing the list
    ! 		(used for packing etc.)
    ! WIDTH: integer: optional: The width of the displayed column.
    ! CHANGED: c_funptr: optional: Callback function for the "changed"
    !           signal to the associated selection object.
    ! DATA: c_ptr: optional: Data to be passed to/from the callback.
    ! MULTIPLE: logical: optional: Whether multiple selections are allowed.
    ! SENSITIVE: logical: optional: Whether the widget is intially sensitive.
    ! TOOLTIP: string: optional: Tooltip for the widget
    ! TITLE: string: optional: Title for the visible column.
    ! HEIGHT: integer: optional: The height of the display (this is
    !            actually the height of the scroll box).

    type(c_ptr) :: list
    type(c_ptr), intent(out) :: scroll
    integer(kind=c_int), intent(in), optional :: width
    type(c_funptr), intent(in), optional :: changed
    type(c_ptr), intent(in), optional :: data
    integer(kind=c_int), intent(in),  optional :: multiple, sensitive
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip, title
    integer(kind=c_int), intent(in), optional :: height

    type(c_ptr), target :: renderer, column, select, model
    integer(kind=c_int) :: nc
    integer(kind=type_kind), target, dimension(2) :: types

    ! Create list storage with 2 colums (one is a dummy, to provide an index)

    types = (/ g_type_int, g_type_string /)
    model = gtk_list_store_newv(2, c_loc(types))

    ! Create visual list inside a scrollbar container
    scroll = gtk_scrolled_window_new(NULL, NULL)
    call gtk_scrolled_window_set_policy(scroll, GTK_POLICY_AUTOMATIC, &
         & GTK_POLICY_AUTOMATIC)
    list = gtk_tree_view_new_with_model(model)
    call gtk_container_add(scroll, list)

    ! Insert index column (invisible)
    renderer = gtk_cell_renderer_text_new()
    call gtk_cell_renderer_set_visible(renderer, FALSE)
    column = gtk_tree_view_column_new()
    call gtk_tree_view_column_pack_start(column, renderer, FALSE)
    call gtk_tree_view_column_set_title(column, "#"//cnull)
    call gtk_tree_view_column_add_attribute(column, renderer, &
         & "text"//CNULL, 0)
    nc = gtk_tree_view_append_column(list, column)
    call gtk_tree_view_column_set_sizing (column,GTK_TREE_VIEW_COLUMN_FIXED)
    call gtk_tree_view_column_set_max_width(column,0)

    ! Insert (visible) column

    renderer = gtk_cell_renderer_text_new()
    column = gtk_tree_view_column_new()
    call gtk_tree_view_column_pack_start(column, renderer, FALSE)
    call gtk_cell_renderer_set_alignment(renderer, 0., 0.)
    call gtk_cell_renderer_set_padding(renderer, 0, 0)
    if (present(title)) call gtk_tree_view_column_set_title(column, title)
    call gtk_tree_view_column_add_attribute(column, renderer, &
         & "text"//CNULL, 1)
    nc = gtk_tree_view_append_column(list, column)

    call gtk_tree_view_column_set_reorderable(column, FALSE)

    ! Set sizes if requested. Note that the vertical size is set with the
    ! scrollable window.
    if (present(width)) then
       call gtk_tree_view_column_set_sizing (column, GTK_TREE_VIEW_COLUMN_FIXED)
       call gtk_tree_view_column_set_fixed_width(column, width)
    end if
    if (present(height)) call gtk_widget_set_size_request(scroll,0,height)

    call gtk_tree_view_column_set_resizable(column,TRUE)

    ! The event handler is attached to the selection object, as is
    ! the multiple selection property.

    select = gtk_tree_view_get_selection(list)

    if (present(multiple)) then
       if (multiple == TRUE) &
            & call gtk_tree_selection_set_mode(select, GTK_SELECTION_MULTIPLE)
    end if

    if (present(changed)) then
       if (present(data)) then
          call g_signal_connect(select, "changed"//cnull, changed, data)
       else
          call g_signal_connect(select, "changed"//cnull, changed)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(list, tooltip)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(list, sensitive)

  end function hl_gtk_list1_new

  subroutine hl_gtk_list1_ins(list, text, row)
    ! Insert a row into a list
    !
    ! LIST: g_ptr: required: The list to insert to.
    ! TEXT: string: required: The text to insert.
    ! ROW: integer: optional: The row at which to insert the text
    ! 		(omit to append)

    type(c_ptr), intent(in) :: list
    character(kind=c_char), dimension(*), intent(in), target :: text
    integer(kind=c_int), intent(in), optional :: row

    integer(kind=c_int), target :: i, nrow
    integer(kind=c_int) :: valid

    type(c_ptr) :: store, val
    type(gtktreeiter), target :: iter
    type(gvalue), target :: vali, valt

    ! Get list storage
    store = gtk_tree_view_get_model(list)

    ! Insert row
    if (present(row)) then
       call gtk_list_store_insert(store, c_loc(iter), row)
       nrow = row
    else
       nrow=gtk_tree_model_iter_n_children (store, NULL);
       call gtk_list_store_append(store, c_loc(iter));
    end if

    ! Set value
    val = c_loc(vali)
    val=g_value_init(val, g_type_int)
    call g_value_set_int(c_loc(vali), nrow)
    call gtk_list_store_set_value(store, c_loc(iter), 0, val)
    val = c_loc(valt)
    val=g_value_init(val, g_type_string)
    call g_value_set_static_string(c_loc(valt), text)
    call gtk_list_store_set_value(store, c_loc(iter), 1, val)

    ! reset the indices for the rest of the list
    if (present(row)) then
       i = row
       do
          valid = gtk_tree_model_iter_next(store, c_loc(iter))
          if (valid == FALSE) exit
          i = i+1
          call gtk_list_store_set_value(store, c_loc(iter), 0, c_loc(i))
       end do
    end if
  end subroutine hl_gtk_list1_ins

  subroutine hl_gtk_list1_rem(list, row)
    ! Remove a row or clear a list
    !
    ! LIST: g_ptr: required: The list to modify
    ! ROW: integer: optional: The row to remove, if absent clear the list

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), optional, intent(in) :: row

    integer(kind=c_int), target :: i
    integer(kind=c_int) :: valid
    type(c_ptr) :: store, val
    type(gtktreeiter), target :: iter
    type(gvalue), target :: vali

    ! Get list store
    store = gtk_tree_view_get_model(list)

    ! If 2 arguments, then remove a row
    if (present(row)) then
       valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), NULL, row)
       if (valid==FALSE) return

       valid = gtk_list_store_remove(store, c_loc(iter))
       if (valid==TRUE) then   ! Not the last element
          i = row
          val = c_loc(vali)
          val = g_value_init(val, g_type_int)
          do
             call g_value_set_int(val, i)
             call gtk_list_store_set_value(store, c_loc(iter), 0, val)
             valid=gtk_tree_model_iter_next(store, c_loc(iter));
             if (valid==FALSE) exit
             i=i+1
          end do
       end if

    else   ! 1 argument clear the whole list
       call gtk_list_store_clear(store)
    end if
  end subroutine hl_gtk_list1_rem

  function hl_gtk_list1_get_selections(list, indices, selection) result(count)
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

    integer(kind=c_int) :: count
    type(c_ptr), intent(in) :: list
    integer(kind=c_int), dimension(:), allocatable, target, &
         & intent(out), optional :: indices
    type(c_ptr), optional :: selection

    type(c_ptr) :: slist, vselection
    type(c_ptr), target :: model
    integer(kind=c_int) :: i
    type(c_ptr) :: cindex
    integer(kind=c_int) :: valid
    type(gtktreeiter), target :: iter
    type(gvalue), target :: val

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
       valid = gtk_tree_model_get_iter(model, c_loc(iter), &
            & g_list_nth_data(slist, i-1))
       call gtk_tree_model_get_value(model, c_loc(iter), 0, c_loc(val))
       indices(i) = g_value_get_int(c_loc(val))
       call clear_gvalue(val)
    end do

    ! Free the selection list.
    call g_list_foreach(slist, c_funloc(gtk_tree_path_free), NULL)
    call g_list_free(slist)

  end function hl_gtk_list1_get_selections

  function hl_gtk_menu_new(orientation) result(menu)
    ! Menu initializer (mainly for consistency)
    !
    ! ORIENTATION: integer: optional: Whether to lay out the top level
    ! 		horizontaly or vertically. If this arguemtn is present, then
    ! 		a menubar is created, otherwise a simple menu.

    type(c_ptr) :: menu
    integer(kind=c_int), intent(in), optional :: orientation

    integer(kind=c_int) :: orient
    if (present(orientation)) then
       orient= orientation
    else
       orient = GTK_PACK_DIRECTION_LTR
    end if

    menu = gtk_menu_bar_new()
    call gtk_menu_bar_set_pack_direction (menu, orient)

  end function hl_gtk_menu_new

  function hl_gtk_menu_submenu(menu, label, tooltip, pos) result(submenu)
    ! Make a submenu node
    !
    ! MENU: c_ptr: required:  The parent of the submenu
    ! LABEL: string: required: The label of the submenu
    ! TOOLTIP: string: optional: A tooltip for the submenu.
    ! POS: integer: optional: The position at which to insert the item
    ! 		(omit to append)

    type(c_ptr) :: submenu
    type(c_ptr) :: menu
    character(kind=c_char), dimension(*), intent(in) :: label
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), optional :: pos

    type(c_ptr) :: item

    ! Create a menu item
    item = gtk_menu_item_new_with_label(label)

    ! Create a submenu and attach it to the item
    submenu = gtk_menu_new()
    call  gtk_menu_item_set_submenu(item, submenu)

    ! Insert it to the parent
    if (present(pos)) then
       call gtk_menu_shell_insert(menu, item, pos)
    else
       call gtk_menu_shell_append(menu, item)
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(item, tooltip)

  end function hl_gtk_menu_submenu

  function hl_gtk_menu_item(menu, label, activate, data, tooltip, pos) &
       & result(item)
    ! Make a menu item or separator
    !
    ! MENU: c_ptr: required: The parent menu.
    ! LABEL: string: optional: The label for the menu, if absent then insert
    ! 		a separator.
    ! ACTIVATE: c_funptr: optional: The callback function for the
    ! 		activate signal
    ! DATA: c_ptr: optional: Data to pass to the callback.
    ! TOOLTIP: string: optional: A tooltip for the menu item.
    ! POS: integer: optional: The position at which to insert the item
    ! 		(omit to append)

    type(c_ptr) ::  item
    type(c_ptr) :: menu
    character(kind=c_char), dimension(*), intent(in), optional :: label
    type(c_funptr), optional :: activate
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), optional :: pos

    ! Create the menu item
    if (present(label)) then
       item = gtk_menu_item_new_with_label(label)
    else
       item = gtk_separator_menu_item_new()
    end if

    ! Insert it to the parent
    if (present(pos)) then
       call gtk_menu_shell_insert(menu, item, pos)
    else
       call gtk_menu_shell_append(menu, item)
    end if

    ! If present, connect the callback
    if (present(activate)) then
       if (.not. present(label)) then
          print *, "HL_GTK_MENU_ITEM: Cannot connect a callback to a separator"
          return
       end if

       if (present(data)) then
          call g_signal_connect(item, "activate"//cnull, activate, data)
       else
          call g_signal_connect(item, "activate"//cnull, activate)
       end if
    end if

    ! Attach a tooltip
    if (present(tooltip)) call gtk_widget_set_tooltip_text(item, tooltip)
  end function hl_gtk_menu_item

  function hl_gtk_progress_bar_new(orientation, step) result(bar)
    ! Intializer for a progress bar
    !
    ! ORIENTATION: integer: optional: The orientation of the bar.
    ! STEP: double: optional: The fractional step to advance when
    ! 		pulsing the bar

    type(c_ptr) :: bar
    integer(kind=c_int), optional :: orientation
    real(kind=c_double), optional :: step

    bar = gtk_progress_bar_new()

    if (present(orientation)) &
         & call gtk_progress_bar_set_orientation(bar, orientation)

    if (present(step)) &
         & call gtk_progress_bar_set_pulse_step(bar, step)

  end function hl_gtk_progress_bar_new

  subroutine hl_gtk_progress_bar_set_f(bar, val, string, text)
    ! Set the value of a progress bar
    !
    ! BAR: c_ptr: required: The bar to set
    ! VAL: double: optional: The value to set. If absent, the bar is pulsed
    ! STRING: logical: optional: Whether to put a string on the bar.
    ! TEXT: string: optional: Text to put in the bar, (overrides STRING)

    type(c_ptr) :: bar
    real(kind=c_double), optional :: val
    integer(kind=c_int), optional :: string
    character(kind=c_char), dimension(*), intent(in), optional :: text

    real(kind=c_double) :: frac
    character(len=50) :: sval

    ! If no value given pulse the bar
    if (.not. present(val)) then
       call gtk_progress_bar_pulse(bar)
    else

       ! Determine the fraction to fill & fill it
       call gtk_progress_bar_set_fraction(bar, val)
    end if

    ! If annotation is needed, add it.
    if (present(text)) then
       call gtk_progress_bar_set_text (bar, text)
    else if (present(string)) then
       if (string == FALSE .or. .not. present(val)) return
       ! Otherwise we display a percentage
       write(sval, "(F5.1,'%')") val*100.

       call gtk_progress_bar_set_text (bar, trim(sval)//cnull)
    end if
  end subroutine hl_gtk_progress_bar_set_f
  subroutine hl_gtk_progress_bar_set_ii(bar, val, maxv, string, text)
    ! Set the value of a progress bar
    !
    ! BAR: c_ptr: required: The bar to set
    ! VAL: int: required: The value to set. If absent, the bar is pulsed
    ! MAXV: int: required: The maximum value for the bar
    ! STRING: logical: optional: Whether to put a string on the bar.
    ! TEXT: string: optional: Text to put in the bar, (overrides STRING)

    type(c_ptr) :: bar
    integer(kind=c_int) :: val, maxv
    integer(kind=c_int), optional :: string
    character(kind=c_char), dimension(*), intent(in), optional :: text

    real(kind=c_double) :: frac
    character(len=50) :: sval

    frac = real(val,c_double)/real(maxv,c_double)
    call gtk_progress_bar_set_fraction(bar, frac)

    ! If annotation is needed, add it.
    if (present(text)) then
       call gtk_progress_bar_set_text (bar, text)
    else if (present(string)) then
       if (string == FALSE) return
        ! Otherwise we display n or m
       write(sval, "(I0,' of ',I0)") val, maxv
       call gtk_progress_bar_set_text (bar, trim(sval)//cnull)
    end if
  end subroutine hl_gtk_progress_bar_set_ii

end module gtk_hl
