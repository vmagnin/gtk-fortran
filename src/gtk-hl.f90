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
! Last modification: 07-13-2011

module gtk_hl
  ! A bunch of procedures to implement higher level creators for
  ! the gtk-fortran widgets. Some settings and operations are also
  ! provided for the more intricate widgets.
  !
  ! Many ideas in this module were taken from the pilib gtk<->fortran
  ! interface.

  !*
  ! Routine list
  ! Currently included
  !
  ! * hl_gtk_window_new; A top-level window.
  ! * hl_gtk_box_new; A packing box
  ! * hl_gtk_box_pack; Pack widget into a box
  ! * hl_gtk_table_new; Make a new table widget
  ! * hl_gtk_table_attach; Put a widget into the table
  ! * hl_gtk_table_expand; Add row or columns to a table
  ! * hl_gtk_notebook_new; Create a new notebook widget
  ! * hl_gtk_notebook_add_page; Add a page to a noteboook widget
  ! * hl_gtk_button_new; A simple button
  ! * hl_gtk_check_button_new; A check button
  ! * hl_gtk_radio_button_new; A radio button with group.
  ! * hl_gtk_radio_group_get_select; Which member of a radio group
  !      is selected.
  ! * hl_gtk_radio_group_set_select; Select a member of a radio group.
  ! * hl_gtk_entry_new; A 1-line text box
  ! * hl_gtk_text_view_new; Multiline text view/edit
  ! * hl_gtk_text_view_insert; Insert text to text view
  ! * hl_gtk_text_view_delete; Delete text from text view
  ! * hl_gtk_text_view_get_text; Get text from text view
  ! * hl_gtk_text_view_get_cursor; Get text view cursor location
  ! * hl_gtk_text_view_get_selection; Get text view selection
  ! * hl_gtk_text_view_get_modified; Get modified status
  ! * hl_gtk_text_view_set_modified; Set/clear modified status
  ! * hl_gtk_text_view_get_info; Miscellaneous information
  ! * hl_gtk_listn_new; Create a multi-column list
  ! * hl_gtk_listn_ins; Insert a row to a multi column list
  ! * hl_gtk_listn_rem; Delete a row from a multi column list
  ! * hl_gtk_listn_get_selections; Get the selected row(s) in a multi-column
  ! list
  ! * hl_gtk_listn_set_selection; Set the selected row in a multi column list
  ! * hl_gtk_listn_set_cell; Set the value of a cell in a multi column list
  ! * hl_gtk_listn_get_cell; Get the contents of a cell in a multi-column list
  ! * hl_gtk_listn_move_row; Move a row to a new location
  ! * hl_gtk_listn_swap_rows; Exchange 2 rows
  ! * hl_gtk_listn_reorder; New order of rows.
  ! * hl_gtk_listn_get_n_rows; How many rows?
  ! * hl_gtk_listn_set_cell_data_func; Set a rendering function for a list
  ! * hl_gtk_list1_new; A single column list with indexing
  ! * hl_gtk_list1_get_selections; Get the selected row(s) from a list.
  ! * hl_gtk_list1_set_selection; Set the selected row in a single column list
  ! * hl_gtk_list1_ins; Insert a row into a list
  ! * hl_gtk_list1_rem; Delete a row from a list, or clear the list.
  ! * hl_gtk_list1_set_cell; Wrapper for above for a single column list.
  ! * hl_gtk_list1_get_cell; Wrapper for above for a single column list.
  ! * hl_gtk_list1_move_row; Move a row to a new location
  ! * hl_gtk_list1_swap_rows; Exchange 2 rows
  ! * hl_gtk_list1_reorder; New order of rows.
  ! * hl_gtk_list1_get_n_rows; How many rows?
  ! * hl_gtk_list1_set_cell_data_func; Set a rendering function for a list
  ! * hl_gtk_tree_new; Create a tree view
  ! * hl_gtk_tree_ins; Insert a row to a tree view
  ! * hl_gtk_tree_abs_iter; Find the iter for a given "absolute" row
  ! * hl_gtk_tree_row_iter; Find the iter for a given row
  ! * hl_gtk_tree_rem; Delete a row from a tree view
  ! * hl_gtk_tree_get_selections; Get the selected rows of a tree view
  ! * hl_gtk_tree_set_cell; Set a cell in a tree view
  ! * hl_gtk_tree_get_cell; Get the value of a cell in a tree view
  ! * hl_gtk_tree_set_cell_data_func; Set a rendering function for a tree
  ! * hl_gtk_menu_new; Create a menubar.
  ! * hl_gtk_menu_submenu_new; Add a submenu to a menu
  ! * hl_gtk_menu_item_new; Add a button to a menu
  ! * hl_gtk_check_menu_item_new: A checkable item in a menu
  ! * hl_gtk_radio_menu_item_new: A radio checkable item in a menu
  ! * hl_gtk_radio_menu_group_set_select: Select an item in a radio
  ! group in a menu
  ! * hl_gtk_progress_bar_new; A progress bar.
  ! * hl_gtk_progress_bar_set; Set the value of a progress bar.
  ! * hl_gtk_message_dialog_show; Show a message dialogue
  ! * hl_gtk_file_chooser_button_new; Simple file chooser button
  ! * hl_gtk_file_chooser_show; Run a more advanced file chooser
  ! * hl_gtk_slider_flt_new; Floating point slider
  ! * hl_gtk_slider_int_new; Integer slider
  ! * hl_gtk_slider_get_value; Get the value of a slider (FP)
  ! * hl_gtk_slider_set_flt; Set a floating point slider
  ! * hl_gtk_slider_set_int; Set an integer slider
  ! * hl_gtk_slider_set_range; Set the limits of a slider (float)
  ! * hl_gtk_slider_set_range_int; Set the limits of a slider (integer)
  ! * hl_gtk_spin_button_flt_new; Floating point spin button
  ! * hl_gtk_spin_button_int_new; Integer slider
  ! * hl_gtk_spin_button_get_value; Get a spin box value
  ! * hl_gtk_spin_button_set_flt; Set a floating point spin box
  ! * hl_gtk_spin_button_set_int; Set an integer spin box
  ! * hl_gtk_spin_button_set_range; Set the limits of a spin box (float)
  ! * hl_gtk_spin_button_set_range_int; Set the limits of a spin box (integer)
  ! * hl_gtk_combo_box_new; Combo box
  ! * hl_gtk_combo_box_add_text; Add an item to combo box
  ! * hl_gtk_combo_box_delete; Delete item
  ! * hl_gtk_combo_box_get_active; Get selected element
  ! * hl_gtk_chooser_resp_cb; Internal signal handler
  ! * hl_gtk_chooser_filt_cb; Internal signal handler
  ! * hl_gtk_widget_add_accelerator; Add an accelerator to a widget
  !/
  ! To facilitate the automatic extraction of API information, there are
  ! 2 types of comment block that are extracted:
  !
  ! Blocks delimited by !* and !/ which provide a section header and
  ! an optional description for a group of widgets.
  !
  ! Block delimited by !+ and !- describe an individual routine.
  ! !+ should immediately precede the function or subroutine statement.
  ! The descriptive comments should come between that and the start of
  ! the declarations. The arguments are described in a colon-separated list
  ! with name: type : status : description.
  ! If there is any description following the table, then a line with just
  ! a ! character must terminate the table.

  ! The iso_c_binding & gtk modules are implicitly included in the
  ! gtk_sup -- speeds compilation to omit them here.
  ! use iso_c_binding
  ! use gtk
  use gtk_sup

  use gtk, only: gtk_box_pack_start, gtk_button_new,&
       & gtk_button_new_with_label, gtk_cell_renderer_set_alignment, gtk_cell_renderer&
       &_set_padding, gtk_cell_renderer_set_visible, gtk_cell_renderer_text_new, gtk_c&
       &heck_button_new, gtk_check_button_new_with_label, gtk_container_add, gtk_conta&
       &iner_set_border_width, gtk_dialog_add_button, gtk_dialog_get_content_area, gtk&
       &_dialog_new, gtk_dialog_run, gtk_editable_set_editable, gtk_entry_new, gtk_ent&
       &ry_set_activates_default, gtk_entry_set_max_length, gtk_entry_set_text, &
       & gtk_entry_get_text, gtk_entry_get_text_length, gtk_hb&
       &ox_new, gtk_label_new, gtk_list_store_append, gtk_list_store_clear, gtk_list_s&
       &tore_insert, gtk_list_store_newv, gtk_list_store_remove, gtk_list_store_set_va&
       &lue, gtk_menu_bar_new, gtk_menu_bar_set_pack_direction, gtk_menu_item_new, gtk&
       &_menu_item_new_with_label, gtk_menu_item_set_submenu, gtk_menu_new, gtk_menu_s&
       &hell_append, gtk_menu_shell_insert, gtk_orientable_set_orientation, gtk_progre&
       &ss_bar_new, gtk_progress_bar_pulse, gtk_progress_bar_set_fraction, &
       & gtk_progress_bar_set_pulse_step, gtk_progress_bar_set_t&
       &ext, gtk_radio_button_get_group, gtk_radio_button_new, gtk_radio_button_new_wi&
       &th_label, gtk_scrolled_window_new, gtk_scrolled_window_set_policy, gtk_separat&
       &or_menu_item_new, gtk_toggle_button_get_active, gtk_toggle_button_set_active, &
       &gtk_tree_model_get_iter, gtk_tree_model_get_value, gtk_tree_model_iter_n_child&
       &ren, gtk_tree_model_iter_next, gtk_tree_model_iter_nth_child, gtk_tree_path_fr&
       &ee, gtk_tree_selection_get_selected, gtk_tree_selection_get_selected_rows, gtk&
       &_tree_selection_set_mode, gtk_tree_view_append_column, gtk_tree_view_column_ad&
       &d_attribute, gtk_tree_view_column_new, gtk_tree_view_column_pack_start, gtk_tr&
       &ee_view_column_set_fixed_width, gtk_tree_view_column_set_max_width, gtk_tree_v&
       &iew_column_set_reorderable, gtk_tree_view_column_set_resizable, gtk_tree_view_&
       &column_set_sizing, gtk_tree_view_column_set_title, gtk_tree_view_get_model, gt&
       &k_tree_view_get_selection, gtk_tree_view_new, gtk_tree_view_new_with_model, gt&
       &k_vbox_new, gtk_widget_destroy, gtk_widget_set_sensitive, gtk_widget_set_size_&
       &request, gtk_widget_set_tooltip_text, gtk_widget_show, gtk_widget_show_all, gt&
       &k_window_new, gtk_window_set_default, gtk_window_set_default_size, gtk_window_&
       &set_modal, gtk_window_set_title, g_signal_connect, gtk_label_set_markup, &
       & gtk_window_set_transient_for, gtk_window_set_destroy_with_parent, &
       & gtk_hscale_new, gtk_hscale_new_with_range, gtk_range_get_value, & ! Scales start
       &gtk_range_set_value, gtk_scale_set_digits, gtk_spin_button_get_value, gtk_spin&
       &_button_new, gtk_spin_button_new_with_range, gtk_spin_button_set_digits, gtk_s&
       &pin_button_set_numeric, gtk_spin_button_set_value, gtk_vscale_new, gtk_vscale_&
       &new_with_range, gtk_scale_set_draw_value, gtk_spin_button_set_wrap, & ! Scales end
       &gtk_text_buffer_delete, gtk_text_buffer_get_end_iter, gtk_text_buf& ! Text view start
       &fer_get_insert, gtk_text_buffer_get_iter_at_line, gtk_text_buffer_get_iter_at_&
       &line_offset, gtk_text_buffer_get_iter_at_mark, gtk_text_buffer_get_modified, g&
       &tk_text_buffer_get_selection_bound, gtk_text_buffer_get_selection_bounds, gtk_&
       &text_buffer_get_start_iter, gtk_text_buffer_get_text, gtk_text_buffer_insert, &
       &gtk_text_buffer_insert_at_cursor, gtk_text_buffer_new, gtk_text_buffer_set_mod&
       &ified, gtk_text_buffer_set_text, gtk_text_iter_forward_char, gtk_text_iter_for&
       &ward_chars, gtk_text_iter_forward_line, gtk_text_iter_forward_lines, gtk_text_&
       &iter_get_line, gtk_text_iter_get_line_offset, gtk_text_view_get_buffer, gtk_te&
       &xt_view_new, gtk_text_view_new_with_buffer, gtk_text_view_set_editable, &
       & gtk_text_iter_get_offset, gtk_text_buffer_get_char_count, &
       & gtk_text_buffer_get_line_count, & ! text view end
       & gtk_image_new_from_stock, &
       &gtk_combo_box_get_active, gtk_combo_box_new,  &
       & gtk_combo_box_set_active, & ! COMBO
       & gtk_file_chooser_add_filter,&   ! File chooser start
       & gtk_file_chooser_button_new, gtk_file_chooser_button_set_width_chars, &
       & gtk_file_chooser_get_current_folder,&
       & gtk_file_chooser_get_file, gtk_file_chooser_get_filename,&
       & gtk_file_chooser_get_filenames,&
       & gtk_file_chooser_get_local_only, gtk_file_chooser_get_uri,&
       & gtk_file_chooser_get_uris, gtk_file_chooser_select_file,&
       & gtk_file_chooser_select_filename,&
       & gtk_file_chooser_set_current_folder,&
       & gtk_file_chooser_set_file, gtk_file_chooser_set_filename,&
       & gtk_file_chooser_set_local_only,&
       & gtk_file_chooser_set_show_hidden,&
       & gtk_file_chooser_widget_new, gtk_file_filter_add_pattern,&
       & gtk_file_filter_new, gtk_file_filter_set_name,&
       & gtk_file_chooser_set_select_multiple,&
       & gtk_file_chooser_set_extra_widget,&
       & gtk_file_filter_add_mime_type,&
       & gtk_file_chooser_set_do_overwrite_confirmation, & ! File
       !  chooser end
       & gtk_tree_model_get_column_type, gtk_tree_view_column_set_sort_column_id, & ! List-n
       & gtk_tree_model_get_value, gtk_tree_view_column_get_tree_view, &
       & gtk_tree_selection_unselect_all, gtk_tree_selection_select_iter, &
       & gtk_tree_model_get_iter_first, gtk_tree_view_column_set_sort_indicator, & ! List-n end
       & gtk_tree_path_get_depth, gtk_tree_path_get_indices_with_depth, & ! Tree
       & gtk_tree_store_remove, gtk_tree_model_iter_children, gtk_tree_model_iter_parent, &
       & gtk_tree_store_newv, gtk_tree_store_set_value, gtk_tree_store_clear, &
       & gtk_tree_store_append, gtk_tree_store_insert, gtk_tree_store_prepend, &
       & gtk_tree_store_insert_before, gtk_tree_path_get_indices,  & ! Tree end
       & gtk_notebook_append_page, gtk_notebook_insert_pag& ! Containers
       &e, gtk_notebook_new, gtk_notebook_popup_disable, gtk_notebook_popup_enable, gt&
       &k_notebook_prepend_page, &
       & gtk_notebook_set_scrollable, gtk_notebook_s&
       &et_show_tabs, gtk_notebook_set_tab_detachable, gtk_notebook_set_tab_pos, gtk_n&
       &otebook_set_tab_reorderable, gtk_table_attach, gtk_table_get_size, gtk_table_n&
       &ew, gtk_table_resize, gtk_table_set_col_spacing, gtk_table_set_col_spacings, g&
       &tk_table_set_row_spacing, gtk_table_set_row_spacings, gtk_notebook_set_group_name,  & ! end containers
       & gtk_window_set_resizable, gtk_window_set_decorated, & !Window new stuff
       & gtk_window_set_deletable, gtk_window_set_keep_above, gtk_window_set_keep_below, & ! End W
       & gtk_tearoff_menu_item_new, gtk_list_store_reorder, gtk_list_store_swap, &
       & gtk_list_store_move_after, gtk_list_store_move_before, &
       & gtk_window_set_transient_for, gtk_tree_view_get_column, gtk_cell_layout_get_cells, &
       & gtk_tree_view_column_set_cell_data_func, &
       & gtk_radio_menu_item_new_with_label, gtk_check_menu_item_new_with_label, &
       & gtk_check_menu_item_set_active, gtk_radio_menu_item_get_group, &
       & gtk_tree_path_new_from_string, gtk_accel_group_new, &
       & gtk_widget_add_accelerator, gtk_window_add_accel_group, &
       & gtk_range_get_adjustment, gtk_range_set_range, gtk_adjustment_get_upper, &
       & gtk_adjustment_get_lower, gtk_spin_button_get_range, &
       & gtk_spin_button_set_range, &
       & TRUE, FALSE, NULL, CNULL, FNULL, &
       & GTK_WINDOW_TOPLEVEL, GTK_POLICY_AUTOMATIC, GTK_TREE_VIEW_COLUMN_FIXED, &
       & GTK_SELECTION_MULTIPLE, GTK_PACK_DIRECTION_LTR, GTK_BUTTONS_NONE, &
       & GTK_BUTTONS_OK, GTK_BUTTONS_CLOSE, GTK_BUTTONS_CANCEL, GTK_BUTTONS_YES_NO, &
       & GTK_BUTTONS_OK_CANCEL, GTK_RESPONSE_OK, GTK_RESPONSE_CLOSE, GTK_RESPONSE_CANCEL, &
       & GTK_RESPONSE_YES, GTK_RESPONSE_NO, GTK_RESPONSE_NONE, &
       & GTK_MESSAGE_INFO, GTK_MESSAGE_WARNING, GTK_MESSAGE_ERROR, &
       & GTK_MESSAGE_QUESTION, GTK_MESSAGE_OTHER, GTK_ICON_SIZE_DIALOG, &
       & GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,& ! File chooser const
       & GTK_FILE_CHOOSER_ACTION_OPEN, GTK_RESPONSE_APPLY,&
       & GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER,&
       & GTK_FILE_CHOOSER_ACTION_SAVE, GTK_RESPONSE_DELETE_EVENT, & ! End file chooser consts
       & GTK_TREE_VIEW_COLUMN_FIXED, GTK_EXPAND, GTK_FILL, GDK_CONTROL_MASK, &
       & GTK_ACCEL_VISIBLE, &
  !GTK2
!!$2       & gtk_progress_bar_set_orientation, &
!!$2       & gtk_combo_box_append_text, gtk_combo_box_entry_new, &
!!$2       & gtk_combo_box_entry_new_text, gtk_notebook_set_group, &
!!$2       & gtk_combo_box_get_active_text, gtk_combo_box_insert_text, &
!!$2       & gtk_combo_box_new_text, gtk_combo_box_prepend_text, &
!!$2       & gtk_combo_box_remove_text, gtk_notebook_set_group, &
!!$2       & GTK_PROGRESS_LEFT_TO_RIGHT, GTK_PROGRESS_BOTTOM_TO_TOP, &
!!$2       & GTK_PROGRESS_TOP_TO_BOTTOM, GTK_PROGRESS_RIGHT_TO_LEFT
  ! Replace the last 2 lines with the next 2 for GTK3
       & GTK_ORIENTATION_VERTICAL, GTK_ORIENTATION_HORIZONTAL, & 
       & gtk_progress_bar_set_inverted, gtk_progress_bar_set_show_text, &
       & gtk_combo_box_text_append_text, &
       & gtk_combo_box_text_get_active_text, gtk_combo_box_text_insert_text, &
       & gtk_combo_box_text_new, gtk_combo_box_text_new_with_entry, &
       & gtk_combo_box_text_prepend_text, gtk_combo_box_text_remove, &
       & gtk_notebook_set_group_name

       use gdk, only: gdk_keyval_from_name

  use g, only: alloca, g_list_foreach, g_list_free, g_list_length, g_list_nth, g_&
       &list_nth_data, g_slist_length, g_slist_nth, g_slist_nth_data, g_value_get_int, &
       & g_value_init, g_value_set_int, g_value_set_static_string, g_strv_length, g_free, &
       & g_slist_free, g_value_set_char, g_value_set_long, g_value_set_int64, g_value_set_float, &
       & g_value_set_string, g_value_set_double, g_value_set_uchar, g_value_set_ulong, &
       & g_value_set_uint64, g_value_set_boolean, g_value_set_uint,  &
       & g_value_get_char, g_value_get_long, g_value_get_int64, g_value_get_float, &
       & g_value_get_string, g_value_get_double, g_value_get_uchar, g_value_get_ulong, &
       & g_value_get_uint64, g_value_get_boolean, g_value_get_uint, &
       & g_list_nth_data, g_object_set_property, g_object_set_data, &
       & g_object_get_data
  use iso_c_binding

  implicit none

  ! A progress bar value can be given as a fraction or m of n
  interface hl_gtk_progress_bar_set
     module procedure  hl_gtk_progress_bar_set_f
     module procedure  hl_gtk_progress_bar_set_ii
  end interface hl_gtk_progress_bar_set

  ! A slider or a spin button can use integers or floats for its settings.
  interface hl_gtk_slider_new
     module procedure hl_gtk_slider_flt_new
     module procedure hl_gtk_slider_int_new
  end interface hl_gtk_slider_new
  interface hl_gtk_slider_set_value
     module procedure hl_gtk_slider_set_flt
     module procedure hl_gtk_slider_set_int
  end interface hl_gtk_slider_set_value

  interface hl_gtk_spin_button_new
     module procedure hl_gtk_spin_button_flt_new
     module procedure hl_gtk_spin_button_int_new
  end interface hl_gtk_spin_button_new
  interface hl_gtk_spin_button_set_value
     module procedure hl_gtk_spin_button_set_flt
     module procedure hl_gtk_spin_button_set_int
  end interface hl_gtk_spin_button_set_value

  ! These items must be shared between the file chooser widget and its event
  ! handler or the filter editor. They are passed to the signal handlers
  ! via the user data argument. Even though it's never used in the C code,
  ! it still has to be bind(c) otherwise c_loc() will croak on it.

  type, bind(c) :: hl_gtk_chooser_info
     type(c_ptr) :: chooser=NULL, chooser_sel_list=NULL
     type(c_ptr) :: chooser_curdir=NULL, fentry=NULL
     integer(kind=c_int) :: iselect=0
  end type hl_gtk_chooser_info

contains

  !*
  ! Containers
  ! The high-level interface provides convenience interfaces for:
  ! * Window, the gtk top-level window.
  ! * Box, Horizontal and vertical boxes to pack widgets. This was added because the
  !   gtk_box_pack_start_defaults procedure is removed from GTK3.x
  ! * Table, a grid layout of widgets
  !   Note that the wierd convention where rows comes before columns in sizing
  !   tables, but X before Y in adding widgets follows the convention of GTK proper.
  ! * Notebook, a tabbed container to pack widgets
  !/
  !+
  function hl_gtk_window_new(title, destroy, delete_event, data_destroy, &
       & data_delete_event, border, wsize, sensitive, resizable, decorated, &
       & deletable, above, below, parent, accel_group) result(win)

    type(c_ptr) :: win
    character(kind=c_char), dimension(*), intent(in), optional :: title
    type(c_funptr), optional :: destroy, delete_event
    type(c_ptr), optional :: data_destroy, data_delete_event
    integer, optional, intent(in) :: border
    integer, optional, intent(in), dimension(2) :: wsize
    integer(kind=c_int), intent(in), optional :: sensitive, resizable, decorated
    integer(kind=c_int), intent(in), optional :: deletable, above, below
    type(c_ptr), intent(in), optional :: parent
    type(c_ptr), intent(out), optional :: accel_group

    ! Higher-level interface to make a gtk_window
    !
    ! TITLE: String: optional: Title for the window
    ! DESTROY: c_funptr: optional: Callback for the "destroy" signal
    ! DELETE_EVENT: c_funptr: optional: Callback for the "delete-event" signal
    ! DATA_DESTROY: c_ptr: optional: Data to be passed to the destroy
    ! 		signal handler
    ! DATA_DELETE_EVENT: c_ptr: optional: Data to be passed to the
    ! 		delete_event signal handler
    ! BORDER: integer: optional: Size of the window border
    ! WSIZE: integer(2): optional: Size of the window
    ! SENSITIVE: boolean: optional: Whether the widget should initially
    ! 		be sensitive or not.
    ! RESIZABLE: boolean: optional: Is the window resizable.
    ! DECORATED: boolean: optional: Set FALSE to disable window decorations.
    ! DELETABLE: boolean: optional: Set to FALSE to remove the "delete" button.
    ! ABOVE: boolean: optional: Set to TRUE to make the window stay on top of others.
    ! BELOW: boolean: optional: Set to TRUE to make the window stay below others.
    ! PARENT: c_ptr: optional: An optional parent window for the new window.
    ! ACCEL_GROUP: c_ptr: optional: An accelerator group, used to add
    ! 		accelerators to widgets within the window.
    !-

    win = gtk_window_new (GTK_WINDOW_TOPLEVEL)
    call gtk_window_set_title(win, title)

    if (present(border)) call gtk_container_set_border_width(win, border)
    if (present(wsize)) &
         & call gtk_window_set_default_size(win, wsize(1), wsize(2))

    if (present(delete_event)) then
       if (present(data_delete_event)) then
          call g_signal_connect(win, "delete-event"//CNULL, delete_event, &
               & data_delete_event)
       else
          call g_signal_connect(win, "delete-event"//CNULL, delete_event)
       end if
    end if

    if (present(destroy)) then
       if (present(data_destroy)) then
          call g_signal_connect(win, "destroy"//CNULL, destroy, &
               & data_destroy)
       else
          call g_signal_connect(win, "destroy"//CNULL, destroy)
       end if
    end if

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(win, sensitive)

    if (present(resizable)) &
         & call gtk_window_set_resizable(win, resizable)
    if (present(decorated)) &
         & call gtk_window_set_decorated(win, decorated)
    if (present(deletable)) &
         & call gtk_window_set_deletable(win, deletable)
    if (present(above)) &
         & call gtk_window_set_keep_above(win, above)
    if (present(below)) &
         & call gtk_window_set_keep_below(win, below)

    if (present(parent)) call gtk_window_set_transient_for(win, parent)

    if (present(accel_group)) then
       accel_group = gtk_accel_group_new()
       call gtk_window_add_accel_group(win, accel_group)
    end if
  end function hl_gtk_window_new

  !+
  function hl_gtk_box_new(horizontal, homogeneous, spacing) result(box)

    type(c_ptr) :: box
    integer(kind=c_int), intent(in), optional :: horizontal, homogeneous
    integer(kind=c_int), intent(in), optional :: spacing

    ! Generic packing box
    !
    ! HORIZONTAL: boolean: optional: Set to TRUE to make a row box. FALSE or
    !		absent implies a column box.
    ! HOMOGENEOUS: boolean: optional: If set to TRUE then all children are
    ! 		the same size, FALSE or absent allows each widget to take its
    ! 		natural size.
    ! SPACING: c_int: optional: Set the space between children.
    !-

    integer(kind=c_int) :: grid, space

    if (present(homogeneous)) then
       grid = homogeneous
    else
       grid=FALSE
    end if

    if (present(spacing)) then
       space = spacing
    else
       space=0
    end if

    if (present(horizontal)) then
       if (horizontal == TRUE) then
          box = gtk_hbox_new(grid, space)
       else
          box = gtk_vbox_new(grid, space)
       end if
    else
       box = gtk_vbox_new(grid, space)
    end if
  end function hl_gtk_box_new

  !+
  subroutine hl_gtk_box_pack(box, child, expand, fill, padding, atend)

    type(c_ptr), intent(in) :: box, child
    integer(kind=c_int), intent(in), optional :: expand, fill
    integer(kind=c_int), intent(in), optional :: padding
    integer(kind=c_int), intent(in), optional :: atend

    ! Put a widget into a box
    !
    ! BOX: c_ptr: required: The box into which to put the child
    ! CHILD: c_ptr: required: The child to pack
    ! EXPAND: boolean: optional: If TRUE then expand this child when
    ! 		filling the box, if FALSE don't, (Default TRUE)
    ! FILL: boolean: optional: If TRUE, then expand the widget when
    ! 		expanding, if FALSE, then put space round it. (Default TRUE,
    ! 		ignored if EXPAND==FALSE.
    ! PADDING: c_int: optional: Extra space to put around the child in the
    ! 		fill direction.
    ! ATEND: boolean: optional: If present and TRUE, then put the child at
    ! 		the end of the box rather than the start.
    !-

    integer(kind=c_int) :: iexp, ifill, ipad, iend

    if (present(expand)) then
       iexp = expand
    else
       iexp = TRUE
    end if
    if (present(fill)) then
       ifill = fill
    else
       ifill = TRUE
    end if
    if (present(padding)) then
       ipad = padding
    else
       ipad = 0
    end if
    if (present(atend)) then
       iend = atend
    else
       iend = FALSE
    end if

    call gtk_box_pack_start(box, child, iexp, ifill, ipad)
  end subroutine hl_gtk_box_pack

  !+
  function hl_gtk_table_new(nrows, ncols, homogeneous, row_spacing, &
       & col_spacing) result(table)

    type(c_ptr) :: table
    integer(kind=c_int), intent(in) :: nrows, ncols
    integer(kind=c_int), intent(in), optional :: homogeneous
    integer(kind=c_int), intent(in), optional :: row_spacing, col_spacing

    ! Utility interface to create a table container
    !
    ! NROWS: c_int: required: The initial number of rows.
    ! NCOLS: c_int: required: The initial number of columns.
    ! HOMOGENEOUS: boolean: optional: Whether the cells all have the
    ! 	same size.
    ! ROW_SPACING: c_int: optional: Spacing between rows.
    ! COL_SPACING: c_int: optional: Spacing between columns.
    !-

    integer(kind=c_int) :: grid

    if (present(homogeneous)) then
       grid = homogeneous
    else
       grid = FALSE
    end if

    table = gtk_table_new(nrows, ncols, grid)

    if (present(row_spacing)) &
         & call gtk_table_set_row_spacings(table, row_spacing)
    if (present(col_spacing)) &
         & call gtk_table_set_col_spacings(table, col_spacing)

  end function hl_gtk_table_new

  !+
  subroutine hl_gtk_table_attach(table, widget, ix, iy, xspan, yspan, &
       & xpad, ypad, xopts, yopts)

    type(c_ptr), intent(in) :: table, widget
    integer(kind=c_int), intent(in) :: ix, iy
    integer(kind=c_int), intent(in), optional :: xspan, yspan
    integer(kind=c_int), intent(in), optional :: xpad, ypad
    integer(kind=c_int), intent(in), optional :: xopts, yopts

    ! Attach a widget to a table
    !
    ! TABLE: c_ptr: required: The table to which to attach
    ! WIDGET: c_ptr: required: The widget to attach to the table
    ! IX: c_int: required: The cell number of the left edge of the widget
    ! IY: c_int: required: The cell number of the top edge of the widget.
    ! XSPAN: c_int: optional: How many cells to span in the X direction (1)
    ! YSPAN: c_int: optional: How many cells to span in the Y direction (1)
    ! XPAD: c_int: optional: Padding around the cell in the X direction
    ! YPAD: c_int: optional: Padding in the Y direction
    ! XOPTS: c_int: optional: X fill/expand options (from the
    ! 		GtkAttachOptions enumerator, or 0 for none)
    ! YOPTS: c_int: optional: Y fill/expand options.
    !-

    integer(kind=c_int) :: ixtop, iytop
    integer(kind=c_int) :: ixpad, iypad
    integer(kind=c_int) :: ixopt, iyopt

    if (present(xspan)) then
       ixtop = ix + xspan
    else
       ixtop = ix+1
    end if
    if (present(yspan)) then
       iytop = iy + yspan
    else
       iytop = iy+1
    end if

    if (present(xpad)) then
       ixpad = xpad
    else
       ixpad = 0
    end if
    if (present(ypad)) then
       iypad = ypad
    else
       iypad = 0
    end if

    if (present(xopts)) then
       ixopt = xopts
    else
       ixopt = ior(GTK_EXPAND, GTK_FILL)
    end if
    if (present(yopts)) then
       iyopt = yopts
    else
       iyopt = ior(GTK_EXPAND, GTK_FILL)
    end if

    call gtk_table_attach(table, widget, ix, ixtop, iy, iytop, &
         & ixopt, iyopt, ixpad, iypad)

  end subroutine hl_gtk_table_attach

  !+
  subroutine hl_gtk_table_expand(table, ny, nx)

    type(c_ptr), intent(in) :: table
    integer(kind=c_int), intent(in), optional :: ny, nx

    ! Add rows and/or columns to a table
    !
    ! TABLE: c_ptr: required: The table to enlarge
    ! NY: c_int: optional: How many rows to add
    ! NX: c_int: optional: How many columns to add
    !
    ! To set an absolute size, use gtk_table_resize
    ! directly. Negative NX and/or NY will reduce the table.
    !-

    integer(kind=c_int), target :: sx, sy

    if (.not. (present(nx) .or. present(ny))) return  ! No resize

    call gtk_table_get_size(table, c_loc(sy), c_loc(sx))

    if (present(nx)) sx = sx+nx
    if (present(ny)) sy = sy+ny

    call gtk_table_resize(table, sy, sx)

  end subroutine hl_gtk_table_expand

  !+
  function hl_gtk_notebook_new(show_tabs, tab_position, popup, &
       & scrollable, group) result(nbook)

    type(c_ptr) :: nbook
    integer(kind=c_int), intent(in), optional :: show_tabs
    integer(kind=c_int), intent(in), optional :: tab_position
    integer(kind=c_int), intent(in), optional :: popup, scrollable
    character(kind=c_char), intent(in), optional, dimension(*), target :: group

    ! Convenience function to create a notebook (tabbed) container
    !
    ! SHOW_TABS: boolean: optional: Whether the tabs are visible
    ! TAB_POSITION: c_int: optional:  Where the tabs are placed (from the
    ! 		GtkPositionType enumerator).
    ! POPUP: boolean: optional: Whether to have a popup tab selector.
    ! SCROLLABLE: boolean: optional: Whether the tabs are scrollable if
    ! 		there are too many to fit.
    ! GROUP: string: optional: A group name for the notebook (needed if you
    ! 		want to drag tabs from one book to another). N.B. For GTK+2,
    ! 		this probably has to be a variable to work.
    !-

    nbook = gtk_notebook_new()

    if (present(show_tabs)) &
         & call gtk_notebook_set_show_tabs(nbook, show_tabs)

    if (present(tab_position)) &
         & call gtk_notebook_set_tab_pos(nbook, tab_position)

    if (present(popup)) then
       if (popup == FALSE) then
          call gtk_notebook_popup_disable(nbook)
       else
          call gtk_notebook_popup_enable(nbook)
       end if
    end if

    if (present(scrollable)) &
         & call gtk_notebook_set_scrollable(nbook, scrollable)

    if (present(group)) &
         & call gtk_notebook_set_group_name(nbook, group)
!!$2         & call gtk_notebook_set_group(nbook, c_loc(group))

  end function hl_gtk_notebook_new

  !+
  function hl_gtk_notebook_add_page(nbook, page, position, at_start, &
       & reorderable, detachable, label) result(location)

    integer(kind=c_int) :: location
    type(c_ptr), intent(in) :: nbook, page
    integer(kind=c_int), intent(in), optional :: position
    integer(kind=c_int), intent(in), optional :: at_start, reorderable, &
         & detachable
    character(kind=c_char), dimension(*), intent(in), optional :: label

    ! Convenience function to add a page to a notebook.
    !
    ! NBOOK: c_ptr: required: The book to which to add the page
    ! PAGE: c_ptr: required: The page to at to the book.
    ! POSITION: c_int: optional: The position at which to add the page.
    ! AT_START: boolean: optional: Set to TRUE to add at the start. (If neither
    ! 		AT_START nor POSITION is given the page is added at the end).
    ! REORDERABLE: boolean: optional: Whether the tab can be reordered by
    ! 		drag and drop
    ! DETACHABLE: boolean: optional: Whether the tab can be dragged to a
    ! 		different notebook (requires a group name for the notebooks).
    ! LABEL: string: optional: A label to show on the tab.
    !
    ! Returns the location at which the tab was added or -1 for failure.
    !-

    type(c_ptr) :: lwidget
    integer(kind=c_int) :: istart

    if (present(label)) then
       lwidget = gtk_label_new(label)
    else
       lwidget = NULL
    end if

    if (present(position)) then
       location = gtk_notebook_insert_page(nbook, page, lwidget, position)
    else
       if (present(at_start)) then
          istart = at_start
       else
          istart = FALSE
       end if
       if (istart == FALSE) then
          location = gtk_notebook_append_page(nbook, page, lwidget)
       else
          location = gtk_notebook_prepend_page(nbook, page, lwidget)
       end if
    end if

    if (location < 0) return

    if (present(reorderable)) &
         & call gtk_notebook_set_tab_reorderable(nbook, page, reorderable)

    if (present(detachable)) &
         & call gtk_notebook_set_tab_detachable(nbook, page, detachable)

  end function hl_gtk_notebook_add_page

  !*
  ! Buttons
  ! Convenience interfaces for regular buttons, checkboxes and radio menus.
  !/
  !+
  function hl_gtk_button_new(label, clicked, data, tooltip, sensitive, &
       & accel_key, accel_mods, accel_group, accel_flags) result(but)

    type(c_ptr) :: but
    character(kind=c_char), dimension(*), intent(in) :: label
    type(c_funptr), optional :: clicked
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), intent(in), optional :: sensitive
    character(kind=c_char), dimension(*), optional, intent(in) :: accel_key
    integer(kind=c_int), optional, intent(in) :: accel_mods, accel_flags
    type(c_ptr), optional, intent(in) :: accel_group

    ! Higher-level button
    !
    ! LABEL: string: required: The label on the button
    ! CLICKED: c_funptr: optional: callback routine for the "clicked" signal
    ! DATA: c_ptr: optional: Data to be passed to the clicked callback
    ! TOOLTIP: string: optional: tooltip to be displayed when the pointer
    ! 		is held over the button.
    ! SENSITIVE: boolean: optional: Whether the widget should initially
    ! 		be sensitive or not.
    ! ACCEL_KEY: string: optional: Set to the character value or code of a
    ! 		key to use as an accelerator.
    ! ACCEL_MODS: c_int: optional: Set to the modifiers for the accelerator.
    ! 		(If not given then GTK_CONTROL_MASK is assumed).
    ! ACCEL_GROUP: c_ptr: optional: The accelerator group to which the
    ! 		accelerator is attached, must have been added to the top-level
    ! 		window.
    ! ACCEL_FLAGS: c_int: optional: Flags for the accelerator, if not present
    ! 		then GTK_ACCEL_VISIBLE, is used (to hide the accelerator,
    ! 		use ACCEL_FLAGS=0).
    !-

    but=gtk_button_new_with_label(label)

    if (present(clicked)) then
       if (present(data)) then
          call g_signal_connect(but, "clicked"//CNULL, &
               & clicked, data)
       else
          call g_signal_connect(but, "clicked"//CNULL, &
               & clicked)
       end if

       ! An accelerator
       if (present(accel_key) .and. present(accel_group)) &
            & call hl_gtk_widget_add_accelerator(but, "clicked"//cnull, &
            & accel_group, accel_key, accel_mods, accel_flags)

    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(but, tooltip)
    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(but, sensitive)

  end function hl_gtk_button_new

  !+
  function hl_gtk_check_button_new(label, toggled, data, tooltip, &
       & initial_state, sensitive) result(but)

    type(c_ptr) :: but
    character(kind=c_char), dimension(*), intent(in) :: label
    type(c_funptr), optional :: toggled
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), intent(in), optional :: initial_state
    integer(kind=c_int), intent(in), optional :: sensitive

    ! Higher level check box.
    !
    ! LABEL: string: required:  The label on the button.
    ! TOGGLED: c_funptr: optional: Callback function for the "toggled" signal.
    ! DATA: c_ptr: optional: Data to pass to/from the toggled callback.
    ! TOOLTIP: string: optional: A tooltip for the check_button.
    ! INITIAL_STATE: integer: optional: set the initial state of the
    !               check_button.
    ! SENSITIVE: boolean: optional: Whether the widget should initially
    ! 		be sensitive or not.
    !-

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

  !+
  function hl_gtk_radio_button_new(group, label, toggled, data, tooltip, &
       & sensitive) result(but)

    type(c_ptr) :: but
    type(c_ptr), intent(inout) :: group
    character(kind=c_char), dimension(*), intent(in) :: label
    type(c_funptr), optional :: toggled
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), intent(in), optional :: sensitive

    ! Radio button
    !
    ! GROUP: c_ptr: required: The group to which the button belongs.
    ! 		This is an INOUT argument so it must be a variable
    ! 		of type(c_ptr). To start a new group (menu) initialize
    ! 		the variable to NULL, to add a new button use the value
    ! 		returned from the last call to hl_gtk_radio_button_new. This
    ! 		is the variable which you use to do things like setting the
    ! 		selection.
    ! LABEL: string: required: The label for the button.
    ! TOGGLED: c_funptr: optional: call back to be executed when the
    ! 		button is toggled
    ! DATA: c_ptr: optional: Data to pass to/from the "toggled" callback.
    ! TOOLTIP: string: optional: A tooltip for the radio button
    ! SENSITIVE: boolean: optional: Whether the widget should initially
    ! 		be sensitive or not.
    !-

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

  !+
  subroutine hl_gtk_radio_group_set_select(group, index)

    type(c_ptr), intent(in) :: group
    integer(kind=c_int), intent(in) :: index

    ! Set the indexth button of a radio group
    !
    ! GROUP: c_ptr: required: The group of the last button added to
    ! 		the radio menu
    ! INDEX: integer: required: The index of the button to set
    ! 		(starting from the first as 0).
    !-

    integer(kind=c_int) :: nbuts
    type(c_ptr) :: datan

    nbuts = g_slist_length(group)

    ! Note that GROUP actually points to the last button added and to the
    ! group of the next to last & so on

    datan= g_slist_nth_data(group, nbuts-index-1)
    call gtk_toggle_button_set_active(datan, TRUE)

  end subroutine hl_gtk_radio_group_set_select

  !+
  function hl_gtk_radio_group_get_select(group) result(index)

    integer(kind=c_int) :: index
    type(c_ptr) :: group

    ! Find the selected button in a radio group.
    !
    ! GROUP: c_ptr: required: The group of the last button added to
    ! 		the radio menu
    !-

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

  !*
  ! Text Entry
  ! Convenience functions for both single and multiple line text boxes.
  !
  ! The single line is just wrappers for the GtkEntry widget.
  !
  ! The multi line editor is based around the GtkTextView widget family. The HL interface hides
  ! the text buffer from the user, except in some callbacks where the signal
  ! is attached to the buffer not the view.
  !
  ! If you do need to access the text buffer directly it can be obtained with
  ! the gtk_text_view_get_buffer function, or it can be returned via the optional
  ! BUFFER argument to the constructor.
  !/

  !+
  function hl_gtk_entry_new(len, editable, activate, data, tooltip, value, &
       & sensitive, changed, data_changed, delete_text, data_delete_text, &
       & insert_text, data_insert_text) result(entry)

    type(c_ptr) :: entry
    integer, intent(in), optional :: len
    integer(c_int), intent(in), optional :: editable
    type(c_funptr), optional :: activate
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip, value
    integer(kind=c_int), intent(in), optional :: sensitive
    type(c_funptr), optional :: changed, delete_text, insert_text
    type(c_ptr), optional :: data_changed, data_delete_text, data_insert_text

    ! Higher level text entry box
    !
    ! LEN: integer: optional: The maximum length of the entry field.
    ! EDITABLE: boolean: optional: whether the entry box can be edited
    ! 		by the user
    ! ACTIVATE: c_funptr: optional: Callback function for the "activate" signal
    ! DATA: c_ptr: optional: Data to be passed to the activate callback (this
    ! 		is a plain DATA because the changed and other signals were
    ! 		added later.
    ! TOOLTIP: string: optional: tooltip to be displayed when the pointer
    ! 		is held over the button.
    ! VALUE: string: optional: An initial value for the entry box.
    ! SENSITIVE: boolean: optional: Whether the widget should initially
    ! 		be sensitive or not.
    ! CHANGED: c_funptr: optional: Callback for the "changed" signal.
    ! DATA_CHANGED: c_ptr: optional: Data to be passed to the changed callback.
    ! DELETE_TEXT: c_funptr: optional: Callback for the "delete-text" signal.
    ! DATA_DELETE_TEXT: c_ptr: optional: Data to be passed to the delete_text
    !            callback
    ! INSERT_TEXT: c_funptr: optional: Callback for the "insert-text" signal.
    ! DATA_INSERT_TEXT: c_ptr: optional: Data to be passed to the insert_text
    !            callback
    !-

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

    if (present(changed)) then
       if (present(data_changed)) then
          call g_signal_connect(entry, "changed"//CNULL, changed, &
               & data_changed)
       else
          call g_signal_connect(entry, "changed"//CNULL, changed)
       end if
    end if
    if (present(delete_text)) then
       if (present(data_delete_text)) then
          call g_signal_connect(entry, "delete-text"//CNULL, delete_text, &
               & data_delete_text)
       else
          call g_signal_connect(entry, "delete-text"//CNULL, delete_text)
       end if
    end if
    if (present(insert_text)) then
       if (present(data_insert_text)) then
          call g_signal_connect(entry, "insert-text"//CNULL, insert_text, &
               & data_insert_text)
       else
          call g_signal_connect(entry, "insert-text"//CNULL, insert_text)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(entry, tooltip)

    if (present(value)) call gtk_entry_set_text(entry, value)
    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(entry, sensitive)

  end function hl_gtk_entry_new

  !+
  subroutine hl_gtk_entry_get_text(entry, text, status)

    type(c_ptr), intent(in) :: entry
    character(len=*), intent(out) :: text
    integer(kind=c_int), optional, intent(out) :: status

    ! Return the text in an entry box as a fortran string.
    !
    ! ENTRY: c_ptr: required: The text entry to read
    ! TEXT: f_string: required: The text read.
    ! STATUS: c_int: optional: Returns -1 if the string is truncated.
    !
    ! To return the text as a c-pointer use gtk_entry_get_text
    !-

    type(c_ptr) :: ctext
    character(kind=c_char), dimension(:), pointer :: textptr
    integer(kind=c_int16_t) :: ntext
    integer(kind=c_int) :: istat

    ntext = gtk_entry_get_text_length(entry)
    if (ntext == 0) then
       text=''
       return
    end if
    ctext = gtk_entry_get_text(entry)

    call c_f_pointer(ctext, textptr, (/int(ntext,c_int)/))
    call convert_c_string(textptr, text, istat)

    if (present(status)) status=istat
  end subroutine hl_gtk_entry_get_text

  !+
  function hl_gtk_text_view_new(scroll, editable, changed, data_changed, &
       & insert_text, data_insert_text, delete_range, data_delete_range, &
       & initial_text, sensitive, tooltip, ssize, buffer) result(view)

    type(c_ptr) :: view
    type(c_ptr), intent(out), optional :: scroll
    integer(kind=c_int), intent(in), optional :: editable
    type(c_funptr), optional :: changed, insert_text, delete_range
    type(c_ptr), optional :: data_changed, data_insert_text, data_delete_range
    character(len=*), dimension(:), intent(in), optional :: initial_text
    integer(kind=c_int), intent(in), optional :: sensitive
    character(kind=c_char), dimension(*), optional :: tooltip
    integer(kind=c_int), dimension(:), optional :: ssize
    type(c_ptr), intent(out), optional :: buffer

    ! A multiline text edit widget
    !
    ! SCROLL: c_ptr: optional: A scrolled window in which the text editor
    ! 		is placed. If it is present, then it must be used used for packing the
    ! 		widget into your application. If it is not used, then scroll bars
    ! 		will not be added if the text goes beyond the edge of the box.
    ! EDITABLE: boolean: optional: Set to FALSE to make a non-editable text box.
    ! CHANGED: c_funptr: optional: Callback for the "activate" signal.
    ! DATA_CHANGED: c_ptr: optional: User data to pass to/from the activate
    ! 		callback
    ! INSERT_TEXT: c_funptr: optional: Callback for the "insert-text" signal.
    ! 		This handler is attached to the text buffer not the text view.
    ! DATA_INSERT_TEXT: c_ptr: optional: User data for the insert-text callback.
    ! DELETE_RANGE: c_funptr: optional: Callback for the "delete-range" signal.
    ! 		This handler is attached to the text buffer not the text view.
    ! DATA_DELETE_RANGE: c_ptr: optional: User data for the delete-range callback.
    ! INITIAL_TEXT: string(): optional: Initial text to put in the text window.
    ! SENSITIVE: boolean: optional: Set to FALSE to make the widget start in an
    ! 		insensitive state.
    ! TOOLTIP: string: optional: A tooltip to display when the pointer is
    ! 		held over the widget.
    ! SSIZE: c_int(2): optional: Size of the scroll widget.
    ! BUFFER: c_ptr: optional: Variable to return the buffer pointer
    !
    ! NOTE -- The insert-text and delete-range callbacks take extra arguments. They
    ! are called before the buffer is actually modified. The changed callback is called
    ! after the change.
    !-

    type(c_ptr) :: tbuf
    character(kind=c_char), dimension(:), allocatable :: text0
    type(gtktextiter), target :: iter

    tbuf = gtk_text_buffer_new(NULL)
    view = gtk_text_view_new_with_buffer(tbuf)

    if (present(scroll)) then
       scroll = gtk_scrolled_window_new(NULL, NULL)
       call gtk_scrolled_window_set_policy(scroll, GTK_POLICY_AUTOMATIC, &
            & GTK_POLICY_AUTOMATIC)
       if (present(ssize)) call gtk_widget_set_size_request(scroll, ssize(1), ssize(2))
       call gtk_container_add(scroll, view)
    else if (present(ssize)) then
       call gtk_widget_set_size_request(view, ssize(1), ssize(2))
    end if

    if (present(editable)) then
       call gtk_text_view_set_editable(view, editable)
    else
       call gtk_text_view_set_editable(view, TRUE)
    end if

    ! If there's an initial value, set it before binding the signals.
    if (present(initial_text)) then
       call convert_f_string(initial_text, text0)
       call gtk_text_buffer_get_start_iter(tbuf, c_loc(iter))
       call gtk_text_buffer_insert(tbuf, c_loc(iter), text0, -1)
       deallocate(text0)
    end if

    ! Attach the various signals
    if (present(changed)) then
       if (present(data_changed)) then
          call g_signal_connect(tbuf, "changed"//cnull, changed, &
               & data_changed)
       else
          call g_signal_connect(tbuf, "changed"//cnull, changed)
       end if
    end if
    if (present(insert_text)) then
       if (present(data_insert_text)) then
          call g_signal_connect(tbuf, "insert-text"//cnull, insert_text, &
               & data_insert_text)
       else
          call g_signal_connect(tbuf, "insert-text"//cnull, insert_text)
       end if
    end if
    if (present(delete_range)) then
       if (present(data_delete_range)) then
          call g_signal_connect(tbuf, "delete-range"//cnull, delete_range, &
               & data_delete_range)
       else
          call g_signal_connect(tbuf, "delete-range"//cnull, delete_range)
       end if
    end if

    if (present(sensitive)) call gtk_widget_set_sensitive(view, sensitive)
    if (present(tooltip)) call gtk_widget_set_tooltip_text(view, tooltip)
    if (present(buffer)) buffer = tbuf

  end function hl_gtk_text_view_new

  !+
  subroutine hl_gtk_text_view_insert(view, text, line, column, replace, &
       & at_cursor, buffer)

    type(c_ptr), intent(in) :: view
    character(len=*), dimension(:), intent(in) :: text
    integer(kind=c_int), optional, intent(in) :: line, column
    integer(kind=c_int), optional, intent(in) :: replace, at_cursor
    type(c_ptr), intent(in), optional :: buffer

    ! Insert text to an text view
    !
    ! VIEW: c_ptr: required: The text view into which to insert.
    ! TEXT: string(): required: The text to insert.
    ! LINE: c_int: optional: The line at which to insert (if omitted,
    ! 		then the text is appended).
    ! COLUMN: c_int: optional: The column as which to insert the text
    ! 		(If omitted, then insert at the start of the line).
    ! REPLACE: boolean: optional: If set to TRUE and LINE and COLUMN are omitted
    ! 		then replace the text in the buffer.
    ! AT_CURSOR: boolean: optional: Set to TRUE to insert the text at the
    ! 		cursor.
    ! BUFFER: c_ptr: optional: The text buffer in which to insert the text
    ! 		If this is given, then VIEW is ignored -- used in signal
    ! 		handlers attached to the buffer.
    !-

    type(c_ptr) :: tbuf
    type(gtktextiter), target :: iter
    integer(kind=c_int) :: icol, irep, atc
    character(kind=c_char), dimension(:), allocatable :: text0

    if (present(buffer)) then
       tbuf = buffer
    else
       tbuf= gtk_text_view_get_buffer(view)
    end if

    call convert_f_string(text, text0)

    ! Check if we are going to insert at the cursor, and if so do so.
    if (present(at_cursor)) then
       atc = at_cursor
    else
       atc = FALSE
    end if

    if (atc == TRUE) then
       call gtk_text_buffer_insert_at_cursor(tbuf, text0, -1)
       deallocate(text0)
       return
    end if

    if (present(line)) then
       if (present(column)) then
          icol = column
       else
          icol = 0
       end if
       if (present(replace)) then
          call hl_gtk_text_view_delete(NULL, line=line, column=icol, &
               & n_chars=size(text0), buffer=tbuf)
       end if
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(iter), &
            & line, column)
    else
       if (present(replace)) then
          irep = replace
       else
          irep = FALSE
       end if
       if (irep == TRUE) then
          call gtk_text_buffer_set_text(tbuf, text0, -1)
          deallocate(text0)
          return
       end if
       call gtk_text_buffer_get_end_iter(tbuf, c_loc(iter))
    end if

    call gtk_text_buffer_insert(tbuf, c_loc(iter), text0, -1)
    deallocate(text0)
  end subroutine hl_gtk_text_view_insert

  !+
  subroutine hl_gtk_text_view_delete(view, line, column, n_chars, n_lines, &
       & buffer)

    type(c_ptr), intent(in) :: view
    integer(kind=c_int), intent(in), optional :: line, column, n_chars, n_lines
    type(c_ptr), intent(in), optional :: buffer

    ! Delete from a text view
    !
    ! VIEW: c_ptr: required: The text view from which to delete.
    ! LINE: c_int: optional: The line at which to start the deletion
    ! COLUMN: c_int: optional: The column at which to start the deletion.
    ! 		required if N_CHARS is given. Ignored if N_LINES is given.
    ! N_CHARS: c_int: optional: How many characters to delete.
    ! N_LINES: c_int: optional: How many lines to delete.
    ! BUFFER: c_ptr: optional: The text buffer from which to delete. If this
    ! 		is given, then VIEW is ignored, used in signal handlers
    ! 		attached to the buffer.
    !
    ! If no location specifiers are given then the buffer is cleared
    !-

    type(c_ptr) :: tbuf
    type(gtktextiter), target :: s_iter, e_iter
    integer(kind=c_int) :: isok

    ! Input checking
    if (present(n_chars) .and. present(n_lines)) then
       write(0, *) &
            & "hl_gtk_text_view_delete:: May not specify both N_CHARS and N_LINES"
       return
    end if

    if (present(n_chars) .and. .not. present(column)) then
       write(0, *) &
            & "hl_gtk_text_view_delete:: Character delete requires a start column"
       return
    end if

    ! Find the buffer

    if (present(buffer)) then
       tbuf = buffer
    else
       tbuf = gtk_text_view_get_buffer(view)
    end if

    if (present(n_chars)) then
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(s_iter), &
            & line, column)
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(e_iter), &
            & line, column)
       isok = gtk_text_iter_forward_chars(c_loc(e_iter), n_chars)
    else if (present(n_lines)) then
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(s_iter), line)
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(e_iter), line)
       isok = gtk_text_iter_forward_lines(c_loc(e_iter), n_lines)
    else
       call gtk_text_buffer_get_start_iter(tbuf, c_loc(s_iter))
       call gtk_text_buffer_get_end_iter(tbuf, c_loc(e_iter))
    end if

    call gtk_text_buffer_delete(tbuf, c_loc(s_iter), c_loc(e_iter))
  end subroutine hl_gtk_text_view_delete

  !+
  subroutine hl_gtk_text_view_get_text(view, text, start_line, start_column, &
       & end_line, end_column, hidden, buffer)

    type(c_ptr), intent(in) :: view
    character(len=*), dimension(:), allocatable, intent(out) :: text
    integer(kind=c_int), intent(in), optional :: start_column, start_line, &
         & end_line, end_column
    integer(kind=c_int), intent(in), optional :: hidden
    type(c_ptr), intent(in), optional :: buffer

    ! Get text from s text view.
    !
    ! VIEW: c_ptr: required: The text view to read.
    ! TEXT: string(): required: A variable to contain the output text.
    ! START_LINE: c_int: optional: The first line to read.
    ! START_COLUMN: c_int: optional: The column at which to start reading.
    ! END_LINE: c_int: optional: The last line to read.
    ! END_COLUMN: c_int: optional: The column at which to stop reading.
    ! HIDDEN: boolean: optional: If set to FALSE, then do not get hidden
    ! 		characters
    ! BUFFER: c_ptr: optional: The text buffer from which to read. If this
    ! 		is given, then VIEW is ignored, useful for signal handlers
    ! 		attached to the buffer.
    !
    ! Note the rules for selection.
    !
    ! * If no selection arguments are present, the whole text is returned.
    ! * If either start_column or end_column is absent, but the matching line
    ! is present, then selection is by line.
    ! * If end_line is absent, but both columns are present, then the selection
    ! is within start_line
    ! * If neither start_line nor start_column is present, then the selection is
    ! from the start of the buffer
    ! * If neither end_line nor end_column is present, then the selection is
    ! to the end of the buffer.
    !-

    type(c_ptr) :: tbuf, ctext0
    character(kind=c_char), dimension(:), pointer :: ftext0
    type(gtktextiter), target :: s_iter, e_iter
    integer(kind=c_int) :: ihid
    integer(kind=c_int) :: nchars_r

    if (present(buffer)) then
       tbuf = buffer
    else
       tbuf = gtk_text_view_get_buffer(view)
    end if

    ! Fully specified
    if (present(start_line) .and. present(start_column) .and. &
         & present(end_line) .and. present(end_column)) then
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(s_iter), &
            & start_line, start_column)
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(e_iter), &
            & end_line, end_column)

       ! Both columns only start line
    else if (present(start_line) .and. present(start_column) .and. &
         &  present(end_column)) then
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(s_iter), &
            & start_line, start_column)
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(e_iter), &
            & start_line, end_column)

       ! Both lines, at least one column not given
    else if (present(start_line) .and. present(start_column)) then
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(s_iter), &
            & start_line)
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(e_iter), &
            & end_line)

       ! Fully specified start, no end
    else if (present(start_line) .and. present(start_column)) then
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(s_iter), &
            & start_line, start_column)
       call gtk_text_buffer_get_end_iter(tbuf, c_loc(e_iter))

       ! Start line only
    else if (present(start_line)) then
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(s_iter), &
            & start_line)
       call gtk_text_buffer_get_end_iter(tbuf, c_loc(e_iter))

       ! Fully specified end, no start
    else if (present(end_line) .and. present(end_column)) then
       call gtk_text_buffer_get_start_iter(tbuf, c_loc(s_iter))
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(e_iter), &
            & start_line, end_column)

       ! End line only
    else if (present(end_line)) then
       call gtk_text_buffer_get_start_iter(tbuf, c_loc(s_iter))
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(e_iter), &
            & end_line)

       ! Should only get here with nothing specified
    else
       call gtk_text_buffer_get_start_iter(tbuf, c_loc(s_iter))
       call gtk_text_buffer_get_end_iter(tbuf, c_loc(e_iter))
    end if

    if (present(hidden)) then
       ihid = hidden
    else
       ihid = TRUE
    end if
    ctext0 = gtk_text_buffer_get_text(tbuf, c_loc(s_iter), c_loc(e_iter), ihid)
    nchars_r = gtk_text_iter_get_offset(c_loc(e_iter)) - &
         & gtk_text_iter_get_offset(c_loc(s_iter)) + 1

    call c_f_pointer(ctext0, ftext0, (/ nchars_r /))
    call convert_c_string(ftext0, text)

  end subroutine hl_gtk_text_view_get_text

  !+
  function hl_gtk_text_view_get_cursor(view, buffer) result(ipos)

    integer(kind=c_int), dimension(3) :: ipos
    type(c_ptr), intent(in) :: view
    type(c_ptr), intent(in), optional :: buffer

    ! Get the current cursor location
    !
    ! VIEW: c_ptr: required: The text view to query
    ! BUFFER: c_ptr: optional: The buffer to query (if given, then
    ! 		VIEW is ignored).
    !
    ! Returns a 3-element array with the line, column and offset of the cursor
    !-

    type(c_ptr) :: tbuf, mark
    type(gtktextiter), target :: iter

    if (present(buffer)) then
       tbuf = buffer
    else
       tbuf = gtk_text_view_get_buffer(view)
    end if

    mark = gtk_text_buffer_get_insert(tbuf)
    call gtk_text_buffer_get_iter_at_mark(tbuf, c_loc(iter), mark)
    ipos(1) = gtk_text_iter_get_line(c_loc(iter))
    ipos(2) = gtk_text_iter_get_line_offset(c_loc(iter))
    ipos(3) = gtk_text_iter_get_offset(c_loc(iter))
  end function hl_gtk_text_view_get_cursor

  !+
  function hl_gtk_text_view_get_selection(view, s_start, s_end, buffer) &
       & result(issel)

    integer(kind=c_int) :: issel
    type(c_ptr), intent(in) :: view
    integer(kind=c_int), dimension(3), intent(out) :: s_start, s_end
    type(c_ptr), intent(in), optional :: buffer

    ! Get the selection range
    !
    ! VIEW: c_ptr: required: The text view to query.
    ! S_START: c_int(): required: The start of the selection. (line, column, offset)
    ! S_END: c_int(): required: The end of the selection. (line, column, offset)
    ! BUFFER: c_ptr: optional: The text buffer to query. If present, then the
    ! 		view argument is ignored.
    !
    ! Returns TRUE if there is a selection, FALSE if there isn't
    !-

    type(c_ptr) :: tbuf
    type(gtktextiter), target :: s_iter, e_iter

    if (present(buffer)) then
       tbuf = buffer
    else
       tbuf = gtk_text_view_get_buffer(view)
    end if

    issel = gtk_text_buffer_get_selection_bounds(tbuf, c_loc(s_iter), &
         & c_loc(e_iter))

    if (issel == FALSE) then ! No selection
       s_start(:) = -1
       s_end(:) = -1
    else
       s_start(1) = gtk_text_iter_get_line(c_loc(s_iter))
       s_start(2) = gtk_text_iter_get_line_offset(c_loc(s_iter))
       s_start(3) = gtk_text_iter_get_offset(c_loc(s_iter))
       s_end(1) = gtk_text_iter_get_line(c_loc(e_iter))
       s_end(2) = gtk_text_iter_get_line_offset(c_loc(e_iter))
       s_end(3) = gtk_text_iter_get_offset(c_loc(e_iter))
    end if
  end function hl_gtk_text_view_get_selection

  !+
  function hl_gtk_text_view_get_modified(view) result(ismod)

    integer(kind=c_int) :: ismod
    type(c_ptr), intent(in) :: view

    ! Check if the buffer of a text view is modified
    !
    ! VIEW: c_ptr: required: The text view to check.
    !
    ! N.B. No BUFFER argument is provided as gtk_text_buffer_get_modified
    ! is just a single call
    !-

    type(c_ptr) :: tbuf

    tbuf = gtk_text_view_get_buffer(view)
    ismod = gtk_text_buffer_get_modified(tbuf)

  end function hl_gtk_text_view_get_modified

  !+
  subroutine hl_gtk_text_view_set_modified(view, state)

    type(c_ptr), intent(in) :: view
    integer(kind=c_int), intent(in) :: state

    ! Set/clear the modified flag on the text buffer of a text view
    !
    ! VIEW: c_ptr: required: The text view to set
    ! STATE: boolean: required: The state to set the flag to.
    !-

    type(c_ptr) :: tbuf

    tbuf = gtk_text_view_get_buffer(view)
    call gtk_text_buffer_set_modified(tbuf, state)

  end subroutine hl_gtk_text_view_set_modified

  !+
  subroutine hl_gtk_text_view_get_info(view, nlines, nchars, ncline, buffer)

    type(c_ptr), intent(in) :: view
    integer(kind=c_int), intent(out), optional :: nlines, nchars
    integer(kind=c_int), intent(out), optional, allocatable, dimension(:) :: ncline
    type(c_ptr), intent(in), optional :: buffer

    ! Get various useful information about a text view
    !
    ! VIEW: c_ptr: required: The view to query
    ! NLINES: c_int: optional: Return the number of lines in the view
    ! NCHARS: c_int: optional: Return the number of characters in the view
    ! NCLINE: c_int(): optional: Return the nuber of characters in each
    ! 		line. Must be an allocatable array.
    ! BUFFER: c_ptr: optional: If present use this buffer and ignore the
    ! 		VIEW argument
    !-

    type(c_ptr) :: tbuf
    type(gtktextiter), target :: i1, i2
    integer(kind=c_int) :: nl
    integer(kind=c_int) :: i
    if (present(buffer)) then
       tbuf = buffer
    else
       tbuf = gtk_text_view_get_buffer(view)
    end if

    if (present(nlines) .or. present(ncline)) &
         &  nl = gtk_text_buffer_get_line_count(tbuf)
    if (present(nlines)) nlines = nl

    if (present(nchars)) &
         & nchars = gtk_text_buffer_get_char_count(tbuf)

    if (present(ncline)) then
       allocate(ncline(nl))
       call gtk_text_buffer_get_start_iter(tbuf, c_loc(i1))
       do i = 1, nl-1
          call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(i2), i)
          ncline(i) = gtk_text_iter_get_offset(c_loc(i2)) - &
               & gtk_text_iter_get_offset(c_loc(i1))-1
          i1 = i2
       end do
       call gtk_text_buffer_get_end_iter(tbuf, c_loc(i2))
       ncline(nl) = gtk_text_iter_get_offset(c_loc(i2)) - &
               & gtk_text_iter_get_offset(c_loc(i1))
    end if
  end subroutine hl_gtk_text_view_get_info

  !*
  ! Lists and Trees
  ! These functions attempt to hide some of the complexity of the GtkTreeView
  ! system of widgets and object, while still allowing the main functionality
  ! to be accessed. Only "text" displays are supported.
  !
  ! There are three types.
  ! 1. listn; A multi-column flat list. At present, there
  ! is no support for determining selected columns, or for editing the cell
  ! contents, because to date I've not been able to decipher the documentation
  ! on how to do it.
  ! 2. list1; A single-column flat list, that allows only string values. (This is
  !    now implemented nbby calls to the corresponding listn routines).
  ! 3. tree; A tree view (similar to listn but with child rows).
  !/

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
       write(0,*) "hl_gtk_listn_new: Cannot determine the number of columns"
       list = NULL
       scroll=NULL
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
          write(0,*) "hl_gtk_listn_new: EDITABLE requires COLNOS"
          list=NULL
          scroll=NULL
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
    scroll = gtk_scrolled_window_new(NULL, NULL)
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
          call g_object_set_property(renderer, "editable"//cnull, pisedit)
          if (editable(i) == TRUE) then
             call g_object_set_data(renderer, "column-number"//cnull, &
                  & c_loc(colnos(i)))
             call g_object_set_data(renderer, "view"//cnull, list)
             if (present(edited)) then
                if (present(data_edited)) then
                   call g_signal_connect(renderer, "edited"//cnull, &
                        & edited, data_edited)
                else
                   call g_signal_connect(renderer, "edited"//cnull, &
                        & edited)
                end if
             else
                call g_signal_connect(renderer, "edited"//cnull, &
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
            &trim(titles(i))//cnull)
       call gtk_tree_view_column_add_attribute(column, renderer, &
            & "text"//CNULL, i-1)
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
          call g_signal_connect(select, "changed"//cnull, &
               & changed, data)
       else
          call g_signal_connect(select, "changed"//cnull, changed)
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
    pcol = g_object_get_data(renderer, "column-number"//cnull)
    call c_f_pointer(pcol, icol)
    call convert_c_string(text, 200, ftext)
    list = g_object_get_data(renderer, "view"//cnull)

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
       valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), NULL, row)
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
    call g_list_foreach(slist, c_funloc(gtk_tree_path_free), NULL)
    call g_list_free(slist)

  end function hl_gtk_listn_get_selections


  !+
  subroutine  hl_gtk_listn_set_selection(list, row)

    type(c_ptr), intent(in) :: list
    integer(kind=c_int), intent(in), optional :: row

    ! Set the selected row in a list (single row only).
    !
    ! LIST: c_ptr: required: The list to work on.
    ! ROW: c_int: optional: The row to select (absent or < 0 is clear
    ! selection)
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
       valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), NULL, row)
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
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), NULL, row)
    if (valid == FALSE) return

    ! Set up the GValue to the right type.
    val = c_loc(value)
    val = g_value_init(val, ctype)

    ! Select according to the cell type
    select case(ctype)
    case(G_TYPE_CHAR)
       if (present(svalue)) then
          call g_value_set_char(val, svalue(1:1))
       else if (present(ivalue)) then
          call g_value_set_char(val, char(ivalue, c_char))
       else if (present(lvalue)) then
          call g_value_set_char(val, char(lvalue, c_char))
       else if (present(l64value)) then
          call g_value_set_char(val, char(l64value, c_char))
       else
          write(0,*) "hl_gtk_listn_set_cell:: Cannot make a 'char' type from given value(s)"
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
          write(0,*) "hl_gtk_listn_set_cell:: Cannot make a 'char' type from given value(s)"
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
             write(0,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'int'"
             return
          end if
          call g_value_set_int(val, iconv)
       else
          write(0,*) "hl_gtk_listn_set_cell:: Cannot make an 'int' type from given value(s)"
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
             write(0,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'int'"
             return
          end if
          call g_value_set_uint(val, iconv)
       else
          write(0,*) "hl_gtk_listn_set_cell:: Cannot make an 'int' type from given value(s)"
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
                write(0,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'int'"
                return
             end if
             call g_value_set_boolean(val, iconv)
          end if
       else
          write(0,*) "hl_gtk_listn_set_cell:: Cannot make an 'int' type from given value(s)"
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
             write(0,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'long'"
             return
          end if
          call g_value_set_long(val, lconv)
       else
          write(0,*) "hl_gtk_listn_set_cell:: Cannot make a 'long' type from given value(s)"
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
             write(0,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'long'"
             return
          end if
          call g_value_set_ulong(val, lconv)
       else
          write(0,*) "hl_gtk_listn_set_cell:: Cannot make a 'long' type from given value(s)"
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
             write(0,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'int64'"
             return
          end if
          call g_value_set_int64(val, l64conv)
       else
          write(0,*) "hl_gtk_listn_set_cell:: Cannot make an 'int64' type from given value(s)"
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
             write(0,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'int64'"
             return
          end if
          call g_value_set_uint64(val, l64conv)
       else
          write(0,*) "hl_gtk_listn_set_cell:: Cannot make an 'int64' type from given value(s)"
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
             write(0,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'float'"
             return
          end if
          call g_value_set_float(val, fconv)
       else
          write(0,*) "hl_gtk_listn_set_cell:: Cannot make a 'float' type from given value(s)"
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
             write(0,*) "hl_gtk_listn_set_cell:: Failed to convert string to 'double'"
             return
          end if
          call g_value_set_double(val, dconv)
       else
          write(0,*) "hl_gtk_listn_set_cell:: Cannot make a 'double' type from given value(s)"
          return
       end if

    case (G_TYPE_STRING)
       if (present(svalue)) then
          call g_value_set_string(val, trim(svalue)//cnull)
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
             write(0,*) "hl_gtk_listn_set_cell:: Cannot make a 'string' type from given value(s)"
             return
          end if
          call g_value_set_string(val, trim(sconv)//cnull)
       end if

    case default
       write(0,*)  "hl_gtk_listn_set_cell:: Cell type ",ctype," is unknown"
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
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), NULL, row)
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
          svalue(1:1) = g_value_get_char(val)
       else if (present(ivalue)) then
          ivalue = ichar(g_value_get_char(val))
       else if (present(lvalue)) then
          lvalue = ichar(g_value_get_char(val))
       else if (present(l64value)) then
          l64value = ichar(g_value_get_char(val))
       else
          write(0,*) "hl_gtk_listn_get_cell:: Cannot return 'char' type to any available output"
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
          write(0,*) "hl_gtk_listn_get_cell:: Cannot return 'char' type to any available output"
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
          write(0,*) "hl_gtk_listn_get_cell:: Cannot return 'int' type to any available output"
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
          write(0,*) "hl_gtk_listn_get_cell:: Cannot return 'int' type to any available output"
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
          write(0,*) "hl_gtk_listn_get_cell:: Cannot return 'bool' type to any available output"
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
          write(0,*) "hl_gtk_listn_get_cell:: Cannot return 'long' type to any available output"
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
          write(0,*) "hl_gtk_listn_get_cell:: Cannot return 'long' type to any available output"
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
          write(0,*) "hl_gtk_listn_get_cell:: Cannot return 'int64' type to any available output"
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
          write(0,*) "hl_gtk_listn_get_cell:: Cannot return 'int64' type to any available output"
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
          write(0,*) "hl_gtk_listn_get_cell:: Cannot return 'float' type to any available output"
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
          write(0,*) "hl_gtk_listn_get_cell:: Cannot return 'double' type to any available output"
          return
       end if

    case (G_TYPE_STRING)
       if (present(svalue)) then
          cstr = g_value_get_string(val)
          call convert_c_string(cstr, len(svalue), svalue)
       else
          write(0,*) "hl_gtk_listn_get_cell:: Cannot return 'string' type to any available output"
       end if

    case default
       write(0,*)  "hl_gtk_listn_get_cell:: Cell type ",ctype," is unknown"
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
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter1), NULL, row1)
    if (valid == FALSE) return
    ! And of the target location
    if (present(row2)) then
       valid = gtk_tree_model_iter_nth_child(store, c_loc(iter2), NULL, row2)
       if (valid == FALSE) return
    end if

    ! Move it
    if (isafter == TRUE) then
       if (present(row2)) then
          call gtk_list_store_move_after(store, c_loc(iter1), c_loc(iter2))
       else
          call gtk_list_store_move_before(store, c_loc(iter1), NULL)
       end if
    else
       if (present(row2)) then
          call gtk_list_store_move_before(store, c_loc(iter1), c_loc(iter2))
       else
          call gtk_list_store_move_after(store, c_loc(iter1), NULL)
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
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter1), NULL, row1)
    if (valid == FALSE) return
    ! And of the second
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter2), NULL, row2)
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
    nrows = gtk_tree_model_iter_n_children(store, NULL)

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
    ! 		to do the rendering (see: GtkTreeCellDataFunc, for
    ! 		details). Omit or set to FNULL to remove a function.
    ! DATA: c_ptr: optional: User data to pass to the function.
    ! DESTROY_NOTIFY: c_funptr: optional: A destroy notify subroutine.
    !-

    type(c_funptr) :: funpass, destpass
    type(c_ptr) :: datapass

    type(c_ptr) :: col, renderer, rlist

    if (present(func)) then
       funpass = func
    else
       funpass = FNULL
    end if

    if (present(data)) then
       datapass = data
    else
       datapass = NULL
    end if

    if (present(destroy_notify)) then
       destpass = destroy_notify
    else
       destpass = FNULL
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
       irow=gtk_tree_model_iter_n_children (store, NULL)-1
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
    ! ROW: c_int: optional: The row to select (absent or < 0 is clear selection)
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
    ! 		to do the rendering (see: GtkTreeCellDataFunc, for
    ! 		details). Omit or set to FNULL to remove a function.
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
       write(0,*) "hl_gtk_tree_new: Cannot determine the number of columns"
       tree = NULL
       scroll=NULL
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
          write(0,*) "hl_gtk_listn_new: EDITABLE requires COLNOS"
          tree=NULL
          scroll=NULL
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
    scroll = gtk_scrolled_window_new(NULL, NULL)
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
          call g_object_set_property(renderer, "editable"//cnull, pisedit)
          if (editable(i) == TRUE) then
             call g_object_set_data(renderer, "column-number"//cnull, &
                  & c_loc(colnos(i)))
             call g_object_set_data(renderer, "view"//cnull, tree)
             if (present(edited)) then
                if (present(data_edited)) then
                   call g_signal_connect(renderer, "edited"//cnull, &
                        & edited, data_edited)
                else
                   call g_signal_connect(renderer, "edited"//cnull, &
                        & edited)
                end if
             else
                call g_signal_connect(renderer, "edited"//cnull, &
                     & c_funloc(hl_gtk_tree_edit_cb))
             endif
          end if
       end if

       column = gtk_tree_view_column_new()
       call gtk_tree_view_column_pack_start(column, renderer, FALSE)
       if (present(titles)) call gtk_tree_view_column_set_title(column, &
            &trim(titles(i))//cnull)
       call gtk_tree_view_column_add_attribute(column, renderer, &
            & "text"//CNULL, i-1)
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
          call g_signal_connect(select, "changed"//cnull, changed, data)
       else
          call g_signal_connect(select, "changed"//cnull, changed)
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
    pcol = g_object_get_data(renderer, "column-number"//cnull)
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
    tree = g_object_get_data(renderer, "view"//cnull)

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
             call gtk_tree_store_append(store, c_loc(iter1), NULL)
          else
             call gtk_tree_store_insert(store, c_loc(iter1), NULL, row(1))
          end if
       else
          do i = 1, size(row)-1
             if (i == 1) then
                valid=gtk_tree_model_iter_nth_child(store, c_loc(iter1), &
                     & NULL, row(1))
             else
                valid=gtk_tree_model_iter_nth_child(store, c_loc(iter1), &
                     & c_loc(iter2), row(i))
             end if
             if (valid == FALSE) then
                write(0,*) "hl_gtk_tree_ins:: Row description does not point to an insertable location"
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
          call gtk_tree_store_append(store, c_loc(iter1), NULL)
       else if (absrow == 0) then
          call gtk_tree_store_prepend(store, c_loc(iter1), NULL)
       else
          valid = hl_gtk_tree_abs_iter(tree, iter1, absrow)
          if (valid == FALSE) then
             write(0,*) "hl_gtk_tree_ins:: Row description does not point to an insertable location"
             return
          end if
          call clear_gtktreeiter(iter2)
          call gtk_tree_store_insert_before(store, c_loc(iter2), NULL, &
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
    ! MODEL: c_ptr: optional: The tree model (if this is givem then TREE is
    ! 		ignored
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
    ! MODEL: c_ptr: optional: The tree model (if this is givem then TREE is
    ! 		ignored
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
    valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), NULL, row(1))

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
       write(0,*) "hl_gtk_tree_rem: Specified row does not exist"
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
    call g_list_foreach(slist, c_funloc(gtk_tree_path_free), NULL)
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
       valid = hl_gtk_tree_row_iter(NULL, iter, row, model=store)
    else if (present(absrow)) then
       valid = hl_gtk_tree_abs_iter(NULL, iter, absrow, model=store)
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
          call g_value_set_char(val, svalue(1:1))
       else if (present(ivalue)) then
          call g_value_set_char(val, char(ivalue, c_char))
       else if (present(lvalue)) then
          call g_value_set_char(val, char(lvalue, c_char))
       else if (present(l64value)) then
          call g_value_set_char(val, char(l64value, c_char))
       else
          write(0,*) "hl_gtk_tree_set_cell:: Cannot make a 'char' type from given value(s)"
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
          write(0,*) "hl_gtk_tree_set_cell:: Cannot make a 'char' type from given value(s)"
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
             write(0,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'int'"
             return
          end if
          call g_value_set_int(val, iconv)
       else
          write(0,*) "hl_gtk_tree_set_cell:: Cannot make an 'int' type from given value(s)"
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
             write(0,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'int'"
             return
          end if
          call g_value_set_uint(val, iconv)
       else
          write(0,*) "hl_gtk_tree_set_cell:: Cannot make an 'int' type from given value(s)"
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
                write(0,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'int'"
                return
             end if
             call g_value_set_boolean(val, iconv)
          end if
       else
          write(0,*) "hl_gtk_tree_set_cell:: Cannot make an 'int' type from given value(s)"
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
             write(0,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'long'"
             return
          end if
          call g_value_set_long(val, lconv)
       else
          write(0,*) "hl_gtk_tree_set_cell:: Cannot make a 'long' type from given value(s)"
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
             write(0,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'long'"
             return
          end if
          call g_value_set_ulong(val, lconv)
       else
          write(0,*) "hl_gtk_tree_set_cell:: Cannot make a 'long' type from given value(s)"
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
             write(0,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'int64'"
             return
          end if
          call g_value_set_int64(val, l64conv)
       else
          write(0,*) "hl_gtk_tree_set_cell:: Cannot make an 'int64' type from given value(s)"
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
             write(0,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'int64'"
             return
          end if
          call g_value_set_uint64(val, l64conv)
       else
          write(0,*) "hl_gtk_tree_set_cell:: Cannot make an 'int64' type from given value(s)"
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
             write(0,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'float'"
             return
          end if
          call g_value_set_float(val, fconv)
       else
          write(0,*) "hl_gtk_tree_set_cell:: Cannot make a 'float' type from given value(s)"
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
             write(0,*) "hl_gtk_tree_set_cell:: Failed to convert string to 'double'"
             return
          end if
          call g_value_set_double(val, dconv)
       else
          write(0,*) "hl_gtk_tree_set_cell:: Cannot make a 'double' type from given value(s)"
          return
       end if

    case (G_TYPE_STRING)
       if (present(svalue)) then
          call g_value_set_string(val, trim(svalue)//cnull)
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
             write(0,*) "hl_gtk_tree_set_cell:: Cannot make a 'string' type from given value(s)"
             return
          end if
          call g_value_set_string(val, trim(sconv)//cnull)
       end if

    case default
       write(0,*)  "hl_gtk_tree_set_cell:: Cell type ",ctype," is unknown"
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
       valid = hl_gtk_tree_row_iter(NULL, iter, row, model=store)
    else if (present(absrow)) then
       valid = hl_gtk_tree_abs_iter(NULL, iter, absrow, model=store)
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
          svalue(1:1) = g_value_get_char(val)
       else if (present(ivalue)) then
          ivalue = ichar(g_value_get_char(val))
       else if (present(lvalue)) then
          lvalue = ichar(g_value_get_char(val))
       else if (present(l64value)) then
          l64value = ichar(g_value_get_char(val))
       else
          write(0,*) "hl_gtk_tree_get_cell:: Cannot return 'char' type to any available output"
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
          write(0,*) "hl_gtk_tree_get_cell:: Cannot return 'char' type to any available output"
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
          write(0,*) "hl_gtk_tree_get_cell:: Cannot return 'int' type to any available output"
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
          write(0,*) "hl_gtk_tree_get_cell:: Cannot return 'int' type to any available output"
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
          write(0,*) "hl_gtk_tree_get_cell:: Cannot return 'bool' type to any available output"
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
          write(0,*) "hl_gtk_tree_get_cell:: Cannot return 'long' type to any available output"
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
          write(0,*) "hl_gtk_tree_get_cell:: Cannot return 'long' type to any available output"
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
          write(0,*) "hl_gtk_tree_get_cell:: Cannot return 'int64' type to any available output"
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
          write(0,*) "hl_gtk_tree_get_cell:: Cannot return 'int64' type to any available output"
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
          write(0,*) "hl_gtk_tree_get_cell:: Cannot return 'float' type to any available output"
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
          write(0,*) "hl_gtk_tree_get_cell:: Cannot return 'double' type to any available output"
          return
       end if

    case (G_TYPE_STRING)
       if (present(svalue)) then
          cstr = g_value_get_string(val)
          call convert_c_string(cstr, len(svalue), svalue)
       else
          write(0,*) "hl_gtk_tree_get_cell:: Cannot return 'string' type to any available output"
       end if

    case default
       write(0,*)  "hl_gtk_tree_get_cell:: Cell type ",ctype," is unknown"
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
    ! 		to do the rendering (see: GtkTreeCellDataFunc, for
    ! 		details). Omit or set to FNULL to remove a function.
    ! DATA: c_ptr: optional: User data to pass to the function.
    ! DESTROY_NOTIFY: c_funptr: optional: A destroy notify subroutine.
    !-

    call hl_gtk_listn_set_cell_data_func(list, colno, func, &
         & data, destroy_notify)
  end subroutine hl_gtk_tree_set_cell_data_func

  !*
  ! Pulldown Menu
  ! Implements the GtkMenuBar menu system.
  !/
  !+
  function hl_gtk_menu_new(orientation) result(menu)

    type(c_ptr) :: menu
    integer(kind=c_int), intent(in), optional :: orientation

    ! Menu initializer (mainly for consistency)
    !
    ! ORIENTATION: integer: optional: Whether to lay out the top level
    ! 		horizontaly or vertically.
    !-

    integer(kind=c_int) :: orient
    if (present(orientation)) then
       orient= orientation
    else
       orient = GTK_PACK_DIRECTION_LTR
    end if

    menu = gtk_menu_bar_new()
    call gtk_menu_bar_set_pack_direction (menu, orient)

  end function hl_gtk_menu_new

  !+
  function hl_gtk_menu_submenu_new(menu, label, tooltip, pos) result(submenu)

    type(c_ptr) :: submenu
    type(c_ptr) :: menu
    character(kind=c_char), dimension(*), intent(in) :: label
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), optional :: pos

    ! Make a submenu node
    !
    ! MENU: c_ptr: required:  The parent of the submenu
    ! LABEL: string: required: The label of the submenu
    ! TOOLTIP: string: optional: A tooltip for the submenu.
    ! POS: integer: optional: The position at which to insert the item
    ! 		(omit to append)
    !-

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

  end function hl_gtk_menu_submenu_new

  !+
  function hl_gtk_menu_item_new(menu, label, activate, data, tooltip, &
       & pos, tearoff, sensitive, accel_key, accel_mods, accel_group, &
       & accel_flags) result(item)

    type(c_ptr) ::  item
    type(c_ptr) :: menu
    character(kind=c_char), dimension(*), intent(in), optional :: label
    type(c_funptr), optional :: activate
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), optional :: pos
    integer(kind=c_int), optional :: tearoff, sensitive
    character(kind=c_char), dimension(*), optional, intent(in) :: accel_key
    integer(kind=c_int), optional, intent(in) :: accel_mods, accel_flags
    type(c_ptr), optional, intent(in) :: accel_group
    
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
    ! TEAROFF: boolean: optional: Set to TRUE to make a tearoff point.
    ! SENSITIVE: boolean: optional: Set to FALSE to make the widget start in an
    ! 		insensitive state.
    ! ACCEL_KEY: string: optional: Set to the character value or code of a
    ! 		key to use as an accelerator.
    ! ACCEL_MODS: c_int: optional: Set to the modifiers for the accelerator.
    ! 		(If not given then GTK_CONTROL_MASK is assumed).
    ! ACCEL_GROUP: c_ptr: optional: The accelerator group to which the
    ! 		accelerator is attached, must have been added to the top-level
    ! 		window.
    ! ACCEL_FLAGS: c_int: optional: Flags for the accelerator, if not present
    ! 		then GTK_ACCEL_VISIBLE, is used (to hide the accelerator,
    ! 		use ACCEL_FLAGS=0).
    !-

    integer(kind=c_int) :: istear

    if (present(tearoff)) then
       istear = tearoff
    else
       istear = FALSE
    end if

    ! Create the menu item
    if (present(label)) then
       item = gtk_menu_item_new_with_label(label)
    else if (istear == TRUE) then
       item = gtk_tearoff_menu_item_new()
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
          write(0, *) "HL_GTK_MENU_ITEM: Cannot connect a callback to a separator"
          return
       end if

       if (present(data)) then
          call g_signal_connect(item, "activate"//cnull, activate, data)
       else
          call g_signal_connect(item, "activate"//cnull, activate)
       end if

       ! An accelerator
       if (present(accel_key) .and. present(accel_group)) &
            & call hl_gtk_widget_add_accelerator(item, "activate"//cnull, &
            & accel_group, accel_key, accel_mods, accel_flags)
    end if

    ! Attach a tooltip
    if (present(tooltip)) call gtk_widget_set_tooltip_text(item, tooltip)

    ! sensitive?
    if (present(sensitive)) call gtk_widget_set_sensitive(item, sensitive)

  end function hl_gtk_menu_item_new

  !+
  function hl_gtk_check_menu_item_new(menu, label, toggled, data, &
       & tooltip, pos, initial_state, sensitive)  result(item)

    type(c_ptr) ::  item
    type(c_ptr) :: menu
    character(kind=c_char), dimension(*), intent(in) :: label
    type(c_funptr), optional :: toggled
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), optional :: pos
    integer(kind=c_int), optional :: initial_state
    integer(kind=c_int), optional :: sensitive

    ! Make a menu item or separator
    !
    ! MENU: c_ptr: required: The parent menu.
    ! LABEL: string: required: The label for the menu.
    ! TOGGLED: c_funptr: optional: The callback function for the
    ! 		"toggled" signal
    ! DATA: c_ptr: optional: Data to pass to the callback.
    ! TOOLTIP: string: optional: A tooltip for the menu item.
    ! POS: integer: optional: The position at which to insert the item
    ! 		(omit to append)
    ! INITIAL_STATE: boolean: optional: Whether the item is initially selected.
    ! SENSITIVE: boolean: optional: Set to FALSE to make the widget start in an
    ! 		insensitive state.
    !-

    ! Create the menu item
    item = gtk_check_menu_item_new_with_label(label)

    ! Insert it to the parent
    if (present(pos)) then
       call gtk_menu_shell_insert(menu, item, pos)
    else
       call gtk_menu_shell_append(menu, item)
    end if

    ! Set the state
    if (present(initial_state)) &
         & call gtk_check_menu_item_set_active(item, initial_state)

    ! If present, connect the callback
    if (present(toggled)) then
       if (present(data)) then
          call g_signal_connect(item, "toggled"//cnull, toggled, data)
       else
          call g_signal_connect(item, "toggled"//cnull, toggled)
       end if
    end if

    ! Attach a tooltip
    if (present(tooltip)) call gtk_widget_set_tooltip_text(item, tooltip)
    ! sensitive?
    if (present(sensitive)) call gtk_widget_set_sensitive(item, sensitive)
  end function hl_gtk_check_menu_item_new

  !+
  function hl_gtk_radio_menu_item_new(group, menu, label, toggled, data, &
       & tooltip, pos, sensitive)  result(item)

    type(c_ptr) ::  group, item
    type(c_ptr) :: menu
    character(kind=c_char), dimension(*), intent(in) :: label
    type(c_funptr), optional :: toggled
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(kind=c_int), optional :: pos
    integer(kind=c_int), optional :: sensitive

    ! Make a menu item or separator
    !
    ! GROUP: c_ptr: required: The group for the radio item (NULL for a
    ! 		new group).
    ! MENU: c_ptr: required: The parent menu.
    ! LABEL: string: required: The label for the menu.
    ! TOGGLED: c_funptr: optional: The callback function for the
    ! 		"toggled" signal
    ! DATA: c_ptr: optional: Data to pass to the callback.
    ! TOOLTIP: string: optional: A tooltip for the menu item.
    ! POS: integer: optional: The position at which to insert the item
    ! 		(omit to append)
    ! SENSITIVE: boolean: optional: Set to FALSE to make the widget start in an
    ! 		insensitive state.
    !-

    ! Create the menu item
    item = gtk_radio_menu_item_new_with_label(group, label)
    group = gtk_radio_menu_item_get_group(item)

    ! Insert it to the parent
    if (present(pos)) then
       call gtk_menu_shell_insert(menu, item, pos)
    else
       call gtk_menu_shell_append(menu, item)
    end if

    ! If present, connect the callback
    if (present(toggled)) then
       if (present(data)) then
          call g_signal_connect(item, "toggled"//cnull, toggled, data)
       else
          call g_signal_connect(item, "toggled"//cnull, toggled)
       end if
    end if

    ! Attach a tooltip
    if (present(tooltip)) call gtk_widget_set_tooltip_text(item, tooltip)
    ! sensitive?
    if (present(sensitive)) call gtk_widget_set_sensitive(item, sensitive)

  end function hl_gtk_radio_menu_item_new

  subroutine hl_gtk_radio_menu_group_set_select(group, index)
    ! Set the indexth button of a radio menu group
    !
    ! GROUP: c_ptr: required: The group of the last button added to
    ! 		the radio menu
    ! INDEX: integer: required: The index of the button to set
    ! 		(starting from the first as 0).
    !-

    type(c_ptr), intent(in) :: group
    integer(kind=c_int), intent(in) :: index

    integer(kind=c_int) :: nbuts
    type(c_ptr) :: datan

    nbuts = g_slist_length(group)

    ! Note that GROUP actually points to the last button added and to the
    ! group of the next to last & so on

    datan= g_slist_nth_data(group, nbuts-index-1)
    call gtk_check_menu_item_set_active(datan, TRUE)

  end subroutine hl_gtk_radio_menu_group_set_select

  !*
  ! Progress Bar
  ! Implements the GtkProgressBar widget. Includes the facility to
  ! make a bar display "n of m" as well as the usual fraction.
  !/

  !+
  function hl_gtk_progress_bar_new(vertical, reversed, step) result(bar)

    type(c_ptr) :: bar
    integer(kind=c_int), optional :: vertical, reversed
    real(kind=c_double), optional :: step

    ! Intializer for a progress bar
    !
    ! ORIENTATION: integer: optional: The orientation of the bar.
    ! STEP: double: optional: The fractional step to advance when
    ! 		pulsing the bar
    !-

    integer(kind=c_int) :: orientation

    bar = gtk_progress_bar_new()

    ! GTK2 version
!!$2    orientation = GTK_PROGRESS_LEFT_TO_RIGHT
!!$2    if (present(vertical)) then
!!$2       if (vertical == TRUE) orientation = GTK_PROGRESS_BOTTOM_TO_TOP
!!$2       if (present(reversed)) then
!!$2          if (reversed == TRUE) orientation = GTK_PROGRESS_TOP_TO_BOTTOM
!!$2       end if
!!$2    else if (present(reversed)) then
!!$2       if (reversed == TRUE) orientation = GTK_PROGRESS_RIGHT_TO_LEFT
!!$2    end if
!!$2    call gtk_progress_bar_set_orientation(bar, orientation)
    ! end GTK2 version
    ! GTK3 version
    if (present(vertical)) then
       if (vertical == TRUE) then
          call gtk_orientable_set_orientation (bar, &
               & GTK_ORIENTATION_VERTICAL)
       else
          call gtk_orientable_set_orientation (bar, &
               & GTK_ORIENTATION_HORIZONTAL)
       end if
    end if

    if (present(reversed)) call gtk_progress_bar_set_inverted(bar, reversed)
    ! end GTK3 version

    if (present(step)) &
         & call gtk_progress_bar_set_pulse_step(bar, step)

  end function hl_gtk_progress_bar_new

  !+
  subroutine hl_gtk_progress_bar_set_f(bar, val, string, text)

    type(c_ptr) :: bar
    real(kind=c_double), optional :: val
    integer(kind=c_int), optional :: string
    character(len=*), intent(in), optional:: text

    ! Set the value of a progress bar (fraction or pulse)
    !
    ! BAR: c_ptr: required: The bar to set
    ! VAL: double: optional: The value to set. If absent, the bar is pulsed
    ! STRING: boolean: optional: Whether to put a string on the bar.
    ! TEXT: string: optional: Text to put in the bar, (overrides STRING)
    !
    ! This routine is normally accessed via the generic interface
    ! hl_gtk_progress_bar
    !-

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
       call gtk_progress_bar_set_text (bar, text//cnull)
! GTK3 Only
      call gtk_progress_bar_set_show_text(bar, TRUE)
! End GTK3 only
    else if (present(string)) then
       if (string == FALSE .or. .not. present(val)) return
       ! Otherwise we display a percentage
       write(sval, "(F5.1,'%')") val*100.

       call gtk_progress_bar_set_text (bar, trim(sval)//cnull)
! GTK3 Only
      call gtk_progress_bar_set_show_text(bar, TRUE)
    else
       call gtk_progress_bar_set_show_text(bar, FALSE)
! End GTK3 only
    end if
  end subroutine hl_gtk_progress_bar_set_f

  !+
  subroutine hl_gtk_progress_bar_set_ii(bar, val, maxv, string, text)

    type(c_ptr) :: bar
    integer(kind=c_int) :: val, maxv
    integer(kind=c_int), optional :: string
    character(len=*), intent(in), optional:: text

    ! Set the value of a progress bar (n of m)
    !
    ! BAR: c_ptr: required: The bar to set
    ! VAL: int: required: The value to set.
    ! MAXV: int: required: The maximum value for the bar
    ! STRING: boolean: optional: Whether to put a string on the bar.
    ! TEXT: string: optional: Text to put in the bar, (overrides STRING)
    !
    ! This routine is normally accessed via the generic interface
    ! hl_gtk_progress_bar
    !-

    real(kind=c_double) :: frac
    character(len=50) :: sval

    frac = real(val,c_double)/real(maxv,c_double)
    call gtk_progress_bar_set_fraction(bar, frac)

    ! If annotation is needed, add it.
    if (present(text)) then
       call gtk_progress_bar_set_text (bar, text//cnull)
! GTK3 Only
       call gtk_progress_bar_set_show_text(bar, TRUE)
! End GTK3 only
    else if (present(string)) then
       if (string == FALSE) return
       ! Otherwise we display n or m
       write(sval, "(I0,' of ',I0)") val, maxv
       call gtk_progress_bar_set_text (bar, trim(sval)//cnull)
! GTK3 Only
       call gtk_progress_bar_set_show_text(bar, TRUE)
    else
       call gtk_progress_bar_set_show_text(bar, FALSE)
! End GTK3 only
    end if
  end subroutine hl_gtk_progress_bar_set_ii

  !*
  ! Dialogue
  ! The message dialogue provided is here because, the built-in message
  ! dialogue GtkMessageDialog cannot be created without calling variadic
  ! functions which are not compatible with Fortran, therefore this is
  ! based around the plain GtkDialog family.
  !/

  !+
  function hl_gtk_message_dialog_show(message, button_set, title, type, &
       & parent) result(resp)

    integer(kind=c_int) :: resp
    character(len=*), dimension(:), intent(in) :: message
    integer(kind=c_int), intent(in) :: button_set
    character(kind=c_char), dimension(*), intent(in), optional :: title
    integer(kind=c_int), intent(in), optional :: type
    type(c_ptr), intent(in), optional :: parent

    ! A DIY version of the message dialogue, needed because both creators
    ! for the built in one are variadic and so not callable from Fortran.
    !
    ! MESSAGE: string(n): required: The message to display. Since this is
    ! 		a string array, the CNULL terminations are provided internally
    ! BUTTON_SET: integer: required: The set of buttons to display
    ! TITLE: string: optional: Title for the window.
    ! TYPE: c_int: optional: Message type (a GTK_MESSAGE_ value)
    ! PARENT: c_ptr: optional: An optional parent for the dialogue.
    !
    ! The return value is the response code, not the widget.
    !-

    type(c_ptr) :: dialog, content, junk, hb, vb
    integer :: i
    integer(kind=c_int) :: itype

    ! Create the dialog window and make it modal.

    dialog=gtk_dialog_new()
    call gtk_window_set_modal(dialog, TRUE)
    if (present(title)) call gtk_window_set_title(dialog, title)

    if (present(parent)) then
       call gtk_window_set_transient_for(dialog, parent)
       call gtk_window_set_destroy_with_parent(dialog, TRUE)
    end if

    ! Get the content area and put the message in it.
    content = gtk_dialog_get_content_area(dialog)
    if (present(type)) then
       itype = type
    else if (button_set == GTK_BUTTONS_YES_NO) then
       itype = GTK_MESSAGE_QUESTION
    else
       itype = GTK_MESSAGE_OTHER
    end if

    if (itype /= GTK_MESSAGE_OTHER) then
       hb = gtk_hbox_new(FALSE, 0)
       call gtk_box_pack_start(content, hb, TRUE, TRUE, 0)
       select case (itype)
       case (GTK_MESSAGE_ERROR)
          junk = gtk_image_new_from_stock(GTK_STOCK_DIALOG_ERROR, &
               & GTK_ICON_SIZE_DIALOG)
       case (GTK_MESSAGE_WARNING)
          junk = gtk_image_new_from_stock(GTK_STOCK_DIALOG_WARNING, &
               & GTK_ICON_SIZE_DIALOG)
       case (GTK_MESSAGE_INFO)
          junk = gtk_image_new_from_stock(GTK_STOCK_DIALOG_INFO, &
               & GTK_ICON_SIZE_DIALOG)
       case (GTK_MESSAGE_QUESTION)
          junk = gtk_image_new_from_stock(GTK_STOCK_DIALOG_QUESTION, &
               & GTK_ICON_SIZE_DIALOG)
       case default
          junk=NULL
       end select
       if (c_associated(junk)) call gtk_box_pack_start(hb, junk, TRUE, TRUE, 0)
       vb = gtk_vbox_new(FALSE, 0)
       call gtk_box_pack_start(hb, vb, TRUE, TRUE, 0)
    else
       vb = content
    end if

    do i = 1, size(message)
       if (i == 1) then
          junk = gtk_label_new(cnull)
          call gtk_label_set_markup(junk, '<b><big>'//trim(message(i))// &
               & '</big></b>'//cnull)
       else
          junk = gtk_label_new(trim(message(i))//cnull)
       end if
       call gtk_box_pack_start(vb, junk, TRUE, TRUE, 0)
    end do

    select case (button_set)
    case (GTK_BUTTONS_NONE)
    case (GTK_BUTTONS_OK)
       junk = gtk_dialog_add_button(dialog, GTK_STOCK_OK, GTK_RESPONSE_OK)
    case (GTK_BUTTONS_CLOSE)
       junk = gtk_dialog_add_button(dialog, GTK_STOCK_CLOSE, &
            & GTK_RESPONSE_CLOSE)
    case (GTK_BUTTONS_CANCEL)
       junk = gtk_dialog_add_button(dialog, GTK_STOCK_CANCEL, &
            & GTK_RESPONSE_CANCEL)
    case (GTK_BUTTONS_YES_NO)
       junk = gtk_dialog_add_button(dialog, GTK_STOCK_YES, GTK_RESPONSE_YES)
       junk = gtk_dialog_add_button(dialog, GTK_STOCK_NO, GTK_RESPONSE_NO)
    case (GTK_BUTTONS_OK_CANCEL)
       junk = gtk_dialog_add_button(dialog, GTK_STOCK_OK, GTK_RESPONSE_OK)
       junk = gtk_dialog_add_button(dialog, GTK_STOCK_CANCEL, &
            & GTK_RESPONSE_CANCEL)
    case default
       call gtk_widget_destroy(dialog)
       resp = GTK_RESPONSE_NONE
       return
    end select

    call gtk_widget_show_all (dialog)
    resp = gtk_dialog_run(dialog)
    call gtk_widget_destroy(dialog)

  end function hl_gtk_message_dialog_show

  !*
  ! File Choosers
  ! hl_gtk_file_chooser_button_new implements the GtkFileChooserButton
  ! and its GtkFileChooser options in a convenient package.
  !
  ! hl_gtk_file_chooser_show implements a more general chooser dialogue
  ! via the file_chooser_widget (file_choose_dialog only has variadic
  ! constructors).
  !
  ! Filters may be either patterns (e.g. '*.f90' or '2011*.lis') or mime types
  ! (e.g. 'image/png' or 'text/*'). The constructors recognise the difference by
  ! the presence or absence of a '/' character. Each filter is a
  ! comma-separated list, which may contain any mixture of patterns and mime
  ! types (e.g. '*.png,image/tiff,*.jpg'). If a name is not provided, then
  ! the filter specification is used as the name.
  !/
  !+
  function hl_gtk_file_chooser_button_new(directory, title, &
       & width, show_hidden, &
       & initial_folder, initial_file, filter, filter_name, file_set, &
       & data, sensitive, tooltip) result(cbutton)

    type(c_ptr) :: cbutton
    integer(kind=c_int), intent(in), optional :: directory
    character(kind=c_char), dimension(*), optional, intent(in) :: title
    integer(kind=c_int), intent(in), optional :: width
    integer(kind=c_int), intent(in), optional :: show_hidden
    character(kind=c_char), dimension(*), optional, intent(in) :: initial_folder, initial_file
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
    ! SHOW_HIDDEN: boolean: optional: Set to TRUE to display hidden files.
    ! INITIAL_FOLDER: string: optional: Use to start the search other than
    ! 		in the current directory.
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
       cbutton = gtk_file_chooser_button_new("Choose file"//cnull, mode)
    else
       cbutton = gtk_file_chooser_button_new("Choose directory"//cnull, mode)
    end if

    call gtk_file_chooser_set_local_only(cbutton, TRUE)

    if (present(show_hidden)) then
       lval = show_hidden
    else
       lval = FALSE
    end if
    call gtk_file_chooser_set_show_hidden(cbutton, lval)

    if (present(width)) call &
         & gtk_file_chooser_button_set_width_chars(cbutton, width)

    if (present(initial_folder)) &
         & lval = gtk_file_chooser_set_current_folder(cbutton, initial_folder)
    if (present(initial_file)) &
         & lval = gtk_file_chooser_set_filename(cbutton, initial_file)

    if (present(filter)) then
       do i = 1, size(filter)
          gfilter = gtk_file_filter_new()

          idx0 = 1
          do
             idx1 = index(filter(i),',')-2
             if (idx1 < 0) then
                if (index(filter(i)(idx0:), '/') == 0) then
                   call gtk_file_filter_add_pattern(gfilter, &
                        & trim(adjustl(filter(i)(idx0:)))//cnull)
                else
                   call gtk_file_filter_add_mime_type(gfilter, &
                        & trim(adjustl(filter(i)(idx0:)))//cnull)
                end if
                exit
             else
                if (index(filter(i)(idx0:idx1), '/') == 0) then
                   call gtk_file_filter_add_pattern(gfilter, &
                        & trim(adjustl(filter(i)(idx0:idx1)))//cnull)
                else
                   call gtk_file_filter_add_mime_type(gfilter, &
                        & trim(adjustl(filter(i)(idx0:idx1)))//cnull)
                end if
                idx0=idx1+2
             end if
          end do
          if (present(filter_name)) then
             call gtk_file_filter_set_name(gfilter, filter_name(i)//cnull)
          else
             call gtk_file_filter_set_name(gfilter, trim(filter(i))//cnull)
          end if
          call gtk_file_chooser_add_filter(cbutton, gfilter)
       end do
    end if

    if (present(file_set)) then
       if (present(data)) then
          call g_signal_connect(cbutton, "file-set"//cnull, file_set, data)
       else
          call g_signal_connect(cbutton, "file-set"//cnull, file_set)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(cbutton, &
         & tooltip)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(cbutton, sensitive)
  end function hl_gtk_file_chooser_button_new

  !+
  function hl_gtk_file_chooser_show(files, cdir, directory, create, &
       & multiple, allow_uri, show_hidden, confirm_overwrite, title, &
       & initial_dir, initial_file, filter, filter_name, parent, all, &
       & wsize, edit_filters) result(isel)

    integer(kind=c_int) :: isel
    character(len=*), dimension(:), intent(out), allocatable :: files
    character(len=*), intent(out), optional :: cdir
    integer(kind=c_int), intent(in), optional :: directory, create, multiple
    integer(kind=c_int), intent(in), optional :: allow_uri, show_hidden
    integer(kind=c_int), intent(in), optional :: confirm_overwrite
    character(kind=c_char), dimension(*), intent(in), optional :: title, initial_dir, initial_file
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
    ! SHOW_HIDDEN: boolean: optional: Set to TRUE to show hidden files.
    ! CONFIRM_OVERWRITE: boolean: optional: Set to TRUE to request
    ! 		confirmation of an overwrite (only used if CREATE
    ! 		is TRUE).
    ! TITLE: string: optional: Title for the window.
    ! INITIAL_DIR: string: optional: Set the initial directory here instead
    ! 		of the current directory.
    ! INITIAL_FILE: string: optional: Set the initial file selection.
    ! FILTER: string(): optional:  The file selection filter. Elements
    ! 		may either be patterns or mime types. Each filter is a
    ! 		comma-separated list of patterns
    ! FILTER_NAME: string(): optional: Names for the filters
    ! PARENT: c_ptr: optional: Parent window for the dialogue.
    ! ALL: boolean: optional: Set to TRUE to add an all-files filter pattern
    ! WSIZE: c_int(2): optional: Set the size for the dialog.
    ! EDIT_FILTERS: boolean: optional: Set to TRUE to proves an entry window
    ! 		to add extra filters.
    !
    ! Returns TRUE if one or more files was selected, FALSE otherwise.
    !-

    type(c_ptr) :: dialog, content, junk, gfilter
    integer(kind=c_int) :: icreate, idir, action, lval
    integer :: i, idx0, idx1
    integer(kind=c_int) :: nsel, resp
    type(c_ptr) :: strptr
    type(c_ptr) :: fbox, fapply
    type(hl_gtk_chooser_info), target :: chooser_info

    ! Create a modal dialogue
    dialog = gtk_dialog_new()
    call gtk_window_set_modal(dialog, TRUE)
    if (present(title)) call gtk_window_set_title(dialog, title)
    if (present(wsize)) then
       call gtk_window_set_default_size(dialog, wsize(1),&
            & wsize(2))
    else
       call gtk_window_set_default_size(dialog, 700, 500)
    end if

    if (present(parent)) then
       call gtk_window_set_transient_for(dialog, parent)
       call gtk_window_set_destroy_with_parent(dialog, TRUE)
    end if

    ! Attach the action buttonsa to the dialogue
    junk = gtk_dialog_add_button(dialog, GTK_STOCK_OPEN, GTK_RESPONSE_APPLY)
    junk = gtk_dialog_add_button(dialog, GTK_STOCK_CANCEL, &
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
       if (icreate == TRUE) then
          action = GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER
       else
          action = GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER
       end if
    else
       if (icreate == TRUE) then
          action = GTK_FILE_CHOOSER_ACTION_SAVE
       else
          action = GTK_FILE_CHOOSER_ACTION_OPEN
       end if
    end if

    ! Create the chooser & put it in the content area
    content = gtk_dialog_get_content_area(dialog)
    chooser_info%chooser = gtk_file_chooser_widget_new(action)
    call gtk_box_pack_start(content, chooser_info%chooser, TRUE, TRUE, 0)

    ! Local/URI
    if (present(allow_uri)) then
       if (allow_uri == FALSE) then
          lval = TRUE
       else
          lval = FALSE
       end if
    else
       lval = TRUE
    end if
    call gtk_file_chooser_set_local_only(chooser_info%chooser, lval)

    ! Multiple selections
    if (present(multiple)) then
       lval = multiple
    else
       lval = FALSE
    end if
    call gtk_file_chooser_set_select_multiple(chooser_info%chooser, lval)

    ! Hidden files
    if (present(show_hidden)) then
       lval = show_hidden
    else
       lval = FALSE
    end if
    call gtk_file_chooser_set_show_hidden(chooser_info%chooser, lval)

    ! Confirm overwrite
    if (icreate == TRUE) then
       if (present(confirm_overwrite)) then
          lval = confirm_overwrite
       else
          lval = FALSE
       end if
       call gtk_file_chooser_set_do_overwrite_confirmation(chooser_info%chooser,&
            & lval)
    end if

    ! Initial directory (precedes file so if file contains a dir it
    ! will overwrite)

    if (present(initial_dir)) &
         & lval = gtk_file_chooser_set_current_folder(chooser_info%chooser, &
         & initial_dir)

    ! Initial file

    if (present(initial_file)) &
         & lval = gtk_file_chooser_select_filename(chooser_info%chooser, &
         & initial_file)


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
                        & trim(adjustl(filter(i)(idx0:)))//cnull)
                else
                   call gtk_file_filter_add_mime_type(gfilter, &
                        & trim(adjustl(filter(i)(idx0:)))//cnull)
                end if
                exit
             else
                if (index(filter(i)(idx0:idx1), '/') == 0) then
                   call gtk_file_filter_add_pattern(gfilter, &
                        & trim(adjustl(filter(i)(idx0:idx1)))//cnull)
                else
                   call gtk_file_filter_add_mime_type(gfilter, &
                        & trim(adjustl(filter(i)(idx0:idx1)))//cnull)
                end if
                idx0=idx1+2
             end if
          end do
          if (present(filter_name)) then
             call gtk_file_filter_set_name(gfilter, &
                  & trim(filter_name(i))//cnull)
          else
             call gtk_file_filter_set_name(gfilter, &
                  & trim(filter(i))//cnull)
          end if
          call gtk_file_chooser_add_filter(chooser_info%chooser, gfilter)
       end do
       if (present(all)) then
          if (all == TRUE) then
             gfilter = gtk_file_filter_new()
             call gtk_file_filter_add_pattern(gfilter, &
                  & "*"//cnull)
             call gtk_file_filter_set_name(gfilter, &
                  "All Files"//cnull)
             call gtk_file_chooser_add_filter(chooser_info%chooser, gfilter)
          end if
       end if
    end if

    ! Add an entry box for extra filters.
    if (present(edit_filters)) then
       if (edit_filters == TRUE) then
          fbox = hl_gtk_box_new(horizontal=TRUE)
          junk = gtk_label_new("New filter:"//cnull)
          call hl_gtk_box_pack(fbox, junk)
          chooser_info%fentry = &
               & hl_gtk_entry_new(activate=c_funloc(hl_gtk_chooser_filt_cb), &
               & len=60, tooltip="Enter a new filter here."//cnull, &
               & data=c_loc(chooser_info))
          call hl_gtk_box_pack(fbox, chooser_info%fentry)
          fapply = hl_gtk_button_new("Apply"//cnull, &
               & clicked=c_funloc(hl_gtk_chooser_filt_cb), &
               & data=c_loc(chooser_info))
          call hl_gtk_box_pack(fbox, fapply)
          call gtk_file_chooser_set_extra_widget(chooser_info%chooser, fbox)
       end if
    end if

    call g_signal_connect(dialog, "response"//cnull, &
         & c_funloc(hl_gtk_chooser_resp_cb), c_loc(chooser_info))

    call gtk_widget_show_all (dialog)
    resp = gtk_dialog_run(dialog)
    call gtk_widget_destroy(dialog)

    isel = chooser_info%iselect
    if (chooser_info%iselect == TRUE) then
       nsel = g_slist_length(chooser_info%chooser_sel_list)
       allocate(files(nsel))
       do i = 1, nsel
          strptr = g_slist_nth_data(chooser_info%chooser_sel_list, i-1)
          call convert_c_string(strptr, len(files), files(i))
          call g_free(strptr)
       end do
       call g_slist_free(chooser_info%chooser_sel_list)

       if (present(cdir)) call convert_c_string(chooser_info%chooser_curdir,&
            & len(cdir), cdir)
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
       if (gtk_file_chooser_get_local_only(chooser_info%chooser) == TRUE) then
          chooser_info%chooser_sel_list = &
               & gtk_file_chooser_get_filenames(chooser_info%chooser)
       else
          chooser_info%chooser_sel_list = &
               & gtk_file_chooser_get_uris(chooser_info%chooser)
       end if
       chooser_info%chooser_curdir = &
            & gtk_file_chooser_get_current_folder(chooser_info%chooser)
    case default
       chooser_info%iselect = FALSE
       write(0,*) "hl_gtk_chooser_resp_cb:: Invalid response received", response
    end select
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
    type(c_ptr) :: gfilter
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
                  & trim(adjustl(filter(idx0:)))//cnull)
          else
             call gtk_file_filter_add_mime_type(gfilter, &
                  & trim(adjustl(filter(idx0:)))//cnull)
          end if
          exit
       else
          if (index(filter(idx0:idx1), '/') == 0) then
             call gtk_file_filter_add_pattern(gfilter, &
                  & trim(adjustl(filter(idx0:idx1)))//cnull)
          else
             call gtk_file_filter_add_mime_type(gfilter, &
                  & trim(adjustl(filter(idx0:idx1)))//cnull)
          end if
          idx0=idx1+2
       end if
    end do
    call gtk_file_filter_set_name(gfilter, &
         & trim(filter)//cnull)

    call gtk_file_chooser_add_filter(chooser_info%chooser, gfilter)
    call gtk_entry_set_text(chooser_info%fentry, CNULL)
  end subroutine hl_gtk_chooser_filt_cb

  !*
  ! Sliders and Spin buttons
  ! GTK sliders and spin buttons use floating point values, the HL interface
  ! implements an automatic interface selection between a floating point or
  ! an integer slider.
  !
  ! Although they belong to completely different widget families in GTK, the
  ! interfaces are very similar, which is why they are grouped together here.
  !/
  !+
  function hl_gtk_slider_flt_new(vmin, vmax, step, vertical, initial_value, &
       & value_changed, data, digits, sensitive, tooltip, draw, length) &
       & result(slider)

    type(c_ptr) :: slider
    real(kind=c_double), intent(in) :: vmin, vmax, step
    integer(kind=c_int), intent(in), optional :: vertical
    real(kind=c_double), intent(in), optional :: initial_value
    type(c_funptr), optional :: value_changed
    type(c_ptr), optional :: data
    integer(kind=c_int), optional, intent(in) :: digits
    integer(kind=c_int), optional, intent(in) :: sensitive
    character(len=*), intent(in), optional:: tooltip
    integer(kind=c_int), intent(in), optional :: draw
    integer(kind=c_int), intent(in), optional :: length

    ! Floating point version of a slider
    !
    ! VMIN: c_double: required: The minimum value for the slider
    ! VMAX: c_double: required: The maximum value for the slider
    ! STEP: c_double: required: The step for the slider.
    ! VERTICAL: boolean: optional: if TRUE then a vertical slider is created
    ! 		if FALSE or absent, then a horizontal silder is created.
    ! INITIAL_VALUE: c_double: optional: Set the intial value of the slider
    ! VALUE_CHANGED: c_funptr: optional: Callback function for the
    ! 		"value-changed" signal.
    ! DATA: c_ptr: optional: User data to pass the the value_changed callback.
    ! DIGITS: c_int: optional: Number of decimal places to show.
    ! SENSITIVE: boolean: optional: Whether the widget is created in the
    ! 		sensitive state.
    ! TOOLTIP: string: optional: A tooltip to display.
    ! DRAW: boolean: optional: Set to FALSE to suppress writing the
    ! 		value.
    ! LENGTH: c_int: optional: Set the length of the slider in pixels
    !
    ! This routine is usually called via its generic interface
    ! hl_gtk_slider_new
    !-

    integer(kind=c_int) :: isvertical, idraw

    ! Create the slider
    if (present(vertical)) then
       isvertical = vertical
    else
       isvertical = FALSE
    end if
    if (isvertical == TRUE) then
       slider = gtk_vscale_new_with_range(vmin, vmax, step)
       if (present(length)) &
            & call gtk_widget_set_size_request(slider, 0, length)
    else
       slider = gtk_hscale_new_with_range(vmin, vmax, step)
       if (present(length)) &
            & call gtk_widget_set_size_request(slider, length, 0)
    end if

    ! Formatting
    if (present(draw)) then
       idraw = draw
    else
       idraw = TRUE
    end if
    call gtk_scale_set_draw_value(slider, idraw)
    if (present(digits)) call gtk_scale_set_digits(slider, digits)

    ! Initial value
    if (present(initial_value)) call gtk_range_set_value(slider, initial_value)

    ! Callback connection
    if (present(value_changed)) then
       if (present(data)) then
          call g_signal_connect(slider, "value-changed"//cnull, value_changed, data)
       else
          call g_signal_connect(slider, "value-changed"//cnull, value_changed)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(slider, &
         & trim(tooltip)//cnull)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(slider, sensitive)
  end function hl_gtk_slider_flt_new

  !+
  function hl_gtk_slider_int_new(imin, imax, vertical, initial_value, &
       & value_changed, data, sensitive, tooltip, draw, length) result(slider)

    type(c_ptr) :: slider
    integer(kind=c_int), intent(in) :: imin, imax
    integer(kind=c_int), intent(in), optional :: vertical
    integer(kind=c_int), intent(in), optional :: initial_value
    type(c_funptr), optional :: value_changed
    type(c_ptr), optional :: data
    integer(kind=c_int), optional, intent(in) :: sensitive
    character(len=*), intent(in), optional:: tooltip ! NB the C-type confuses generic interfaces.
    integer(kind=c_int), intent(in), optional :: draw
    integer(kind=c_int), intent(in), optional :: length

    ! Floating point version of a slider
    !
    ! IMIN: c_int: required: The minimum value for the slider
    ! IMAX: c_int: required: The maximum value for the slider
    ! VERTICAL: boolean: optional: if TRUE then a vertical slider is created
    ! 		if FALSE or absent, then a horizontal silder is created.
    ! INITIAL_VALUE: c_int: optional: Set the intial value of the slider
    ! VALUE_CHANGED: c_funptr: optional: Callback function for the
    ! 		"value-changed" signal.
    ! DATA: c_ptr: optional: User data to pass the the value_changed callback.
    ! SENSITIVE: boolean: optional: Whether the widget is created in the
    ! 		sensitive state.
    ! TOOLTIP: string: optional: A tooltip to display.
    ! DRAW: boolean: optional: Set to FALSE to suppress writing the
    ! 		value.
    ! LENGTH: c_int: optional: Set the length of the slider in pixels
    !
    ! This routine is usually called via its generic interface
    ! hl_gtk_slider_new
    !-

    integer(kind=c_int) :: isvertical, idraw

    ! Create the slider
    if (present(vertical)) then
       isvertical = vertical
    else
       isvertical = FALSE
    end if
    if (isvertical == TRUE) then
       slider = gtk_vscale_new_with_range(real(imin, c_double), &
            &real(imax, c_double), 1.0_c_double)
       if (present(length)) &
            & call gtk_widget_set_size_request(slider, 0, length)
    else
       slider = gtk_hscale_new_with_range(real(imin, c_double), &
            &real(imax, c_double), 1.0_c_double)
       if (present(length)) &
            & call gtk_widget_set_size_request(slider, length, 0)
    end if

    ! Formatting
    if (present(draw)) then
       idraw = draw
    else
       idraw = TRUE
    end if
    call gtk_scale_set_draw_value(slider, idraw)

    ! Initial value
    if (present(initial_value)) call gtk_range_set_value(slider, &
         & real(initial_value, c_double))

    ! Callback connection
    if (present(value_changed)) then
       if (present(data)) then
          call g_signal_connect(slider, "value-changed"//cnull, &
               & value_changed, data)
       else
          call g_signal_connect(slider, "value-changed"//cnull, value_changed)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(slider, &
         & trim(tooltip)//cnull)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(slider, sensitive)
  end function hl_gtk_slider_int_new

  !+
  function hl_gtk_slider_get_value(slider) result(val)

    real(kind=c_double) :: val
    type(c_ptr) :: slider

    ! Get the value of a slider
    !
    ! SLIDER: c_ptr: required: The slider to read.
    !
    ! Note even for an integer slider we get a float value but there's
    ! no problem letting Fortran do the truncation
    !-

    val = gtk_range_get_value(slider)
  end function hl_gtk_slider_get_value

  !+
  subroutine hl_gtk_slider_set_flt(slider, val)

    type(c_ptr), intent(in) :: slider
    real(kind=c_double), intent(in) :: val

    ! Set a floating point value for a slider
    !
    ! SLIDER: c_ptr: required: The slider to set.
    ! VAL: c_double: required: The value to set.
    !
    ! This is usually accessed via the generic interface hl_gtk_slider_set_value
    !-

    call gtk_range_set_value(slider, val)
  end subroutine hl_gtk_slider_set_flt

  !+
  subroutine hl_gtk_slider_set_int(slider, val)

    type(c_ptr), intent(in) :: slider
    integer(kind=c_int), intent(in) :: val

    ! Set a floating point value for a slider
    !
    ! SLIDER: c_ptr: required: The slider to set.
    ! VAL: c_int: required: The value to set.
    !
    ! This is usually accessed via the generic interface hl_gtk_slider_set_value
    !-

    call gtk_range_set_value(slider, real(val, c_double))
  end subroutine hl_gtk_slider_set_int

  !+
  subroutine hl_gtk_slider_set_range(slider, lower, upper)
    type(c_ptr), intent(in) :: slider
    real(kind=c_double), intent(in), optional :: lower, upper

    ! Adjust the bounds of a slider
    !
    ! SLIDER: c_ptr: required: The slider to modify
    ! LOWER: c_double: optional: The new lower bound
    ! UPPER: c_double: optional: The new uppper bound
    !
    ! Note: This routine is not a generic interface as
    ! overloading requires that the interface be distinguishable by its
    ! required arguments, and it seems less annoying to have to convert to
    ! doubles or use a separate call than to specify an unchanged bound.
    !-

    type(c_ptr) :: adjustment
    real(kind=c_double) :: nlower, nupper

    ! Check it's not a do-nothing
    if (.not. (present(upper) .or. present(lower))) return

    adjustment = gtk_range_get_adjustment(slider)
    if (present(lower)) then
       nlower = lower
    else
       nlower = gtk_adjustment_get_lower(adjustment)
    end if

    if (present(upper)) then
       nupper = upper
    else
       nupper = gtk_adjustment_get_upper(adjustment)
    end if

    call gtk_range_set_range(slider, nlower, nupper)

  end subroutine hl_gtk_slider_set_range

  !+
  subroutine hl_gtk_slider_set_range_int(slider, lower, upper)
    type(c_ptr), intent(in) :: slider
    integer(kind=c_int), intent(in), optional :: lower, upper

    ! Adjust the bounds of a slider, integer values
    !
    ! SLIDER: c_ptr: required: The slider to modify
    ! LOWER: c_int: optional: The new lower bound
    ! UPPER: c_int: optional: The new uppper bound
    !
    ! Note: This routine is not a generic interface as
    ! overloading requires that the interface be distinguishable by its
    ! required arguments, and it seems less annoying to use a separate
    ! call than to specify an unchanged bound.
    !-

    type(c_ptr) :: adjustment
    real(kind=c_double) :: nlower, nupper

    ! Check it's not a do-nothing
    if (.not. (present(upper) .or. present(lower))) return

    adjustment = gtk_range_get_adjustment(slider)
    if (present(lower)) then
       nlower = real(lower, c_double)
    else
       nlower = gtk_adjustment_get_lower(adjustment)
    end if

    if (present(upper)) then
       nupper = real(upper, c_double)
    else
       nupper = gtk_adjustment_get_upper(adjustment)
    end if

    call gtk_range_set_range(slider, nlower, nupper)

  end subroutine hl_gtk_slider_set_range_int

  !+
  function hl_gtk_spin_button_flt_new(vmin, vmax, step, initial_value, &
       & value_changed, data, digits, sensitive, tooltip, wrap) &
       & result(spin_button)

    type(c_ptr) :: spin_button
    real(kind=c_double), intent(in) :: vmin, vmax, step
    real(kind=c_double), intent(in), optional :: initial_value
    type(c_funptr), optional :: value_changed
    type(c_ptr), optional :: data
    integer(kind=c_int), optional, intent(in) :: digits
    integer(kind=c_int), optional, intent(in) :: sensitive
    character(len=*), intent(in), optional:: tooltip ! NB the C-type confuses generic interfaces.
    integer(kind=c_int), intent(in), optional :: wrap

    ! Floating point version of a spin_button
    !
    ! VMIN: c_double: required: The minimum value for the spin_button
    ! VMAX: c_double: required: The maximum value for the spin_button
    ! STEP: c_double: required: The step for the spin_button.
    ! INITIAL_VALUE: c_double: optional: Set the intial value of the spin_button
    ! VALUE_CHANGED: c_funptr: optional: Callback function for the
    ! 		"value-changed" signal.
    ! DATA: c_ptr: optional: User data to pass the the value_changed callback.
    ! DIGITS: c_int: optional: Number of decimal places to show.
    ! SENSITIVE: boolean: optional: Whether the widget is created in the
    ! 		sensitive state.
    ! TOOLTIP: string: optional: A tooltip to display.
    ! WRAP: boolean: optional: If set to TRUE then wrap around if limit is
    ! 		exceeded
    !
    ! This routine is usually called via its generic interface
    ! hl_gtk_spin_button_new
    !-

    ! Create the spin_button
    spin_button = gtk_spin_button_new_with_range(vmin, vmax, step)

    ! Formatting
    call gtk_spin_button_set_numeric(spin_button, TRUE)
    if (present(digits)) call gtk_spin_button_set_digits(spin_button, digits)
    if (present(wrap)) call gtk_spin_button_set_wrap(spin_button, wrap)

    ! Initial value
    if (present(initial_value)) &
         & call gtk_spin_button_set_value(spin_button, initial_value)

    ! Callback connection
    if (present(value_changed)) then
       if (present(data)) then
          call g_signal_connect(spin_button, "value-changed"//cnull, value_changed, &
               & data)
       else
          call g_signal_connect(spin_button, "value-changed"//cnull, value_changed)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(spin_button, &
         & trim(tooltip)//cnull)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(spin_button, sensitive)
  end function hl_gtk_spin_button_flt_new

  !+
  function hl_gtk_spin_button_int_new(imin, imax, initial_value, &
       & value_changed, data, sensitive, tooltip, wrap) result(spin_button)

    type(c_ptr) :: spin_button
    integer(kind=c_int), intent(in) :: imin, imax
    integer(kind=c_int), intent(in), optional :: initial_value
    type(c_funptr), optional :: value_changed
    type(c_ptr), optional :: data
    integer(kind=c_int), optional, intent(in) :: sensitive
    character(len=*), intent(in), optional:: tooltip ! NB the C-type confuses generic interfaces.
    integer(kind=c_int), intent(in), optional :: wrap

    ! Floating point version of a spin_button
    !
    ! IMIN: c_int: required: The minimum value for the spin_button
    ! IMAX: c_int: required: The maximum value for the spin_button
    ! INITIAL_VALUE: c_int: optional: Set the intial value of the spin_button
    ! VALUE_CHANGED: c_funptr: optional: Callback function for the
    ! 		"value-changed" signal.
    ! DATA: c_ptr: optional: User data to pass the the value_changed callback.
    ! SENSITIVE: boolean: optional: Whether the widget is created in the
    ! 		sensitive state.
    ! TOOLTIP: string: optional: A tooltip to display.
    ! WRAP: boolean: optional: If set to TRUE then wrap around if limit is
    ! 		exceeded
    !
    ! This routine is usually called via its generic interface
    ! hl_gtk_spin_button_new
    !-

    ! Create the spin_button
    spin_button = gtk_spin_button_new_with_range(real(imin, c_double), &
         &real(imax, c_double), 1.0_c_double)

    ! Formatting
    call gtk_spin_button_set_numeric(spin_button, TRUE)
    if (present(wrap)) call gtk_spin_button_set_wrap(spin_button, wrap)

    ! Initial value
    if (present(initial_value)) call gtk_spin_button_set_value(spin_button, &
         & real(initial_value, c_double))

    ! Callback connection
    if (present(value_changed)) then
       if (present(data)) then
          call g_signal_connect(spin_button, "value-changed"//cnull, value_changed, &
               & data)
       else
          call g_signal_connect(spin_button, "value-changed"//cnull, value_changed)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(spin_button, &
         & trim(tooltip)//cnull)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(spin_button, sensitive)
  end function hl_gtk_spin_button_int_new

  !+
  function hl_gtk_spin_button_get_value(spin_button) result(val)

    real(kind=c_double) :: val
    type(c_ptr) :: spin_button

    ! Get the value of a spin_button
    !
    ! SPIN_BUTTON: c_ptr: required: The spin_button to read.
    !
    ! Note even for an integer spin_button we get a float value but there's
    ! no problem letting Fortran do the truncation
    !-

    val = gtk_spin_button_get_value(spin_button)
  end function hl_gtk_spin_button_get_value

  !+
  subroutine hl_gtk_spin_button_set_flt(spin_button, val)

    type(c_ptr), intent(in) :: spin_button
    real(kind=c_double), intent(in) :: val

    ! Set a floating point value for a spin_button
    !
    ! SPIN_BUTTON: c_ptr: required: The spin_button to set.
    ! VAL: c_double: required: The value to set.
    !
    ! This is usually accessed via the generic interface hl_gtk_spin_button_set_value
    !-

    call gtk_spin_button_set_value(spin_button, val)
  end subroutine hl_gtk_spin_button_set_flt

  !+
  subroutine hl_gtk_spin_button_set_int(spin_button, val)

    type(c_ptr), intent(in) :: spin_button
    integer(kind=c_int), intent(in) :: val

    ! Set a floating point value for a spin_button
    !
    ! SPIN_BUTTON: c_ptr: required: The spin_button to set.
    ! VAL: c_int: required: The value to set.
    !
    ! This is usually accessed via the generic interface hl_gtk_spin_button_set_value
    !-

    call gtk_spin_button_set_value(spin_button, real(val, c_double))
  end subroutine hl_gtk_spin_button_set_int

  !+
  subroutine hl_gtk_spin_button_set_range(spin_button, lower, upper)
    type(c_ptr), intent(in) :: spin_button
    real(kind=c_double), intent(in), optional :: lower, upper

    ! Adjust the bounds of a spin box
    !
    ! SLIDER: c_ptr: required: The slider to modify
    ! LOWER: c_double: optional: The new lower bound
    ! UPPER: c_double: optional: The new uppper bound
    !
    ! Note: This routine is not a generic interface as
    ! overloading requires that the interface be distinguishable by its
    ! required arguments, and it seems less annoying to have to convert to
    ! doubles or use a separate call than to specify an unchanged bound.
    !-

    type(c_ptr), target :: clower, cupper
    real(kind=c_double), pointer :: olower, oupper
    real(kind=c_double) :: nlower, nupper

    ! Check it's not a do-nothing
    if (.not. (present(upper) .or. present(lower))) return

    call gtk_spin_button_get_range(spin_button, clower, cupper)

    if (present(lower)) then
       nlower = lower
    else
       call c_f_pointer(clower, olower)
       nlower = olower
    end if

    if (present(upper)) then
       nupper = upper
    else
       call c_f_pointer(cupper, oupper)
       nupper = oupper
    end if

    call gtk_spin_button_set_range(spin_button, nlower, nupper)

  end subroutine hl_gtk_spin_button_set_range

  !+
  subroutine hl_gtk_spin_button_set_range_int(spin_button, lower, upper)
    type(c_ptr), intent(in) :: spin_button
    integer(kind=c_int), intent(in), optional :: lower, upper

    ! Adjust the bounds of a spin box, integer values
    !
    ! SLIDER: c_ptr: required: The slider to modify
    ! LOWER: c_int: optional: The new lower bound
    ! UPPER: c_int: optional: The new uppper bound
    !
    ! Note: This routine is not a generic interface as
    ! overloading requires that the interface be distinguishable by its
    ! required arguments, and it seems less annoying to use a separate
    ! call than to specify an unchanged bound.
    !-

    type(c_ptr), target :: clower, cupper
    real(kind=c_double), pointer :: olower, oupper
    real(kind=c_double) :: nlower, nupper

    ! Check it's not a do-nothing
    if (.not. (present(upper) .or. present(lower))) return

    call gtk_spin_button_get_range(spin_button, clower, cupper)

    if (present(lower)) then
       nlower = real(lower, c_double)
    else
       call c_f_pointer(clower, olower)
       nlower = olower
    end if

    if (present(upper)) then
       nupper = real(upper, c_double)
    else
       call c_f_pointer(cupper, oupper)
       nupper = oupper
    end if

    call gtk_spin_button_set_range(spin_button, nlower, nupper)

  end subroutine hl_gtk_spin_button_set_range_int

  !*
  ! ComboBox
  ! This interface implements the GtkComboBoxText widget for making a chooser.
  ! While this has more limited capabilities than the full GtkComboBox, it
  ! is adequate for the vast majority of uses.
  !/

  !+
  function hl_gtk_combo_box_new(has_entry, changed, data, initial_choices, &
       & sensitive, tooltip, active) result(cbox)

    type(c_ptr) :: cbox
    integer(kind=c_int), intent(in), optional :: has_entry
    type(c_funptr), optional :: changed
    type(c_ptr), intent(in), optional :: data
    character(len=*), dimension(:), intent(in), optional :: initial_choices
    integer(kind=c_int), intent(in), optional :: sensitive
    character(kind=c_char), dimension(*), optional, intent(in) :: tooltip
    integer(kind=c_int), optional, intent(in) :: active

    ! Creator for the combobox.
    !
    ! HAS_ENTRY: boolean: optional: Set to TRUE to add an entry field.
    ! CHANGED: c_funptr: optional: Callback routine for the "changed" signal.
    ! DATA: c_ptr: optional: User data for the changed callback.
    ! INITIAL_CHOICES: string(): optional: Initial list of choices.
    ! SENSITIVE: boolean: optional: Set to FALSE to make the widget start in an
    ! 		insensitive state.
    ! TOOLTIP: string: optional: A tooltip to display when the pointer is
    ! 		held over the widget.
    ! ACTIVE: c_int: optional: The initial active selection.
    !-

    integer(kind=c_int) :: ientry
    integer(kind=c_int) :: i

    if (present(has_entry)) then
       ientry = has_entry
    else
       ientry = FALSE
    end if

    if (ientry == TRUE) then
!GTK3
       cbox = gtk_combo_box_text_new_with_entry()
!GTK2
!!$2       cbox = gtk_combo_box_entry_new_text()
    else
!GTK3
       cbox = gtk_combo_box_text_new()
!GTK2
!!$2       cbox =  gtk_combo_box_new_text()
    end if

    if (present(initial_choices)) then
       do i=1,size(initial_choices)
!GTK3
          call gtk_combo_box_text_append_text(cbox, &
               & trim(initial_choices(i))//CNULL)
!GTK2
!!$2          call gtk_combo_box_append_text(cbox, &
!!$2               & trim(initial_choices(i))//CNULL)
       end do
    end if

    if (present(changed)) then
       if (present(data)) then
          call g_signal_connect(cbox, "changed"//CNULL, changed, data)
       else
          call g_signal_connect(cbox, "changed"//CNULL, changed)
       end if
    end if

    if (present(active)) call gtk_combo_box_set_active(cbox, active)
    if (present(sensitive)) call gtk_widget_set_sensitive(cbox, sensitive)
    if (present(tooltip)) call gtk_widget_set_tooltip_text(cbox, tooltip)
  end function hl_gtk_combo_box_new

  !+
  subroutine hl_gtk_combo_box_add_text(cbox, text, index, at_start)

    type(c_ptr), intent(in) :: cbox
    character(kind=c_char), dimension(*), optional :: text
    integer(kind=c_int), intent(in), optional :: index
    integer(kind=c_int), intent(in), optional :: at_start

    ! Add a new choice to a combo box.
    !
    ! CBOX: c_ptr: required: The combo box to modify.
    ! TEXT: string: required: The text to add.
    ! INDEX: c_int: optional: The location at which to add the text.
    ! AT_START: boolean: optional: If set to TRUE and INDEX is not given
    ! 		then add the text at the start of the list.
    !
    ! If neither INDEX nor AT_START is present the text is appended.
    !-

    integer(kind=c_int) :: prepend

    if (present(index)) then
!GTK3
       call gtk_combo_box_text_insert_text(cbox, index, text)
!GTK2
!!$2       call gtk_combo_box_insert_text(cbox, index, text)
    else
       if (present(at_start)) then
          prepend = at_start
       else
          prepend = FALSE
       end if
       if (prepend == TRUE) then
!GTK3
          call gtk_combo_box_text_prepend_text(cbox, text)
!GTK2
!!$2          call gtk_combo_box_prepend_text(cbox, text)
       else
!GTK3
          call gtk_combo_box_text_append_text(cbox, text)
!GTK2
!!$2          call gtk_combo_box_append_text(cbox, text)
       end if
    end if
  end subroutine hl_gtk_combo_box_add_text

  !+
  subroutine hl_gtk_combo_box_delete(cbox, index)

    type(c_ptr), intent(in) :: cbox
    integer(kind=c_int), intent(in) :: index

    ! Delete a line from a combo box
    !
    ! CBOX: c_ptr: required: The combo box to update
    ! INDEX: c_int: required: The index of the choce to remove
    !-

!GTK3
    call gtk_combo_box_text_remove(cbox, index)
!GTK2
!!$2    call gtk_combo_box_remove_text(cbox, index)

  end subroutine hl_gtk_combo_box_delete

  !+
  function hl_gtk_combo_box_get_active(cbox, text, ftext) result(index)

    integer(kind=c_int) :: index
    type(c_ptr), intent(in) :: cbox
    type(c_ptr), intent(out), optional :: text
    character(len=*), intent(out), optional :: ftext

    ! Get the selection from a combo box
    !
    ! CBOX: c_ptr: required: The combo box to query.
    ! TEXT: c_ptr: optional: C pointer to the text.
    ! FTEXT: fstring: optional: The string as a Fortran string.
    !-

    type(c_ptr), target :: ctext

    index = gtk_combo_box_get_active(cbox)

    if (present(text) .or. present(ftext)) then

!GTK3
      ctext = gtk_combo_box_text_get_active_text(cbox)
!GTK2
!!$2       ctext = gtk_combo_box_get_active_text(cbox)

       ! This is a bit ugly
       if (present(ftext)) &
            & call convert_c_string(ctext, len(ftext), ftext)

       if (present(text)) text=ctext
    end if
  end function hl_gtk_combo_box_get_active

  !+
  subroutine hl_gtk_widget_add_accelerator(widget, signal, accel_group, &
       & accel_key, accel_mods, accel_flags)
    type(c_ptr), intent(in) :: widget
    character(kind=c_char), dimension(*), intent(in) :: signal
    type(c_ptr), intent(in) :: accel_group
    character(kind=c_char), dimension(*), intent(in) :: accel_key
    integer(kind=c_int), intent(in), optional :: accel_mods, accel_flags
    
    ! Add an accelerator to a widget (just saves a lot of code duplication)
    !
    ! WIDGET: c_ptr: required: The widget with which the accelerator is
    ! 		associated.
    ! SIGNAL: string: required: The signal with which the accelerator is
    ! 		associated
    ! ACCEL_GROUP: c_ptr: required: The accelerator group to which the
    ! 		accelerator belongs (must have been created and added to
    ! 		the top-level window).
    ! ACCEL_KEY: string: required: The key name to use for the accelerator
    ! ACCEL_MODS: c_int: optional: The key modifiers for the accelerator,
    ! 		This defaults to GDK_CONTROL_MASK, set it to 0 to use the
    ! 		unmodified key.
    ! ACCEL_FLAGS: c_int: optional: Flags for the accelerator, if not present
    ! 		then GTK_ACCEL_VISIBLE, is used (to hide the accelerator,
    ! 		use ACCEL_FLAGS=0).
    !-

    integer(kind=c_int) :: ikey, imods, iflags

    ikey = gdk_keyval_from_name(accel_key)
    if (present(accel_mods)) then
       imods = accel_mods
    else
       imods = GDK_CONTROL_MASK
    end if
    if (present(accel_flags)) then
       iflags = accel_flags
    else
       iflags = GTK_ACCEL_VISIBLE
    end if
    call gtk_widget_add_accelerator(widget, signal, &
               & accel_group, ikey, imods, iflags)
  end subroutine hl_gtk_widget_add_accelerator
end module gtk_hl
