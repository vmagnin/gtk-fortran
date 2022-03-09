! A translation into Fortran of the "hello_world" tree demo from
! the GTK TreeView Tutorial.
! Original version by: Tim-Philipp Müller (2005)
! http://scentric.net/tutorial/ch-treeview.html#sec-TreeView-HelloWorld
! varargs calls replaced & translated into Fortran: James Tappin 21-Mar-2011
! GTK 4 version: vmagnin 2020-05-28

module handlers
  use, intrinsic :: iso_c_binding, only: c_null_char, c_null_ptr
  use gtk_sup  ! Contains iter structure and gtypes definitions
  use gtk, only: gtk_cell_renderer_text_new, gtk_window_set_child, &
  & gtk_list_store_append, gtk_list_store_newv, gtk_list_store_set_value,&
  & gtk_tree_view_column_add_attribute, gtk_tree_view_column_new, &
  & gtk_tree_view_column_pack_start, gtk_tree_view_column_set_title, &
  & gtk_tree_view_insert_column, gtk_tree_view_new, gtk_tree_view_set_model, &
  & gtk_widget_show, gtk_window_new, gtk_window_set_title, FALSE, &
  & gtk_application_window_new, g_signal_connect
  use g, only: g_object_unref, g_value_init, g_value_set_static_string, &
             & g_value_set_uint

  implicit none

  enum, bind(c)
     enumerator :: COL_NAME = 0
     enumerator :: COL_AGE
     enumerator :: NUM_COLS
  end enum

contains
  ! Callback function for the signal "activate" emitted by g_application_run().
  ! We use a subroutine because it should return void.
  ! The GUI is defined here.
  subroutine activate(app, gdata) bind(c)
    type(c_ptr), value, intent(in)  :: app, gdata
    ! Pointers toward our GTK widgets:
    type(c_ptr) :: window, view

    ! Create the window:
    window = gtk_application_window_new(app)
    ! Don't forget that C strings must end with a null char:
    call gtk_window_set_title(window, "List Demo"//c_null_char)

    !******************************************************************
    ! Adding widgets in the window:
    !******************************************************************
    view = create_view_and_model ()
    call gtk_window_set_child(window, view)
    !******************************************************************

    ! If you don't show it, nothing will appear on screen...
    call gtk_widget_show(window)
  end subroutine activate


  function create_and_fill_model() result(store)
    type(c_ptr) :: store
    integer(kind=type_kind), dimension(2), target :: ctypes = &
         & [g_type_string, g_type_uint]
    type(gtktreeiter), target :: iter
    type(gvalue), target :: valt, vali
    type(c_ptr) :: val

    ! Initialize the GValues
    val = c_loc(valt)
    val = g_value_init(val, G_TYPE_STRING)
    val = c_loc(vali)
    val = g_value_init(val, G_TYPE_UINT)

    ! Create the list store
    store = gtk_list_store_newv(NUM_COLS, c_loc(ctypes))

    ! Append row 1 and add data
    call gtk_list_store_append(store, c_loc(iter))
    call g_value_set_static_string(c_loc(valt), "Heinz El-Mann"//c_null_char)
    call gtk_list_store_set_value(store, c_loc(iter), COL_NAME, &
         & c_loc(valt))
    call g_value_set_uint(c_loc(vali), 51_c_int)
    call gtk_list_store_set_value(store, c_loc(iter), COL_AGE, c_loc(vali))

    ! append another row and fill in some data
    call gtk_list_store_append (store, c_loc(iter))
    call g_value_set_static_string(c_loc(valt), "Jane Doe"//c_null_char)
    call gtk_list_store_set_value(store, c_loc(iter), COL_NAME, c_loc(valt))
    call g_value_set_uint(c_loc(vali), 23_c_int)
    call gtk_list_store_set_value(store, c_loc(iter), COL_AGE, c_loc(vali))

    ! And a third
    call gtk_list_store_append (store, c_loc(iter))
    call g_value_set_static_string(c_loc(valt), "Joe Bungop"//c_null_char)
    call gtk_list_store_set_value(store, c_loc(iter), COL_NAME, c_loc(valt))
    call g_value_set_uint(c_loc(vali), 91_c_int)
    call gtk_list_store_set_value(store, c_loc(iter), COL_AGE, c_loc(vali))
  end function create_and_fill_model


  function create_view_and_model() result(view)
    type(c_ptr) :: view

    type(c_ptr) :: col, renderer, model
    integer(kind=c_int) :: ncol

    view = gtk_tree_view_new ()

    ! --- Column #1 ---
    renderer = gtk_cell_renderer_text_new ()
    col = gtk_tree_view_column_new()
    call gtk_tree_view_column_pack_start(col, renderer, FALSE)
    ncol= gtk_tree_view_insert_column(view, col, -1_c_int)
    call gtk_tree_view_column_add_attribute(col, renderer, &
         & "text"//c_null_char, COL_NAME)
    call gtk_tree_view_column_set_title(col, "Name"//c_null_char)

    ! --- Column #2 ---
    col = gtk_tree_view_column_new()
    renderer = gtk_cell_renderer_text_new ()
    call gtk_tree_view_column_pack_start(col, renderer, FALSE)
    ncol = gtk_tree_view_insert_column(view, col, -1_c_int)
    call gtk_tree_view_column_set_title(col, "Age"//c_null_char)
    call gtk_tree_view_column_add_attribute(col, renderer, &
         & "text"//c_null_char, COL_AGE)

    model = create_and_fill_model ()
    call gtk_tree_view_set_model (view, model)
    call g_object_unref (model)  ! destroy model automatically with view
  end function create_view_and_model
end module handlers

!*******************************************************************************
! In the main program, we declare the GTK application, connect it to its 
! "activate" function where we will create the GUI, 
! and finally call the GLib main loop.
!*******************************************************************************
program list_demo

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
  app = gtk_application_new("gtk-fortran.examples.list_demo"//c_null_char, &
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

end program list_demo

