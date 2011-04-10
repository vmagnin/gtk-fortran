program list_demo
  ! A translation into Fortran of the "hello_world" tree demo from
  ! the GTK TreeView Tutorial.
  ! Original version by: Tim-Philipp Müller (2005)
  ! http://scentric.net/tutorial/ch-treeview.html#sec-TreeView-HelloWorld
  ! varargs calls replaced & translated into Fortran: James Tappin
  ! 21-Mar-2011

  use gtk_sup  ! Contains iter structure and gtypes definitions

  use gtk, only: gtk_cell_renderer_text_new, gtk_container_add, gtk_list_store_ap&
  &pend, gtk_list_store_newv, gtk_list_store_set_value, gtk_main, gtk_main_quit, &
  &gtk_tree_view_column_add_attribute, gtk_tree_view_column_new, gtk_tree_view_co&
  &lumn_pack_start, gtk_tree_view_column_set_title, gtk_tree_view_insert_column, &
  &gtk_tree_view_new, gtk_tree_view_set_model, gtk_widget_show, gtk_widget_show_a&
  &ll, gtk_window_new,&
  &FALSE, CNULL, NULL, GTK_WINDOW_TOPLEVEL, gtk_init, g_signal_connect

  use g, only: g_object_unref, g_value_init, g_value_set_static_string, g_value_s&
  &et_uint


  enum, bind(c)
     enumerator :: COL_NAME = 0
     enumerator :: COL_AGE
     enumerator :: NUM_COLS
  end enum

  type(c_ptr) :: window, view

  call gtk_init()
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  call g_signal_connect (window, "delete_event"//CNULL, &
       & c_funloc(gtk_main_quit), NULL); ! dirty

  view = create_view_and_model ()

  call gtk_container_add (window, view)

  call gtk_widget_show_all (window)

  call gtk_main ()

contains
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
    call g_value_set_static_string(c_loc(valt), "Heinz El-Mann"//CNULL)
    call gtk_list_store_set_value(store, c_loc(iter), COL_NAME, &
         & c_loc(valt))
    call g_value_set_uint(c_loc(vali), 51)
    call gtk_list_store_set_value(store, c_loc(iter), COL_AGE, c_loc(vali))

    ! append another row and fill in some data
    call gtk_list_store_append (store, c_loc(iter))
    call g_value_set_static_string(c_loc(valt), "Jane Doe"//CNULL)
    call gtk_list_store_set_value(store, c_loc(iter), COL_NAME, c_loc(valt))
    call g_value_set_uint(c_loc(vali), 23)
    call gtk_list_store_set_value(store, c_loc(iter), COL_AGE, c_loc(vali))

    ! And a third
    call gtk_list_store_append (store, c_loc(iter))
    call g_value_set_static_string(c_loc(valt), "Joe Bungop"//cnull)
    call gtk_list_store_set_value(store, c_loc(iter), COL_NAME, c_loc(valt))
    call g_value_set_uint(c_loc(vali), 91)
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
    ncol= gtk_tree_view_insert_column(view, col, -1)
    call gtk_tree_view_column_add_attribute(col, renderer, &
         & "text"//cnull, COL_NAME)
    call gtk_tree_view_column_set_title(col, "Name"//cnull)


    ! --- Column #2 ---

    col = gtk_tree_view_column_new()
    renderer = gtk_cell_renderer_text_new ()
    call gtk_tree_view_column_pack_start(col, renderer, FALSE)
    ncol = gtk_tree_view_insert_column(view, col, -1)
    call gtk_tree_view_column_set_title(col, "Age"//cnull)
    call gtk_tree_view_column_add_attribute(col, renderer, &
         & "text"//cnull, COL_AGE)

    model = create_and_fill_model ()

    call gtk_tree_view_set_model (view, model)

    call g_object_unref (model)  ! destroy model automatically with view
  end function create_view_and_model
end program list_demo
