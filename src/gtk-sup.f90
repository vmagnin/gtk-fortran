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

! You should have received a copy of the GNU General Public
!  License along with
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Contributed by James Tappin, Ian Harvey (IanH0073)
! Last modifications: 06-20-2012, vmagnin+IanH0073 02-21-2019

!*
! Supplementary material
! This module contains some supplementary material useful for writing Gtk+
! programs in Fortran.
!
! These are mostly definitions that are not (currenty) extracted by the
! automatic tools. There are also some character conversion routines.
!/
module gtk_sup
  ! The definitions in this supplementary module are ones that are not (yet)
  !  automatically extracted from the GTK headers.

  ! Currently it contains:
  ! GTYPE: Definitions of the integer length and the values for each type.
  ! GtkTreeIter: Type definition.
  ! GValue: Pseudo type definition.
  ! GtkTextIter: Type definition.
  ! GError: Type definition.
  ! Various GTK_STOCK strings.

  use iso_c_binding
  use gtk, only:  TRUE, FALSE
  use g, only: g_type_fundamental

  implicit none

  !+
  ! Gtype
  ! The various Gtype definitions. 
  !-

  ! Gtype definitions
  integer, parameter :: type_kind=c_size_t
  integer(kind=c_int), parameter :: g_type_fundamental_shift=2
  integer(kind=type_kind), parameter :: G_TYPE_INVALID = &
       & ishft(0_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_NONE = &
       & ishft(1_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_INTERFACE = &
       & ishft(2_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_CHAR = &
       & ishft(3_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_UCHAR = &
       & ishft(4_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_BOOLEAN = &
       & ishft(5_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_INT = &
       & ishft(6_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_UINT = &
       & ishft(7_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_LONG = &
       & ishft(8_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_ULONG = &
       & ishft(9_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_INT64 = &
       & ishft(10_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_UINT64 = &
       & ishft(11_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_ENUM = &
       & ishft(12_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_FLAGS = &
       & ishft(13_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_FLOAT = &
       & ishft(14_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_DOUBLE = &
       & ishft(15_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_STRING = &
       & ishft(16_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_POINTER = &
       & ishft(17_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_BOXED = &
       & ishft(18_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_PARAM = &
       & ishft(19_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_OBJECT = &
       & ishft(20_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_VARIANT = &
       & ishft(21_type_kind, g_type_fundamental_shift)

  !+
  ! Iterators and Gvalues
  ! These structures are always allocated in the calling program, rather
  ! than being declared as pointers and leaving Gtk+ to allocate them.
  !
  ! * GtkTreeIter, Iterator for TreeView widgets
  ! * GtkTextIter, Iterator for TextView widgets
  ! * GValue, A value container.
  !-

  ! Define a GtkTreeIter (this has to be pre-allocated in the calls)
  type, bind(c) :: gtktreeiter
     integer(kind=c_int) :: intv=0
     type(c_ptr) :: p0=C_NULL_PTR, p1=C_NULL_PTR, p2=C_NULL_PTR
  end type gtktreeiter

  ! Define a spacemaker for GValue It's 24 bytes on 64 bit & 20 on 32,
  ! i.e. one long and 2 64-bit integers
  type, bind(c) :: gvalue
     integer(kind=c_long) :: il = 0
     integer(kind=c_int64_t), dimension(2) :: i64 = (/0, 0/)
  end type gvalue

  ! Define a GtkTextIter (this has to be pre-allocated in the calls)
  type, bind(c) :: gtktextiter 
     type(c_ptr) :: d1, d2
     integer(kind=c_int) :: d3, d4, d5, d6, d7, d8
     type(c_ptr) :: d9, d10
     integer(kind=c_int) :: d11, d12, d13
     type(c_ptr) :: d14
  end type gtktextiter

  !+
  ! GError
  ! GError is a transparent structure that returns error information.
  !-
  type, bind(c) :: gerror
     integer(kind=c_int32_t) :: domain
     integer(kind=c_int) :: code
     type(c_ptr) :: message    ! A C pointer to the error message.
  end type gerror

  !+
  ! GTK_STOCK
  ! Stock button and icon names extracted from gtk/gtkstock.h
  ! N.B. GTK_STOCK_ADD clashes with the routine gtk_stock_add and is therefore
  ! omitted.
  !-

  character(len=*), parameter :: GTK_STOCK_ABOUT = &
       & "gtk-about"//c_null_char
!!$  GTK_STOCK_ADD clashes with the routine gtk_stock_add
!!$  character(len=*), parameter :: GTK_STOCK_ADD = &
!!$       & "gtk-add"//c_null_char
  character(len=*), parameter :: GTK_STOCK_APPLY = &
       & "gtk-apply"//c_null_char
  character(len=*), parameter :: GTK_STOCK_BOLD = &
       & "gtk-bold"//c_null_char
  character(len=*), parameter :: GTK_STOCK_CANCEL = &
       & "gtk-cancel"//c_null_char
  character(len=*), parameter :: GTK_STOCK_CAPS_LOCK_WARNING= &
       & "gtk-caps-lock-warning"//c_null_char
  character(len=*), parameter :: GTK_STOCK_CDROM = &
       & "gtk-cdrom"//c_null_char
  character(len=*), parameter :: GTK_STOCK_CLEAR = &
       & "gtk-clear"//c_null_char
  character(len=*), parameter :: GTK_STOCK_CLOSE = &
       & "gtk-close"//c_null_char
  character(len=*), parameter :: GTK_STOCK_COLOR_PICKER = &
       & "gtk-color-picker"//c_null_char
  character(len=*), parameter :: GTK_STOCK_CONNECT = &
       & "gtk-connect"//c_null_char
  character(len=*), parameter :: GTK_STOCK_CONVERT = &
       & "gtk-convert"//c_null_char
  character(len=*), parameter :: GTK_STOCK_COPY = &
       & "gtk-copy"//c_null_char
  character(len=*), parameter :: GTK_STOCK_CUT = &
       & "gtk-cut"//c_null_char
  character(len=*), parameter :: GTK_STOCK_DELETE = &
       & "gtk-delete"//c_null_char
  character(len=*), parameter :: GTK_STOCK_DIALOG_AUTHENTICATION= &
       & "gtk-dialog-authentication"//c_null_char
  character(len=*), parameter :: GTK_STOCK_DIALOG_INFO = &
       & "gtk-dialog-info"//c_null_char
  character(len=*), parameter :: GTK_STOCK_DIALOG_WARNING = &
       & "gtk-dialog-warning"//c_null_char
  character(len=*), parameter :: GTK_STOCK_DIALOG_ERROR = &
       & "gtk-dialog-error"//c_null_char
  character(len=*), parameter :: GTK_STOCK_DIALOG_QUESTION = &
       & "gtk-dialog-question"//c_null_char
  character(len=*), parameter :: GTK_STOCK_DIRECTORY = &
       & "gtk-directory"//c_null_char
  character(len=*), parameter :: GTK_STOCK_DISCARD = &
       & "gtk-discard"//c_null_char
  character(len=*), parameter :: GTK_STOCK_DISCONNECT = &
       & "gtk-disconnect"//c_null_char
  character(len=*), parameter :: GTK_STOCK_DND = &
       & "gtk-dnd"//c_null_char
  character(len=*), parameter :: GTK_STOCK_DND_MULTIPLE = &
       & "gtk-dnd-multiple"//c_null_char
  character(len=*), parameter :: GTK_STOCK_EDIT = &
       & "gtk-edit"//c_null_char
  character(len=*), parameter :: GTK_STOCK_EXECUTE = &
       & "gtk-execute"//c_null_char
  character(len=*), parameter :: GTK_STOCK_FILE = &
       & "gtk-file"//c_null_char
  character(len=*), parameter :: GTK_STOCK_FIND = &
       & "gtk-find"//c_null_char
  character(len=*), parameter :: GTK_STOCK_FIND_AND_REPLACE= &
       & "gtk-find-and-replace"//c_null_char
  character(len=*), parameter :: GTK_STOCK_FLOPPY = &
       & "gtk-floppy"//c_null_char
  character(len=*), parameter :: GTK_STOCK_FULLSCREEN = &
       & "gtk-fullscreen"//c_null_char
  character(len=*), parameter :: GTK_STOCK_GOTO_BOTTOM = &
       & "gtk-goto-bottom"//c_null_char
  character(len=*), parameter :: GTK_STOCK_GOTO_FIRST = &
       & "gtk-goto-first"//c_null_char
  character(len=*), parameter :: GTK_STOCK_GOTO_LAST = &
       & "gtk-goto-last"//c_null_char
  character(len=*), parameter :: GTK_STOCK_GOTO_TOP = &
       & "gtk-goto-top"//c_null_char
  character(len=*), parameter :: GTK_STOCK_GO_BACK = &
       & "gtk-go-back"//c_null_char
  character(len=*), parameter :: GTK_STOCK_GO_DOWN = &
       & "gtk-go-down"//c_null_char
  character(len=*), parameter :: GTK_STOCK_GO_FORWARD = &
       & "gtk-go-forward"//c_null_char
  character(len=*), parameter :: GTK_STOCK_GO_UP = &
       & "gtk-go-up"//c_null_char
  character(len=*), parameter :: GTK_STOCK_HARDDISK = &
       & "gtk-harddisk"//c_null_char
  character(len=*), parameter :: GTK_STOCK_HELP = &
       & "gtk-help"//c_null_char
  character(len=*), parameter :: GTK_STOCK_HOME = &
       & "gtk-home"//c_null_char
  character(len=*), parameter :: GTK_STOCK_INDEX = &
       & "gtk-index"//c_null_char
  character(len=*), parameter :: GTK_STOCK_INDENT = &
       & "gtk-indent"//c_null_char
  character(len=*), parameter :: GTK_STOCK_INFO = &
       & "gtk-info"//c_null_char
  character(len=*), parameter :: GTK_STOCK_ITALIC = &
       & "gtk-italic"//c_null_char
  character(len=*), parameter :: GTK_STOCK_JUMP_TO = &
       & "gtk-jump-to"//c_null_char
  character(len=*), parameter :: GTK_STOCK_JUSTIFY_CENTER = &
       & "gtk-justify-center"//c_null_char
  character(len=*), parameter :: GTK_STOCK_JUSTIFY_FILL = &
       & "gtk-justify-fill"//c_null_char
  character(len=*), parameter :: GTK_STOCK_JUSTIFY_LEFT = &
       & "gtk-justify-left"//c_null_char
  character(len=*), parameter :: GTK_STOCK_JUSTIFY_RIGHT = &
       & "gtk-justify-right"//c_null_char
  character(len=*), parameter :: GTK_STOCK_LEAVE_FULLSCREEN= &
       & "gtk-leave-fullscreen"//c_null_char
  character(len=*), parameter :: GTK_STOCK_MISSING_IMAGE = &
       & "gtk-missing-image"//c_null_char
  character(len=*), parameter :: GTK_STOCK_MEDIA_FORWARD = &
       & "gtk-media-forward"//c_null_char
  character(len=*), parameter :: GTK_STOCK_MEDIA_NEXT = &
       & "gtk-media-next"//c_null_char
  character(len=*), parameter :: GTK_STOCK_MEDIA_PAUSE = &
       & "gtk-media-pause"//c_null_char
  character(len=*), parameter :: GTK_STOCK_MEDIA_PLAY = &
       & "gtk-media-play"//c_null_char
  character(len=*), parameter :: GTK_STOCK_MEDIA_PREVIOUS = &
       & "gtk-media-previous"//c_null_char
  character(len=*), parameter :: GTK_STOCK_MEDIA_RECORD = &
       & "gtk-media-record"//c_null_char
  character(len=*), parameter :: GTK_STOCK_MEDIA_REWIND = &
       & "gtk-media-rewind"//c_null_char
  character(len=*), parameter :: GTK_STOCK_MEDIA_STOP = &
       & "gtk-media-stop"//c_null_char
  character(len=*), parameter :: GTK_STOCK_NETWORK = &
       & "gtk-network"//c_null_char
  character(len=*), parameter :: GTK_STOCK_NEW = &
       & "gtk-new"//c_null_char
  character(len=*), parameter :: GTK_STOCK_NO = &
       & "gtk-no"//c_null_char
  character(len=*), parameter :: GTK_STOCK_OK = &
       & "gtk-ok"//c_null_char
  character(len=*), parameter :: GTK_STOCK_OPEN = &
       & "gtk-open"//c_null_char
  character(len=*), parameter :: GTK_STOCK_ORIENTATION_PORTRAIT= &
       & "gtk-orientation-portrait"//c_null_char
  character(len=*), parameter :: GTK_STOCK_ORIENTATION_LANDSCAPE= &
       & "gtk-orientation-landscape"//c_null_char
  character(len=*), parameter :: GTK_STOCK_ORIENTATION_REVERSE_LANDSCAPE= &
       & "gtk-orientation-reverse-landscape"//c_null_char
  character(len=*), parameter :: GTK_STOCK_ORIENTATION_REVERSE_PORTRAIT= &
       & "gtk-orientation-reverse-portrait"//c_null_char
  character(len=*), parameter :: GTK_STOCK_PAGE_SETUP = &
       & "gtk-page-setup"//c_null_char
  character(len=*), parameter :: GTK_STOCK_PASTE = &
       & "gtk-paste"//c_null_char
  character(len=*), parameter :: GTK_STOCK_PREFERENCES = &
       & "gtk-preferences"//c_null_char
  character(len=*), parameter :: GTK_STOCK_PRINT = &
       & "gtk-print"//c_null_char
  character(len=*), parameter :: GTK_STOCK_PRINT_ERROR = &
       & "gtk-print-error"//c_null_char
  character(len=*), parameter :: GTK_STOCK_PRINT_PAUSED = &
       & "gtk-print-paused"//c_null_char
  character(len=*), parameter :: GTK_STOCK_PRINT_PREVIEW = &
       & "gtk-print-preview"//c_null_char
  character(len=*), parameter :: GTK_STOCK_PRINT_REPORT = &
       & "gtk-print-report"//c_null_char
  character(len=*), parameter :: GTK_STOCK_PRINT_WARNING = &
       & "gtk-print-warning"//c_null_char
  character(len=*), parameter :: GTK_STOCK_PROPERTIES = &
       & "gtk-properties"//c_null_char
  character(len=*), parameter :: GTK_STOCK_QUIT = &
       & "gtk-quit"//c_null_char
  character(len=*), parameter :: GTK_STOCK_REDO = &
       & "gtk-redo"//c_null_char
  character(len=*), parameter :: GTK_STOCK_REFRESH = &
       & "gtk-refresh"//c_null_char
  character(len=*), parameter :: GTK_STOCK_REMOVE = &
       & "gtk-remove"//c_null_char
  character(len=*), parameter :: GTK_STOCK_REVERT_TO_SAVED = &
       & "gtk-revert-to-saved"//c_null_char
  character(len=*), parameter :: GTK_STOCK_SAVE = &
       & "gtk-save"//c_null_char
  character(len=*), parameter :: GTK_STOCK_SAVE_AS = &
       & "gtk-save-as"//c_null_char
  character(len=*), parameter :: GTK_STOCK_SELECT_ALL = &
       & "gtk-select-all"//c_null_char
  character(len=*), parameter :: GTK_STOCK_SELECT_COLOR = &
       & "gtk-select-color"//c_null_char
  character(len=*), parameter :: GTK_STOCK_SELECT_FONT = &
       & "gtk-select-font"//c_null_char
  character(len=*), parameter :: GTK_STOCK_SORT_ASCENDING = &
       & "gtk-sort-ascending"//c_null_char
  character(len=*), parameter :: GTK_STOCK_SORT_DESCENDING = &
       & "gtk-sort-descending"//c_null_char
  character(len=*), parameter :: GTK_STOCK_SPELL_CHECK = &
       & "gtk-spell-check"//c_null_char
  character(len=*), parameter :: GTK_STOCK_STOP = &
       & "gtk-stop"//c_null_char
  character(len=*), parameter :: GTK_STOCK_STRIKETHROUGH = &
       & "gtk-strikethrough"//c_null_char
  character(len=*), parameter :: GTK_STOCK_UNDELETE = &
       & "gtk-undelete"//c_null_char
  character(len=*), parameter :: GTK_STOCK_UNDERLINE = &
       & "gtk-underline"//c_null_char
  character(len=*), parameter :: GTK_STOCK_UNDO = &
       & "gtk-undo"//c_null_char
  character(len=*), parameter :: GTK_STOCK_UNINDENT = &
       & "gtk-unindent"//c_null_char
  character(len=*), parameter :: GTK_STOCK_YES = &
       & "gtk-yes"//c_null_char
  character(len=*), parameter :: GTK_STOCK_ZOOM_100 = &
       & "gtk-zoom-100"//c_null_char
  character(len=*), parameter :: GTK_STOCK_ZOOM_FIT = &
       & "gtk-zoom-fit"//c_null_char
  character(len=*), parameter :: GTK_STOCK_ZOOM_IN = &
       & "gtk-zoom-in"//c_null_char
  character(len=*), parameter :: GTK_STOCK_ZOOM_OUT = &
       & "gtk-zoom-out"//c_null_char


  ! Interfaces for string conversions
  interface convert_c_string
     module procedure convert_c_string_scalar
     module procedure convert_c_string_array
     module procedure convert_c_string_scalar_cptr
     module procedure convert_c_string_array_cptr
  end interface convert_c_string

  public :: c_f_string_copy
!  public :: c_f_string_copy_alloc
!  public :: c_f_string_ptr

  interface c_f_string
     module procedure convert_c_string_scalar
     module procedure convert_c_string_array
     module procedure convert_c_string_scalar_cptr
     module procedure convert_c_string_array_cptr
  end interface c_f_string
  interface f_c_string
     module procedure convert_f_string_a
     module procedure convert_f_string_s
  end interface f_c_string
  interface convert_f_string
     module procedure convert_f_string_a
     module procedure convert_f_string_s
  end interface convert_f_string

  ! Interfaces for logical conversion
  interface f_c_logical
     module procedure f_c_logical4
     module procedure f_c_logical1
  end interface f_c_logical

  ! String conversion routines below lazily use the C std library function 
  ! strlen.  If this is not available for some bizarre reason, then a 
  ! simple index of the null character will suffice.
  ! Contributed by Ian Harvey, 2014.
  interface
    function strlen(str) bind(c, name='strlen')
      use, intrinsic :: iso_c_binding, only: c_size_t, c_ptr
      implicit none
      type(c_ptr), intent(in), value :: str
      integer(c_size_t) :: strlen
    end function strlen
  end interface

!  private :: strlen
!  private :: do_association


contains
  ! These 2 clear_ routines are only needed of you need to re-initialize
  ! the types. The definitions include the intial setting to zero or NULL.

  !+
  subroutine clear_gtktreeiter(iter)
    type(gtktreeiter), intent(inout) :: iter

    ! Clear a tree iterator
    !
    ! ITER: gtktreeiter: required: The iterator to clear
    !-

    iter%intv=0
    iter%p0=C_NULL_PTR
    iter%p1=C_NULL_PTR
    iter%p2=C_NULL_PTR
  end subroutine clear_gtktreeiter

  !+
  subroutine clear_gvalue(gval)
    type(gvalue), intent(inout) :: gval

    ! Clear a GValue
    !
    ! GVAL: gvalue: required: The GValue to clear.
    !-
    gval%il=0
    gval%i64=(/0,0/)
  end subroutine clear_gvalue

  ! Some string conversion routines

  
  ! Create a default character deferred length allocatable copy of the 
  ! value of a c string.
  ! Contributed by Ian Harvey, 2014.
  ! This requires a relatively recent gfortran.
!  subroutine c_f_string_copy_alloc(the_ptr, f_string)
!    type(c_ptr), intent(in) :: the_ptr
!    character(:), intent(out), allocatable :: f_string
!    
!    character(kind=c_char), pointer :: f_array(:)
!    integer :: i
!    
!    call c_f_pointer(the_ptr, f_array, [strlen(the_ptr)])
!    allocate(character(size(f_array)) :: f_string)
!    forall (i = 1:size(f_array)) f_string(i:i) = f_array(i)
!  end subroutine c_f_string_copy_alloc

  ! Create a default character fixed length copy of the value of a c string.
  !
  ! This is probably ok for older gfortran.
  subroutine c_f_string_copy(the_ptr, f_string, status)
    type(c_ptr), intent(in) :: the_ptr
    character(*), intent(out) :: f_string
    integer, intent(out), optional :: status

    character(kind=c_char), pointer :: f_array(:)
    integer :: i

    call c_f_pointer(the_ptr, f_array, [strlen(the_ptr)])

    do i = 1, size(f_array)
      if (i > len(f_string)) then
        if (present(status)) status = -1
        return
      end if
      f_string(i:i) = f_array(i)
    end do

    ! i here is size(f_array) + 1.  Define the remainder of fstring.
    f_string(i:) = ''

    if (present(status)) status = 0
  end subroutine c_f_string_copy

  ! Associate a pointer with a c string.
  !
  ! This requires a Fortran 2003 compiler (a relatively recent gfortran).
!  subroutine c_f_string_ptr(the_ptr, f_string)
!    type(c_ptr), intent(in) :: the_ptr
!    character(:,kind=c_char), intent(out), pointer :: f_string
!    
!    character(kind=c_char), pointer :: f_array(:)
!    
!    call c_f_pointer(the_ptr, f_array, [strlen(the_ptr)])
!    ! Here we rely on sequence association.  f_array is an array expression 
!    ! (one that happens to be an array designator), so it designates an 
!    ! array element sequence of all the elements of the array.  That array 
!    ! element sequence is then associated with an array of different length 
!    ! (but same total number of characters) inside do_association.
!    call do_association(size(f_array), f_array, f_string)
!  end subroutine c_f_string_ptr

  ! Worker routine for c_f_string_ptr
  !
  ! It is processor dependent whether pointers associated with the actual 
  ! argument are associated with the dummy argument inside the procedure.  
  ! It is similarly processor dependent whether pointers associated with a 
  ! dummy argument remain associated after the procedure exits.  But in 
  ! F2003, this is the only way.  In F2008 things are a little better.  In 
  ! F201X they are probably quite ok.
!  subroutine do_association(l, str, f_string)
!    integer, intent(in) :: l
!    character(len=l,kind=c_char), intent(in), target :: str(1)
!    character(:,kind=c_char), intent(out), pointer :: f_string
!    f_string => str(1)
!  end subroutine do_association


  !+
  subroutine convert_c_string_scalar(textptr, f_string, status)
    character(kind=c_char), dimension(:), intent(in) :: textptr
    character(len=*), intent(out) :: f_string
    integer(kind=c_int), intent(out), optional :: status

    ! Convert a null-terminated c-string to  a fortran string
    !
    ! TEXTPTR: string: required:  The C string to be converted.
    ! F_STRING: f_string: required: A Scalar Fortran string.
    ! STATUS: integer: optional: Is set to -1 if the Fortran string
    ! 		is too short.
    !
    ! Usually called via the convert_c_string generic interface.
    !-
    ! Contributed by jtappin and Ian Harvey.

    integer :: i

    do i = 1, size(textptr)
      if (textptr(i) == c_null_char) exit
      if (i > len(f_string)) then
        if (present(status)) status = -1  ! Output string not long enough
        return
      end if
      f_string(i:i)=textptr(i)
    end do

    f_string(i:) = ''

    if (present(status)) status = 0
  end subroutine convert_c_string_scalar

  !+
  subroutine convert_c_string_array(textptr, f_string, status)
    character(kind=c_char), dimension(:), intent(in) :: textptr
    character(len=*), intent(out), dimension(:), allocatable :: f_string
    integer, intent(out), optional :: status
 
    ! Convert a null-terminated LF-separated c-string into a fortran
    ! string array
    !
    ! TEXTPTR: string: required:  The C string to be converted.
    ! F_STRING: f_string(): required: A Fortran string array.
    ! STATUS: integer: optional: Is set to -1 if the Fortran string
    ! 		is too short for any line.
    !
    ! Usually called via the convert_c_string generic interface.
    !-

    integer :: i, j, ii, count

    count = 1
    i = 1
    do
       if (i > size(textptr)) exit
       if (textptr(i) == c_null_char) exit
       if (textptr(i) == c_new_line) count = count+1
       i = i+1
    end do
    allocate(f_string(count))

    if (present(status)) status = 0
    ii = 1
    do j = 1, count
       f_string(j) = ""
       do i = 1, len(f_string)
          if (ii > size(textptr)) then
             if (j < count .and. present(status)) status=-1
             return
          end if

          if (textptr(ii) == c_null_char) return
          if (textptr(ii) == c_new_line) then
             ii = ii+1
             exit
          end if
          f_string(j)(i:i)=textptr(ii)
          ii = ii+1
       end do
       if (i > len(f_string) .and. present(status)) &
            & status = -1  ! Output string not long enough
    end do
  end subroutine convert_c_string_array


  !+
  subroutine convert_c_string_scalar_cptr(ctext, f_string, status)
    type(c_ptr), intent(in) :: ctext
    character(len=*), intent(out) :: f_string
    integer, intent(out), optional :: status

    ! Convert a null-terminated c-string to  a fortran string
    !
    ! CTEXT: c_ptr: required:  A C poiner to string to be converted.
    ! F_STRING: f_string: required: A Scalar Fortran string.
    ! STATUS: integer: optional: Is set to -1 if the Fortran string
    ! 		is too short.
    !
    ! Usually called via the convert_c_string generic interface.
    !-
    ! Contributed by jtappin and Ian Harvey.

    integer :: i
    character(kind=c_char), dimension(:), pointer :: textptr

    call c_f_pointer(ctext, textptr, (/ strlen(ctext) /))

    do i = 1, size(textptr)
      if (i > len(f_string)) then
        if (present(status)) status = -1  ! Output string not long enough
        return
      end if
      f_string(i:i)=textptr(i)
    end do

    f_string(i:) = ''
    if (present(status)) status = 0

  end subroutine convert_c_string_scalar_cptr


  !+
  subroutine convert_c_string_array_cptr(ctext, f_string, status)
    type(c_ptr), intent(in) :: ctext
    character(len=*), intent(out), dimension(:), allocatable :: f_string
    integer, intent(out), optional :: status

    ! Convert a null-terminated LF-separated c-string into a fortran
    ! string array
    ! CTEXT: c_ptr: required:  A C poiner to string to be converted.
    ! F_STRING: f_string(): required: A  Fortran string. array
    ! STATUS: integer: optional: Is set to -1 if the Fortran string
    ! 		is too short for any of the lines.
    !
    ! Usually called via the convert_c_string generic interface.
    !-

    integer :: i, j, ii, count
    character(kind=c_char), dimension(:), pointer :: textptr

    call c_f_pointer(ctext, textptr, (/ strlen(ctext) /))
    ! count = COUNT(textptr == c_new_line)
    count = 1
    i = 1
    do
       if (i > size(textptr)) exit
       if (textptr(i) == c_new_line) count = count+1
       i = i+1
    end do
    allocate(f_string(count))

    if (present(status)) status = 0
    ii = 1
    do j = 1, count
       f_string(j) = ""
       do i = 1, len(f_string)
          if (ii > size(textptr)) then
             return
          end if

          if (textptr(ii) == c_new_line) then
             ii = ii+1
             exit
          end if
          f_string(j)(i:i)=textptr(ii)
          ii = ii+1
       end do
       if (i > len(f_string) .and. present(status)) &
            & status = -1  ! Output string not long enough
    end do
  end subroutine convert_c_string_array_cptr

  !+
  !+
  subroutine convert_f_string_a(f_string, textptr, length)
    character(len=*), intent(in), dimension(:) :: f_string
    character(kind=c_char), dimension(:), intent(out), allocatable :: textptr
    integer(kind=c_int), intent(out), optional :: length

    ! Convert a fortran string array into a null-terminated, LF_separated
    ! c-string
    !
    ! F_STRING: f_string: required: The fortran string to convert
    ! TEXTPR: string: required: A C type string, (allocatable).
    ! LENGTH: c_int: optional: The length of the generated c string.
    !-

    integer :: lcstr, i, j, ii, nfstr
    integer, dimension(:), allocatable :: lfstr

    nfstr = size(f_string)
    allocate(lfstr(nfstr))
    lfstr = len_trim(f_string)

    lcstr = sum(lfstr) 
    do i = 1, nfstr-1
       if (lfstr(i) == 0) then
          lcstr = lcstr+1
       else if (f_string(i)(lfstr(i):lfstr(i)) /= c_null_char .and. &
            & f_string(i)(lfstr(i):lfstr(i)) /= c_new_line) then
          lcstr = lcstr+1
       end if
    end do
    if (lfstr(nfstr) == 0) then
       lcstr = lcstr+1
    else if (f_string(nfstr)(lfstr(nfstr):lfstr(nfstr)) /= c_null_char) then
       lcstr = lcstr+1
    end if

    allocate(textptr(lcstr))
    if (present(length)) length = lcstr

    ii = 1
    do i = 1, nfstr
       do j = 1, lfstr(i)
          if (j == lfstr(i) .and. &
               & (f_string(i)(j:j) == c_null_char .or. &
               & (f_string(i)(j:j) == c_new_line .and. i /= nfstr))) exit
          textptr(ii) = f_string(i)(j:j)
          ii = ii+1
       end do
       if (i < nfstr) then
          textptr(ii) = c_new_line
       else
          textptr(ii) = c_null_char
       end if
       ii = ii+1
    end do
  end subroutine convert_f_string_a
  !+
  subroutine convert_f_string_s(f_string, textptr, length)
    character(len=*), intent(in) :: f_string
    character(kind=c_char), dimension(:), intent(out), allocatable :: textptr
    integer(kind=c_int), intent(out), optional :: length

    ! Convert a fortran string into a null-terminated c-string
    !
    ! F_STRING: f_string: required: The fortran string to convert
    ! TEXTPR: string: required: A C type string, (allocatable).
    ! LENGTH: c_int: optional: The length of the generated c string.
    !-

    integer :: lcstr, j
    logical :: add_null

    lcstr = len_trim(f_string)
    if (lcstr == 0) then
       lcstr = lcstr+1
       add_null = .true.
    else if (f_string(lcstr:lcstr) /= c_null_char) then
       lcstr = lcstr+1
       add_null = .true.
    else
       add_null = .false.
    end if

    allocate(textptr(lcstr))
    if (present(length)) length = lcstr

    do j = 1, len_trim(f_string)
       textptr(j) = f_string(j:j)
    end do
    if (add_null) textptr(lcstr) = c_null_char

  end subroutine convert_f_string_s

  !+
  function c_f_logical(cbool)
    logical :: c_f_logical
    integer(kind=c_int), intent(in) :: cbool

    ! Convert a gboolean to a Fortran logical
    !
    ! CBOOL: boolean: required: The Gboolean to convert.
    !-

    if (cbool == FALSE) then
       c_f_logical = .false.
    else
       c_f_logical = .true.
    end if

  end function c_f_logical

  !+
  function f_c_logical4(flog)
    integer(kind=c_int) :: f_c_logical4
    logical, intent(in) :: flog

    ! Convert a Fortran default logical to a gboolean
    !
    ! FLOG: logical: required: The fortran logical to convert.
    !
    ! Usually accessed via the generic f_c_logical interface
    !-

    if (flog) then
       f_c_logical4 = TRUE
    else
       f_c_logical4 = FALSE
    end if
  end function f_c_logical4

  !+
  function f_c_logical1(flog)
    integer(kind=c_int) :: f_c_logical1
    logical(kind=1), intent(in) :: flog

    ! Convert a Fortran 1-byte logical to a gboolean
    !
    ! FLOG: logical*1: required: The fortran logical to convert.
    !
    ! Usually accessed via the generic f_c_logical interface
    !-

    if (flog) then
       f_c_logical1 = TRUE
    else
       f_c_logical1 = FALSE
    end if
  end function f_c_logical1

end module gtk_sup
