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
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Contributed by James Tappin
! Last modification: 04-17-2011

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
  ! Various GTK_STOCK strings.

  use iso_c_binding
  use gtk, only: NULL, CNULL
  use g, only: alloca, g_type_fundamental

  implicit none

  !+
  ! Gtype
  ! The various Gtype definitions. 
  !-

  ! Gtype definitions
  integer, parameter :: type_kind=c_long
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
  type(c_ptr) :: p0=NULL, p1=NULL, p2=NULL
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
! GTK_STOCK
! Stock button and icon names extracted from gtk/gtkstock.h
! N.B. GTK_STOCK_ADD clashes with the routine gtk_stock_add and is therefore
! omitted.
!-

character(len=len("gtk-about")+1), parameter :: GTK_STOCK_ABOUT = &
& "gtk-about"//cnull
!!$  GTK_STOCK_ADD clashes with the routine gtk_stock_add
!!$  character(len=len("gtk-add")+1), parameter :: GTK_STOCK_ADD = &
!!$       & "gtk-add"//cnull
character(len=len("gtk-apply")+1), parameter :: GTK_STOCK_APPLY = &
& "gtk-apply"//cnull
character(len=len("gtk-bold")+1), parameter :: GTK_STOCK_BOLD = &
& "gtk-bold"//cnull
character(len=len("gtk-cancel")+1), parameter :: GTK_STOCK_CANCEL = &
& "gtk-cancel"//cnull
character(len=len("gtk-caps-lock-warning")+1), parameter :: GTK_STOCK_CAPS_LOCK_WARNING= &
& "gtk-caps-lock-warning"//cnull
character(len=len("gtk-cdrom")+1), parameter :: GTK_STOCK_CDROM = &
& "gtk-cdrom"//cnull
character(len=len("gtk-clear")+1), parameter :: GTK_STOCK_CLEAR = &
& "gtk-clear"//cnull
character(len=len("gtk-close")+1), parameter :: GTK_STOCK_CLOSE = &
& "gtk-close"//cnull
character(len=len("gtk-color-picker")+1), parameter :: GTK_STOCK_COLOR_PICKER = &
& "gtk-color-picker"//cnull
character(len=len("gtk-connect")+1), parameter :: GTK_STOCK_CONNECT = &
& "gtk-connect"//cnull
character(len=len("gtk-convert")+1), parameter :: GTK_STOCK_CONVERT = &
& "gtk-convert"//cnull
character(len=len("gtk-copy")+1), parameter :: GTK_STOCK_COPY = &
& "gtk-copy"//cnull
character(len=len("gtk-cut")+1), parameter :: GTK_STOCK_CUT = &
& "gtk-cut"//cnull
character(len=len("gtk-delete")+1), parameter :: GTK_STOCK_DELETE = &
& "gtk-delete"//cnull
character(len=len("gtk-dialog-authentication")+1), parameter :: GTK_STOCK_DIALOG_AUTHENTICATION= &
& "gtk-dialog-authentication"//cnull
character(len=len("gtk-dialog-info")+1), parameter :: GTK_STOCK_DIALOG_INFO = &
& "gtk-dialog-info"//cnull
character(len=len("gtk-dialog-warning")+1), parameter :: GTK_STOCK_DIALOG_WARNING = &
& "gtk-dialog-warning"//cnull
character(len=len("gtk-dialog-error")+1), parameter :: GTK_STOCK_DIALOG_ERROR = &
& "gtk-dialog-error"//cnull
character(len=len("gtk-dialog-question")+1), parameter :: GTK_STOCK_DIALOG_QUESTION = &
& "gtk-dialog-question"//cnull
character(len=len("gtk-directory")+1), parameter :: GTK_STOCK_DIRECTORY = &
& "gtk-directory"//cnull
character(len=len("gtk-discard")+1), parameter :: GTK_STOCK_DISCARD = &
& "gtk-discard"//cnull
character(len=len("gtk-disconnect")+1), parameter :: GTK_STOCK_DISCONNECT = &
& "gtk-disconnect"//cnull
character(len=len("gtk-dnd")+1), parameter :: GTK_STOCK_DND = &
& "gtk-dnd"//cnull
character(len=len("gtk-dnd-multiple")+1), parameter :: GTK_STOCK_DND_MULTIPLE = &
& "gtk-dnd-multiple"//cnull
character(len=len("gtk-edit")+1), parameter :: GTK_STOCK_EDIT = &
& "gtk-edit"//cnull
character(len=len("gtk-execute")+1), parameter :: GTK_STOCK_EXECUTE = &
& "gtk-execute"//cnull
character(len=len("gtk-file")+1), parameter :: GTK_STOCK_FILE = &
& "gtk-file"//cnull
character(len=len("gtk-find")+1), parameter :: GTK_STOCK_FIND = &
& "gtk-find"//cnull
character(len=len("gtk-find-and-replace")+1), parameter :: GTK_STOCK_FIND_AND_REPLACE= &
& "gtk-find-and-replace"//cnull
character(len=len("gtk-floppy")+1), parameter :: GTK_STOCK_FLOPPY = &
& "gtk-floppy"//cnull
character(len=len("gtk-fullscreen")+1), parameter :: GTK_STOCK_FULLSCREEN = &
& "gtk-fullscreen"//cnull
character(len=len("gtk-goto-bottom")+1), parameter :: GTK_STOCK_GOTO_BOTTOM = &
& "gtk-goto-bottom"//cnull
character(len=len("gtk-goto-first")+1), parameter :: GTK_STOCK_GOTO_FIRST = &
& "gtk-goto-first"//cnull
character(len=len("gtk-goto-last")+1), parameter :: GTK_STOCK_GOTO_LAST = &
& "gtk-goto-last"//cnull
character(len=len("gtk-goto-top")+1), parameter :: GTK_STOCK_GOTO_TOP = &
& "gtk-goto-top"//cnull
character(len=len("gtk-go-back")+1), parameter :: GTK_STOCK_GO_BACK = &
& "gtk-go-back"//cnull
character(len=len("gtk-go-down")+1), parameter :: GTK_STOCK_GO_DOWN = &
& "gtk-go-down"//cnull
character(len=len("gtk-go-forward")+1), parameter :: GTK_STOCK_GO_FORWARD = &
& "gtk-go-forward"//cnull
character(len=len("gtk-go-up")+1), parameter :: GTK_STOCK_GO_UP = &
& "gtk-go-up"//cnull
character(len=len("gtk-harddisk")+1), parameter :: GTK_STOCK_HARDDISK = &
& "gtk-harddisk"//cnull
character(len=len("gtk-help")+1), parameter :: GTK_STOCK_HELP = &
& "gtk-help"//cnull
character(len=len("gtk-home")+1), parameter :: GTK_STOCK_HOME = &
& "gtk-home"//cnull
character(len=len("gtk-index")+1), parameter :: GTK_STOCK_INDEX = &
& "gtk-index"//cnull
character(len=len("gtk-indent")+1), parameter :: GTK_STOCK_INDENT = &
& "gtk-indent"//cnull
character(len=len("gtk-info")+1), parameter :: GTK_STOCK_INFO = &
& "gtk-info"//cnull
character(len=len("gtk-italic")+1), parameter :: GTK_STOCK_ITALIC = &
& "gtk-italic"//cnull
character(len=len("gtk-jump-to")+1), parameter :: GTK_STOCK_JUMP_TO = &
& "gtk-jump-to"//cnull
character(len=len("gtk-justify-center")+1), parameter :: GTK_STOCK_JUSTIFY_CENTER = &
& "gtk-justify-center"//cnull
character(len=len("gtk-justify-fill")+1), parameter :: GTK_STOCK_JUSTIFY_FILL = &
& "gtk-justify-fill"//cnull
character(len=len("gtk-justify-left")+1), parameter :: GTK_STOCK_JUSTIFY_LEFT = &
& "gtk-justify-left"//cnull
character(len=len("gtk-justify-right")+1), parameter :: GTK_STOCK_JUSTIFY_RIGHT = &
& "gtk-justify-right"//cnull
character(len=len("gtk-leave-fullscreen")+1), parameter :: GTK_STOCK_LEAVE_FULLSCREEN= &
& "gtk-leave-fullscreen"//cnull
character(len=len("gtk-missing-image")+1), parameter :: GTK_STOCK_MISSING_IMAGE = &
& "gtk-missing-image"//cnull
character(len=len("gtk-media-forward")+1), parameter :: GTK_STOCK_MEDIA_FORWARD = &
& "gtk-media-forward"//cnull
character(len=len("gtk-media-next")+1), parameter :: GTK_STOCK_MEDIA_NEXT = &
& "gtk-media-next"//cnull
character(len=len("gtk-media-pause")+1), parameter :: GTK_STOCK_MEDIA_PAUSE = &
& "gtk-media-pause"//cnull
character(len=len("gtk-media-play")+1), parameter :: GTK_STOCK_MEDIA_PLAY = &
& "gtk-media-play"//cnull
character(len=len("gtk-media-previous")+1), parameter :: GTK_STOCK_MEDIA_PREVIOUS = &
& "gtk-media-previous"//cnull
character(len=len("gtk-media-record")+1), parameter :: GTK_STOCK_MEDIA_RECORD = &
& "gtk-media-record"//cnull
character(len=len("gtk-media-rewind")+1), parameter :: GTK_STOCK_MEDIA_REWIND = &
& "gtk-media-rewind"//cnull
character(len=len("gtk-media-stop")+1), parameter :: GTK_STOCK_MEDIA_STOP = &
& "gtk-media-stop"//cnull
character(len=len("gtk-network")+1), parameter :: GTK_STOCK_NETWORK = &
& "gtk-network"//cnull
character(len=len("gtk-new")+1), parameter :: GTK_STOCK_NEW = &
& "gtk-new"//cnull
character(len=len("gtk-no")+1), parameter :: GTK_STOCK_NO = &
& "gtk-no"//cnull
character(len=len("gtk-ok")+1), parameter :: GTK_STOCK_OK = &
& "gtk-ok"//cnull
character(len=len("gtk-open")+1), parameter :: GTK_STOCK_OPEN = &
& "gtk-open"//cnull
character(len=len("gtk-orientation-portrait")+1), parameter :: GTK_STOCK_ORIENTATION_PORTRAIT= &
& "gtk-orientation-portrait"//cnull
character(len=len("gtk-orientation-landscape")+1), parameter :: GTK_STOCK_ORIENTATION_LANDSCAPE= &
& "gtk-orientation-landscape"//cnull
character(len=len("gtk-orientation-reverse-landscape")+1), parameter :: GTK_STOCK_ORIENTATION_REVERSE_LANDSCAPE= &
& "gtk-orientation-reverse-landscape"//cnull
character(len=len("gtk-orientation-reverse-portrait")+1), parameter :: GTK_STOCK_ORIENTATION_REVERSE_PORTRAIT= &
& "gtk-orientation-reverse-portrait"//cnull
character(len=len("gtk-page-setup")+1), parameter :: GTK_STOCK_PAGE_SETUP = &
& "gtk-page-setup"//cnull
character(len=len("gtk-paste")+1), parameter :: GTK_STOCK_PASTE = &
& "gtk-paste"//cnull
character(len=len("gtk-preferences")+1), parameter :: GTK_STOCK_PREFERENCES = &
& "gtk-preferences"//cnull
character(len=len("gtk-print")+1), parameter :: GTK_STOCK_PRINT = &
& "gtk-print"//cnull
character(len=len("gtk-print-error")+1), parameter :: GTK_STOCK_PRINT_ERROR = &
& "gtk-print-error"//cnull
character(len=len("gtk-print-paused")+1), parameter :: GTK_STOCK_PRINT_PAUSED = &
& "gtk-print-paused"//cnull
character(len=len("gtk-print-preview")+1), parameter :: GTK_STOCK_PRINT_PREVIEW = &
& "gtk-print-preview"//cnull
character(len=len("gtk-print-report")+1), parameter :: GTK_STOCK_PRINT_REPORT = &
& "gtk-print-report"//cnull
character(len=len("gtk-print-warning")+1), parameter :: GTK_STOCK_PRINT_WARNING = &
& "gtk-print-warning"//cnull
character(len=len("gtk-properties")+1), parameter :: GTK_STOCK_PROPERTIES = &
& "gtk-properties"//cnull
character(len=len("gtk-quit")+1), parameter :: GTK_STOCK_QUIT = &
& "gtk-quit"//cnull
character(len=len("gtk-redo")+1), parameter :: GTK_STOCK_REDO = &
& "gtk-redo"//cnull
character(len=len("gtk-refresh")+1), parameter :: GTK_STOCK_REFRESH = &
& "gtk-refresh"//cnull
character(len=len("gtk-remove")+1), parameter :: GTK_STOCK_REMOVE = &
& "gtk-remove"//cnull
character(len=len("gtk-revert-to-saved")+1), parameter :: GTK_STOCK_REVERT_TO_SAVED = &
& "gtk-revert-to-saved"//cnull
character(len=len("gtk-save")+1), parameter :: GTK_STOCK_SAVE = &
& "gtk-save"//cnull
character(len=len("gtk-save-as")+1), parameter :: GTK_STOCK_SAVE_AS = &
& "gtk-save-as"//cnull
character(len=len("gtk-select-all")+1), parameter :: GTK_STOCK_SELECT_ALL = &
& "gtk-select-all"//cnull
character(len=len("gtk-select-color")+1), parameter :: GTK_STOCK_SELECT_COLOR = &
& "gtk-select-color"//cnull
character(len=len("gtk-select-font")+1), parameter :: GTK_STOCK_SELECT_FONT = &
& "gtk-select-font"//cnull
character(len=len("gtk-sort-ascending")+1), parameter :: GTK_STOCK_SORT_ASCENDING = &
& "gtk-sort-ascending"//cnull
character(len=len("gtk-sort-descending")+1), parameter :: GTK_STOCK_SORT_DESCENDING = &
& "gtk-sort-descending"//cnull
character(len=len("gtk-spell-check")+1), parameter :: GTK_STOCK_SPELL_CHECK = &
& "gtk-spell-check"//cnull
character(len=len("gtk-stop")+1), parameter :: GTK_STOCK_STOP = &
& "gtk-stop"//cnull
character(len=len("gtk-strikethrough")+1), parameter :: GTK_STOCK_STRIKETHROUGH = &
& "gtk-strikethrough"//cnull
character(len=len("gtk-undelete")+1), parameter :: GTK_STOCK_UNDELETE = &
& "gtk-undelete"//cnull
character(len=len("gtk-underline")+1), parameter :: GTK_STOCK_UNDERLINE = &
& "gtk-underline"//cnull
character(len=len("gtk-undo")+1), parameter :: GTK_STOCK_UNDO = &
& "gtk-undo"//cnull
character(len=len("gtk-unindent")+1), parameter :: GTK_STOCK_UNINDENT = &
& "gtk-unindent"//cnull
character(len=len("gtk-yes")+1), parameter :: GTK_STOCK_YES = &
& "gtk-yes"//cnull
character(len=len("gtk-zoom-100")+1), parameter :: GTK_STOCK_ZOOM_100 = &
& "gtk-zoom-100"//cnull
character(len=len("gtk-zoom-fit")+1), parameter :: GTK_STOCK_ZOOM_FIT = &
& "gtk-zoom-fit"//cnull
character(len=len("gtk-zoom-in")+1), parameter :: GTK_STOCK_ZOOM_IN = &
& "gtk-zoom-in"//cnull
character(len=len("gtk-zoom-out")+1), parameter :: GTK_STOCK_ZOOM_OUT = &
& "gtk-zoom-out"//cnull


! Interfaces for string conversions
interface convert_c_string
module procedure convert_c_string_scalar
module procedure convert_c_string_array
module procedure convert_c_string_scalar_cptr
module procedure convert_c_string_array_cptr
end interface convert_c_string

contains
  ! These 2 clear_ routines are only needed of you need to re-initialize
  ! the types. The definitions include the intial setting to zero or NULL.

  !+
  subroutine clear_gtktreeiter(iter)
    ! Clear a tree iterator
    !
    ! ITER: gtktreeiter: required: The iterator to clear
    !-
    type(gtktreeiter), intent(inout) :: iter

    iter%intv=0
    iter%p0=NULL
    iter%p1=NULL
    iter%p2=NULL
  end subroutine clear_gtktreeiter

  !+
  subroutine clear_gvalue(gval)
    ! Clear a GValue
    !
    ! GVAL: gvalue: required: The GValue to clear.
    !-
    type(gvalue) :: gval
    gval%il=0
    gval%i64=(/0,0/)
  end subroutine clear_gvalue

! Some string conversion routines

!+
subroutine convert_c_string_scalar(textptr, f_string, status)
  ! Convert a null-terminated c-string to  a fortran string
  !
  ! TEXTPTR: string: required:  The C string to be converted.
  ! F_STRING: f_string: required: A Scalar Fortran string.
  ! STATUS: integer: optional: Is set to -1 if the Fortran string
  ! 		is too short.
  !
  ! Usually called via the convert_c_string generic interface.
  !-
use iso_c_binding, only: c_char
implicit none
character(kind=c_char), dimension(:), intent(in) :: textptr
character(len=*), intent(out) :: f_string
integer, intent(out), optional :: status
integer :: i

f_string=""

if (present(status)) status = 0
do i = 1, len(f_string)
if (textptr(i) == cnull) return
f_string(i:i)=textptr(i)
end do
if (present(status)) status = -1  ! Output string not long enough
end subroutine convert_c_string_scalar

!+
subroutine convert_c_string_array(textptr, f_string, status)
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
use iso_c_binding, only: c_char
implicit none
character(kind=c_char), dimension(:), intent(in) :: textptr
character(len=*), intent(out), dimension(:), allocatable :: f_string
integer, intent(out), optional :: status
integer :: i, j, ii, count

count = 1
i = 1
do
if (textptr(i) == cnull) exit
if (textptr(i) == c_new_line) count = count+1
i = i+1
end do
allocate(f_string(count))

if (present(status)) status = 0
ii = 1
do j = 1, count
f_string(j) = ""
do i = 1, len(f_string)
 if (textptr(ii) == cnull) return
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
subroutine convert_c_string_scalar_cptr(ctext, clen, f_string, status)
  ! Convert a null-terminated c-string to  a fortran string
  !
  ! CTEXT: c_ptr: required:  A C poiner to string to be converted.
  ! CLEN: c_int: required: The length of the string (or of the Fortran
  ! 		string if the C-string is of unknow length,=.
  ! F_STRING: f_string: required: A Scalar Fortran string.
  ! STATUS: integer: optional: Is set to -1 if the Fortran string
  ! 		is too short.
  !
  ! Usually called via the convert_c_string generic interface.
  !-

type(c_ptr), intent(in) :: ctext
integer(kind=c_int), intent(in) :: clen
character(len=*), intent(out) :: f_string
integer, intent(out), optional :: status

integer :: i
character(kind=c_char), dimension(:), pointer :: textptr

call c_f_pointer(ctext, textptr, (/clen/))

f_string=""

if (present(status)) status = 0
do i = 1, len(f_string)
   if (textptr(i) == cnull) return
   f_string(i:i)=textptr(i)
end do
if (present(status)) status = -1  ! Output string not long enough
end subroutine convert_c_string_scalar_cptr


!+
subroutine convert_c_string_array_cptr(ctext, clen, f_string, status)
  ! Convert a null-terminated LF-separated c-string into a fortran
  ! string array
  ! CTEXT: c_ptr: required:  A C poiner to string to be converted.
  ! CLEN: c_int: required: The length of the string (or of the Fortran
  ! 		string if the C-string is of unknow length,=.
  ! F_STRING: f_string(): required: A  Fortran string. array
  ! STATUS: integer: optional: Is set to -1 if the Fortran string
  ! 		is too short for any of the lines.
  !
  ! Usually called via the convert_c_string generic interface.
  !-

type(c_ptr), intent(in) :: ctext
integer(kind=c_int), intent(in) :: clen
character(len=*), intent(out), dimension(:), allocatable :: f_string
integer, intent(out), optional :: status

integer :: i, j, ii, count
character(kind=c_char), dimension(:), pointer :: textptr

call c_f_pointer(ctext, textptr, (/clen/))

count = 1
i = 1
do
if (textptr(i) == cnull) exit
if (textptr(i) == c_new_line) count = count+1
i = i+1
end do
allocate(f_string(count))

if (present(status)) status = 0
ii = 1
do j = 1, count
f_string(j) = ""
do i = 1, len(f_string)
 if (textptr(ii) == cnull) return
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
subroutine convert_f_string(f_string, textptr, length)
  ! Convert a fortran string array into a null-terminated, LF_separated
  ! c-string
  !
  ! F_STRING: f_string: required: The fortran string to convert
  ! TEXTPR: string: required: A C tyoe string, (allocatable).
  ! LENGTH: c_int: optional: The lenght of the generated c string.
  !-
character(len=*), intent(in), dimension(:) :: f_string
character(kind=c_char), dimension(:), intent(out), allocatable :: textptr
integer(kind=c_int), intent(out), optional :: length

integer :: lcstr, i, j, ii

lcstr = 0
do i = 1, size(f_string)
lcstr = lcstr + len_trim(f_string(i))+1 ! The +1 is for the LF and NULL characters
end do

allocate(textptr(lcstr))
if (present(length)) length = lcstr

ii = 1
do i = 1, size(f_string)
do j = 1, len_trim(f_string(i))
 textptr(ii) = f_string(i)(j:j)
 ii = ii+1
end do
if (i < size(f_string)) then
 textptr(ii) = c_new_line
else
 textptr(ii) = cnull
end if
ii = ii+1
end do
end subroutine convert_f_string
end module gtk_sup
