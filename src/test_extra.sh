#!/bin/sh
# This file is part of gtk-fortran, a GTK / Fortran interface library.
# Copyright (C) 2016 The gtk-fortran team
#
# An interactive script to test projects gtk-fortran-extra
# and gtkzero_fpm before release
#
# Contributed by Vincent MAGNIN, 2023-03-20
# Last modification: 2023-03-20

# For a safer script:
set -u

echo '-------------------------------------------------------------------------'
echo 'gtk-fortran installed version:'
echo '-------------------------------------------------------------------------'
gtk-4-fortran | head -n 2

echo '--------------------------------------------------------------------'
echo 'Do you want to install the gtk-fortran library on your system? (y/N)'
read -r answer
case ${answer} in
'Y' | 'y')
    sudo make install
    ;;
*)
    echo 'No installation'
    ;;
esac

echo '-------------------------------------------------------------------------'
echo 'Do you want to test gtk-fortran-extra? (y/N)'
read -r answer
case ${answer} in
'Y' | 'y')
    cd /tmp
    git clone git@github.com:vmagnin/gtk-fortran-extra.git
    cd gtk-fortran-extra
    echo '******************'
    echo 'my_fast_app'
    cd my_fast_app && ./with_GUI.sh
    echo '******************'
    echo 'my_long_app'
    cd ../my_long_app && ./with_GUI.sh
    echo '******************'
    echo 'unknown_pleasures'
    cd ../unknown_pleasures && ./build.sh && ./a.out
    echo '******************'
    echo 'parallel_app'
    cd ../parallel_app && ./build.sh
    ;;
*)
    echo 'Not tested: gtk-fortran-extra'
    ;;
esac

echo '-----------------------------------------------------------------------'
echo 'Do you want to test gtkzero_fpm with the dev branch gtk4-vmagnin? (y/N)'
read -r answer
case ${answer} in
'Y' | 'y')
    cd /tmp
    git clone git@github.com:vmagnin/gtkzero_fpm.git
    cd gtkzero_fpm
    sed -i 's/branch = "gtk4"/branch = "gtk4-vmagnin"/g' fpm.toml
    fpm run
    ;;
*)
    echo 'Not tested: gtkzero_fpm'
    ;;
esac
