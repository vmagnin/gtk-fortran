#! /bin/sh
# Show libraries and tools versions used in gtk-fortran
# Parameters: none
# Contributed by Vincent MAGNIN, 2019-03-13
# Updated 2019-03-25
# Needs: in Fedora, lsb_release is in the package redhat-lsb-core 

echo "======="
echo "SYSTEM:"
echo "======="

# Distribution (Linux only):
lsb_release -sd
# Kernel, release, processor, OS:
uname -srpo

echo "=========="
echo "LIBRARIES:"
echo "=========="

# Default package names:
LIB_GTK2="libgtk2.0-dev"
LIB_GTK3="libgtk-3-dev"
LIB_GLIB="libglib2.0-dev"
LIB_PLPLOT="libplplot-dev"

if [ $(uname -srpo | grep -c MINGW) != 0 ]; then
    pacman -Q mingw-w64-x86_64-gtk2
    pacman -Q mingw-w64-x86_64-gtk3	
    pacman -Q mingw-w64-x86_64-glib2
    pacman -Q mingw-w64-x86_64-plplot
elif [ $(uname -srpo | grep -c fc) != 0 ]; then
    dnf info --installed gtk2-devel | grep Source
    dnf info --installed gtk3-devel | grep Source
    dnf info --installed glib2 | grep Source
    dnf info --installed plplot-devel | grep Source
elif [ $(uname -srpo | grep -c FreeBSD) != 0 ]; then
    pkg info gtk2 | grep gtk2-
    pkg info gtk3 | grep gtk3-
    pkg info glib | grep ^glib
    pkg info plplot | grep plplot-
else
    dpkg-query --show $LIB_GTK2
    dpkg-query --show $LIB_GTK3
    dpkg-query --show $LIB_GLIB
    dpkg-query --show $LIB_PLPLOT
fi

echo "=========="
echo "LANGUAGES:"
echo "=========="

if [ $(uname -srpo | grep -c FreeBSD) != 0 ]; then
    gfortran8 --version | head -n 1
else
    gfortran --version | head -n 1
fi

python3 --version
perl --version | head -n 2 | tail -n 1
bash --version | head -n 1

echo "============"
echo "BUILD TOOLS:"
echo "============"

cmake --version | head -n 1
echo "pkg-config $(pkg-config --version)"
git --version

echo "============"
echo "OTHER TOOLS:"
echo "============"
