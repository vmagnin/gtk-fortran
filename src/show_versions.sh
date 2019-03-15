#! /bin/sh
# Show libraries and tools versions used in gtk-fortran
# Parameters: none
# Contributed by Vincent MAGNIN, 2019-03-13
# Updated 2019-03-15

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

if [ $(lsb_release -sd | grep -c Fedora) != 0 ]; then
#     LIB_GTK2="libgtk2.0-dev"
#     LIB_GTK3="libgtk-3-dev"
#     LIB_GLIB="libglib2.0-dev"
#     LIB_PLPLOT="libplplot-dev"
    echo
fi

dpkg-query --show $LIB_GTK2
dpkg-query --show $LIB_GTK3
dpkg-query --show $LIB_GLIB
dpkg-query --show $LIB_PLPLOT

echo "=========="
echo "LANGUAGES:"
echo "=========="

gfortran --version | head -n 1
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
