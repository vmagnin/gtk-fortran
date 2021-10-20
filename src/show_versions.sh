#! /bin/sh
# Show libraries and tools versions used in gtk-fortran
# Parameters: none
# Contributed by Vincent MAGNIN, 2019-03-13
# Updated 2021-10-20
# Needs: in Fedora, lsb_release is in the package redhat-lsb-core 

# The shell -e option is not used in this script to avoid exiting each time a
# command is not installed on a specific system:
set -u

echo "======="
echo "SYSTEM:"
echo "======="

# Distribution (Linux only):
readonly RELEASE="$(lsb_release -sd)"
echo "${RELEASE}"
# Kernel, release, processor, OS:
readonly SYSTEM="$(uname -srpo)"
echo "${SYSTEM}"

echo "=========="
echo "LIBRARIES:"
echo "=========="

# Default package names (Debian/Ubuntu):
readonly LIB_GTK2="libgtk2.0-dev"
readonly LIB_GTK3="libgtk-3-dev"
readonly LIB_GTK4="libgtk-4-dev"
readonly LIB_GLIB="libglib2.0-dev"
readonly LIB_PLPLOT="libplplot-dev"

if echo "${SYSTEM}" | grep -q MINGW ; then
    pacman -Q mingw-w64-x86_64-gtk2
    pacman -Q mingw-w64-x86_64-gtk3	
    pacman -Q mingw-w64-x86_64-glib2
    pacman -Q mingw-w64-x86_64-plplot
elif echo "${SYSTEM}" | grep -q MANJARO ; then
    pacman -Q gtk2
    pacman -Q gtk3
    pacman -Q glib2
    pacman -Q plplot
elif echo "${SYSTEM}" | grep -q fc ; then    # Fedora
    dnf info --installed gtk2-devel | grep Source
    dnf info --installed gtk3-devel | grep Source
    dnf info --installed gtk4-devel | grep Source
    dnf info --installed glib2 | grep Source
    dnf info --installed plplot-devel | grep Source
elif echo "${SYSTEM}" | grep -q FreeBSD ; then
    pkg info gtk2 | grep gtk2-
    pkg info gtk3 | grep gtk3-
    pkg info gtk4 | grep gtk4-
    pkg info glib | grep ^glib
    pkg info plplot | grep plplot-
elif echo "${RELEASE}" | grep -q openSUSE ; then
    zypper info gtk2-devel | grep Source
    zypper info gtk3-devel | grep Source
    zypper info glib2-devel | grep Source
    zypper info plplot-devel | grep Source
else
    dpkg-query --show ${LIB_GTK2}
    dpkg-query --show ${LIB_GTK3}
    dpkg-query --show ${LIB_GTK4}
    dpkg-query --show ${LIB_GLIB}
    dpkg-query --show ${LIB_PLPLOT}
fi

echo "=========="
echo "LANGUAGES:"
echo "=========="

if echo "${SYSTEM}" | grep -q FreeBSD ; then
    gfortran11 --version | head -n 1
else
    gfortran --version | head -n 1
fi

python3 --version
perl --version | head -n 2 | tail -n 1
bash --version | head -n 1

echo "============"
echo "BUILD TOOLS:"
echo "============"

echo "meson $(meson --version)"
cmake --version | head -n 1
echo "pkg-config $(pkg-config --version)"
git --version

echo "============"
echo "OTHER TOOLS:"
echo "============"
