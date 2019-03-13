#! /bin/bash
# Show libraries and tools versions used in gtk-fortran
# Parameters: none
# Contributed by Vincent MAGNIN, 2019-03-13
# Updated 2019-03-13

# Distribution (Linux only):
lsb_release -sd

# Kernel, release, processor, OS:
uname -srpo

# Libraries:
dpkg-query --show libgtk2.0-dev
dpkg-query --show libgtk-3-dev
dpkg-query --show libplplot-dev

# Languages:
gfortran --version | head -n 1
python3 --version
perl --version | head -n 2 | tail -n 1

# Build tools:
cmake --version | head -n 1
echo "pkg-config $(pkg-config --version)"

# Other tools:
git --version
bash --version | head -n 1
