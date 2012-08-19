#! /bin/sh
# Automatic screenshots of the gtk-fortran examples
# Parameters: $1 suffix (e.g. -ubuntu11_04), $2 sleep time
# You need to install scrot
# GNU GPL 3
# Contributed by Vincent MAGNIN
# April 27th 2011, updated April 28th 2011.

if [ $# -eq 0 ]; then
  seconds=3
  suffix="-ubuntu_11_04_32bits"
else
  seconds=$2
  suffix=$1
fi

echo "Suffix: "$suffix
echo "Sleep time: "$seconds
echo "Taking screenshots..."
cd ../examples/

for file in *.f90.out; do 
  echo $file
  #Launch the program:
  ./$file &
  sleep $seconds
  #remove the double extension (seven characters after a point):
  picfile=`echo $file|sed 's/\..\{7\}$//'`
  #Take and save a screenshot of the active window with border:
  scrot -ub ../screenshots/$picfile$suffix.png
  #Kill the program before launching next one:
  kill $!
done
