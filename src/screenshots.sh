#! /bin/sh
# Automatic screenshots of the gtk-fortran built examples
# Parameters: $1 suffix (e.g. -ubuntu11_04), $2 sleep time
# You need to install scrot
# GNU GPL 3
# Contributed by Vincent MAGNIN 2011-04-27
# Updated 2020-02-12

# For a safer script:
set -eu

if [ $# -eq 0 ]; then
  readonly seconds=3
  readonly suffix="-kubuntu_$(lsb_release -rs)"
else
  readonly seconds=${2}
  readonly suffix=${1}
fi

echo "Suffix: ${suffix}"
echo "Sleep time: ${seconds}"
echo "Taking screenshots..."

# Scanning all built examples:
for directory in ../build/examples/ ../build/plplot ../build/sketcher ; do
    cd ${directory}
    pwd
    for file in * ; do
        # Is it an executable file ?
        if [ -x "${file}" ] && [ ! -d "${file}" ] && [ ! "${file}" = "gio_demo" ] && [ ! "${file}" = "tests" ]; then
            echo "${file}"
            #Launch the program:
            ./"${file}" &
            sleep "${seconds}"
            #remove the double extension (seven characters after a point):
            picfile=$(echo "${file}"|sed 's/\..\{7\}$//')
            #Take and save a screenshot of the active window with border:
            scrot -ub ../../screenshots/"${picfile}${suffix}".png
            #Kill the program before launching next one:
            kill $!
        fi
    done
    # Go back to src before next iteration:
    cd ../../src
done
