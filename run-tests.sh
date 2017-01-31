#!/bin/sh
# This runs the test for emacs verifast-mode.

emacs --batch || {
   echo "You must set EMACS to a program that runs emacs."
   exit 1
}

$( emacs -batch > /dev/null 2>&1 ) || {
    echo "Your emacs command (emacs) does not run properly."
    exit 2
};

$( emacs -batch --eval "(require 'ert)" > /dev/null 2>&1 ) || {
    echo 'You must install the `ert` dependency;'
    exit 3
};

$( emacs -batch --eval "(progn (package-initialize)(require 'assess))" > /dev/null 2>&1 ) || {
    echo 'You must install the `assess` dependency;'
    exit 3
};

warnings="$( emacs -Q -batch -f batch-byte-compile verifast-mode.el 2>&1 | grep -v '^Wrote ' )"
if [ -n "$warnings" ]; then
    echo "Byte-compilation failed:"
    echo "$warnings"
    exit 4
else
    echo "Byte-compilation passed."
fi

emacs -batch --eval "(package-initialize)" -l verifast-mode.el -l verifast-mode-tests.el -f ert-run-tests-batch-and-exit
