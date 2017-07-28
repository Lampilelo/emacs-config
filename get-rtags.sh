#!/bin/sh

EMACS_DIR="$HOME/.emacs.d"

cd $EMACS_DIR && git clone --recursive https://github.com/Andersbakken/rtags.git

if [ -x $EMACS_DIR/rtags/build ]; then rm -r $EMACS_DIR/rtags/build; fi
mkdir $EMACS_DIR/rtags/build
cd $EMACS_DIR/rtags/build && cmake .. && make
