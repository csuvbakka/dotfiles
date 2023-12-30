#!/usr/bin/env bash

if [ -z "$INSTALL_ROOT" ]; then INSTALL_ROOT='..'; fi
THIS_DIR='emacs'

USER_EMACS_DIR=$HOME/.emacs.d
if [ -d $USER_EMACS_DIR ]; then
    echo "Creating backup $.emacs.d.bak"
    cp -r $USER_EMACS_DIR $USER_EMACS_DIR.bak
fi
cp $INSTALL_ROOT/$THIS_DIR/* $USER_EMACS_DIR/
