#!/usr/bin/env bash

if [ -z "$INSTALL_ROOT" ]; then INSTALL_ROOT='..'; fi
THIS_DIR='ideavim'

VIMRC=$HOME/.ideavimrc
if [ -f $VIMRC ]; then
    echo "Creating backup $VIMRC.bak"
    cp $VIMRC $VIMRC.bak
fi
cp $INSTALL_ROOT/$THIS_DIR/.ideavimrc $VIMRC
