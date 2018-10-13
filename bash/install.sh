#!/usr/bin/env bash

if [ -z "$INSTALL_ROOT" ]; then INSTALL_ROOT='..'; fi
THIS_DIR='bash'

BASHRC=$HOME/.bashrc
if [ -f $BASHRC ]; then
    echo "Creating backup $BASHRC.bak"
    cp $BASHRC $BASHRC.bak
fi
cp $INSTALL_ROOT/$THIS_DIR/.bashrc $BASHRC
