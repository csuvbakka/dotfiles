#!/usr/bin/env bash

if [ -z "$INSTALL_ROOT" ]; INSTALL_ROOT='..'; fi
THIS_DIR='vim'

VIMDIR=$HOME/.vim
mkdir -p $VIMDIR
cp -R colors $VIMDIR/colors

VIMRC=$HOME/.vimrc
if [ -f $VIMRC ]; then
    echo "Creating backup $VIMRC.bak"
    cp $VIMRC $VIMRC.bak
fi
cp $INSTALL_ROOT/$THIS_DIR/.vimrc $VIMRC
cp $INSTALL_ROOT/$THIS_DIR/common.vim $VIMDIR/
