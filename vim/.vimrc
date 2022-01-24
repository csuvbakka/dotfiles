set nocompatible
filetype plugin indent on

source $HOME/.vim/common.vim

set completeopt=menu           "do not show preview window
set backspace=indent,eol,start "do not use vi compatible backspace

set laststatus=2     "always display status line

syntax on
set background=dark
set t_Co=256
colorscheme dogrun

"remember last position when reopening a file
if has("autocmd")
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif
