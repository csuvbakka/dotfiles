set ignorecase
set smartcase
set number
set relativenumber
set scrolloff=3

set tabstop=4    "number of spaces that a <Tab> counts for
set shiftwidth=4 "number of spaces to use for each step of (auto)indent
set expandtab    "use the appropriate number of spaces to insert a <Tab>
set autoindent
set hlsearch
set incsearch

let mapleader = ","

nnoremap n nzz
nnoremap N Nzz

"split vertically and move to new window
nnoremap <leader>w <C-w>v<C-w>w

"use ,s for saving
nnoremap <leader>s :w<CR>

"use jk to get back to normal mode
inoremap jk <Esc>

" create newlines like o and O but stay in normal mode
nnoremap <leader>j o<Esc>k
nnoremap <leader>k O<Esc>j
nnoremap <leader>o i<CR><Esc>

" Ctrl + l/h to move between buffers
nnoremap <C-l> :bn<CR>
nnoremap <C-h> :bp<CR>

nnoremap <leader>v :edit ~/.vimrc<CR>
