" ========= VUNDLE CONFIG ===========
set nocompatible
set nofoldenable
set showcmd
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'

" LIST PLUGINS HERE
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-sensible'
Plugin 'scrooloose/syntastic'
Plugin 'dag/vim2hs'
Plugin 'morhetz/gruvbox'
Plugin 'bling/vim-bufferline'
Plugin 'scrooloose/nerdtree'
let g:syntastic_always_populate_loc_list=1
" A Haskell plugin we'll install later is 'dag/vim2hs',
" but installing it now is fine, too.
" VUNDLE CLEANUP    
call vundle#end()
filetype plugin indent on

colorscheme gruvbox
" ========== GENERAL VIM SETTINGS ==========
map gn :bn<cr>
map gp :bp<cr>
map gd :bd<cr>
cmap w!! w !sudo tee > /dev/null %
" dark mode for gruvbox
set background=dark
" Enable search highlighting
set hlsearch
" Enable line numbers
set number
" Use F11 to toggle between paste and nopaste
"set pastetoggle=

" vim-sensible enables smarttab. Here, we configure the rest:
" Set the display size of t characters
set tabstop=2
" When hitting , insert combination of t and spaces for this width.
" This combination is deleted as if it were 1 t when using backspace.
set softtabstop=2
" Set code-shifting width. Since smarttab is enabled, this is also the tab
" insert size for the beginning of a line.
set shiftwidth=2
" When inserting tab characters, use spaces instead
set expandtab

" Instead of failing command, present dialog if unsaved changes
set confirm

" Enable mouse in all modes
set mouse=a

" Map jk and kj to  to exit insert mode. We need to use F11 to toggle to
" paste mode before pasting any string with jk or kj, then switch back. When
" inserting jk or kj manually, we will need to type the keys slowly so that
" the key mapping times out. Using jk or kj to escape is easier than many
" other alternatives.

" Set a vertical line for long line width. This will give us a visual
" indicator for cases in which line length is approaching 80 chars
set colorcolumn=102840
" Set the command section height to 2 lines.  Useful if notices (like syntastic) are shown on command lines
set cmdheight=1

""no highlight
set nohlsearch
