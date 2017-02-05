
" based on nice intro explained .vimrc
" http://dougblack.io/words/a-good-vimrc.html

syntax enable     " enable syntax highlighting
set expandtab      " tabs are spaces
set tabstop=4      " number of visual spaces per tab
set softtabstop=4  " number of spaces in tab when editing
set shiftwidth=4

filetype indent on      " load filetype-specific indent files
set wildmenu         " visual autocomplete for command menu
set showmatch           " highlight matching [{()}]

set incsearch       " search as characters are entered
set hlsearch        " highlight matches
" turn off search highlight
nnoremap <leader><space> :nohlsearch<CR>   # ,<space>
set autoindent
set smartindent

" Custom Keybindings
let mapleader=","       " leader is comma
" jk is escape
inoremap jk <esc>
