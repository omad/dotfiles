set nocompatible
set encoding=utf-8
scriptencoding utf-8

" Use the same config dirs between Windows and *nix
if has('win32')
    set runtimepath=~/.vim,$VIMRUNTIME
endif

" Use a posix compatible shell if running from within fish
if &shell =~# 'fish$'
    set shell=sh
endif

" Easier to type than \
let mapleader = ","

" -- Display
set title " Update the title of your window or your terminal
set nonumber
set norelativenumber
set ruler " Display cursor position
set scrolloff=3 " Display at least 3 lines around you cursor

syntax enable      " enable syntax highlighting
set expandtab      " tabs are spaces
set tabstop=4      " number of visual spaces per tab
set softtabstop=4  " number of spaces in tab when editing
set shiftwidth=4

filetype indent on " load filetype-specific indent files
set wildmenu       " visual autocomplete for command menu
set showmatch      " highlight matching [{()}]

set incsearch      " search as characters are entered
set hlsearch       " highlight matches
" turn off search highlight
nnoremap <leader><space> :nohlsearch<CR>   # ,<space>
set autoindent
set smartindent

set wrap " Wrap lines when they are too long
set textwidth=79
set formatoptions=qrn1
set colorcolumn=85

" download vim-plug if missing
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Specify a directory for plugins (for Neovim: ~/.local/share/nvim/plugged)
call plug#begin('~/.vim/plugged')

"  Plug 'xolox/vim-misc'
"  Plug 'xolox/vim-easytags'
"  Plug 'ludovicchabant/vim-gutentags'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-surround'
  Plug 'junegunn/vim-easy-align'
  Plug 'https://github.com/junegunn/vim-github-dashboard.git'
  Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
  Plug 'pearofducks/ansible-vim'
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'dag/vim-fish'
  Plug 'LnL7/vim-nix'
  Plug 'ojroques/vim-oscyank', {'branch': 'main'} " System clipboard support

  Plug 'jremmen/vim-ripgrep'


" Colorschemes
  Plug 'mhartington/oceanic-next'
  Plug 'morhetz/gruvbox'
  Plug 'tomasr/molokai'
  Plug 'dracula/vim'
" On-demand loading
  Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
call plug#end()

" Write all buffers before navigating from Vim to tmux pane
let g:tmux_navigator_save_on_switch = 2

set mouse=a
" For using with Neovim
" if has("mouse_sgr")
"     set ttymouse=sgr
" else
"     set ttymouse=xterm2
" end

" (for scrolling)
set guioptions=T " Enable the toolbar

" -- Search
set ignorecase " Ignore case when searching
set smartcase " If there is an uppercase in your search term


" replace multiple occurances on a line by default
set gdefault

" Jump between brackets with <tab>
nnoremap <tab> %
vnoremap <tab> %

" Setup OSCYank for copying to the system clipboard. In Visual mode and Normal
" mode
vnoremap <leader>c :OSCYank<CR>
nmap <leader>o <Plug>OSCYank


" Use standard regex syntax
nnoremap / /\v
vnoremap / /\v

" -- Beep
set visualbell " Prevent Vim from beeping
set noerrorbells " Prevent Vim from beeping

" Backspace behaves as expected
set backspace=indent,eol,start

" Hide buffer (file) instead of abandoning when switching
" to another buffer
set hidden


" Backup files and Swap files
" trailing double slash means use full path, so same name in different dirs doesn't clobber
set backupdir^=~/.vim/backup//
set directory^=~/.vim/swap//
set undodir^=~/.vim/undo//

" GUI Options | Gvim Options
if has('gui_running')
    set guifont=Monospace,Consolas

" Enable pasting with Shift-Insert
    :map <silent> <S-Insert> "+p
    :imap <silent> <S-Insert> <Esc>"+pa
endif

" Syntax highlighting and colours setup
set background=dark
let g:solarized_visibility = "high"
let g:solarized_contrast = "high"
colorscheme gruvbox


" Automatically reload VIM config files when they change
if has ('autocmd') " Remain compatible with earlier versions
 augroup vimrc     " Source vim configuration upon save
    autocmd! BufWritePost $MYVIMRC source % | echom "Reloaded " . $MYVIMRC | redraw
    autocmd! BufWritePost $MYGVIMRC if has('gui_running') | so % | echom "Reloaded " . $MYGVIMRC | endif | redraw
  augroup END
endif " has autocmd

" Splits
" ===================================
set splitright            " vertical splits use right half of screen
set splitbelow            " horizontal splits use bottom half of screen

" Auto completion
" ===================================
" Show potential matches above completion, complete first immediately
set browsedir=buffer                       " browse files in same dir as open file, make tab completion for files/buffers act like bash
set wildmenu                               " Enhanced command line completion.
set wildmode=list:longest,list,full        " Complete files using a menu AND list
set wildignorecase

" Ignore
" ===================================
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz
set wildignore+=*.swp,.lock,.DS_Store,._*
set wildignore+=.sass-cache
set wildignore+=*.eot,*.otf,*.ttf,*.woff
set wildignore+=*.doc,*.pdf,*.retry


" Other
" ===================================
set isk+=_,$,@,%,#,-      " none word dividers
set list
set listchars=tab:\ \ ,trail:· " Display tabs and trailing spaces visually


" NetRW Setup
" ===================================
let g:netrw_banner = 0       " No Banner

let g:netrw_liststyle = 3    " Tree list style

let g:netrw_browse_split = 4 " Open file in previous window to right of project drawer
let g:netrw_altv = 1
let g:netrw_winsize = 25

" Open NetRW draw on vim startup
"augroup ProjectDrawer
"    autocmd!
"    autocmd VimEnter * :Vexplore
"augroup END
let g:netrw_list_hide= '.*\.swp$'


" Fix vim-tmux-navigator keys in netrw
" Thanks https://github.com/christoomey/vim-tmux-navigator/issues/53
augroup navigator
  autocmd!
  autocmd FileType netrw call s:reset_netrw_keys()
augroup END

function! s:reset_netrw_keys() abort
"  nmap <buffer> <silent> <c-h> <Plug>NetrwHideEdit
"  nmap <buffer> <silent> <c-l> <Plug>NetrwRefresh
  noremap <buffer> <c-h> <c-w><c-h>
  noremap <buffer> <c-l> <c-w><c-l>
endfunction

" Switch cursor charactors if using mintty
if !empty($MSYSCON) && $MSYSCON == "mintty.exe"
    let &t_ti.="\e[1 q"
    let &t_SI.="\e[5 q"
    let &t_EI.="\e[1 q"
    let &t_te.="\e[0 q"
endif

" Toggle Line numbers
nnoremap <leader>nn :set number! relativenumber!<cr>

" Toggle paste mode
nnoremap <leader>p :set paste!<cr>


