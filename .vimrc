set nocompatible

if has('win32')
    set runtimepath=~/.vim,$VIMRUNTIME
endif


" Easier to type than \
let mapleader = ","

" -- Display
set title " Update the title of your window or your terminal
set number " Display line numbers
set relativenumber " Display relative numbers
set ruler " Display cursor position
set scrolloff=3 " Display at least 3 lines around you cursor

set wrap " Wrap lines when they are too long
set textwidth=79
set formatoptions=qrn1
set colorcolumn=85

" download vim-plug if missing
if empty(glob("~/.vim/autoload/plug.vim"))
  silent! execute '!curl --create-dirs -fsSLo ~/.vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * silent! PlugInstall
endif

" Specify a directory for plugins (for Neovim: ~/.local/share/nvim/plugged)
call plug#begin('~/.vim/plugged')

  Plug 'tomasr/molokai'
  Plug 'xolox/vim-misc'
  Plug 'xolox/vim-easytags'
  Plug 'tpope/vim-fugitive'
  Plug 'junegunn/vim-easy-align'
  Plug 'https://github.com/junegunn/vim-github-dashboard.git'
"  Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
  Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
  Plug 'pearofducks/ansible-vim'
  Plug 'christoomey/vim-tmux-navigator'
call plug#end()

" Write all buffers before navigating from Vim to tmux pane
let g:tmux_navigator_save_on_switch = 2

set mouse=a
if has("mouse_sgr")
    set ttymouse=sgr
else
    set ttymouse=xterm2
end

" (for scrolling)
set guioptions=T " Enable the toolbar

" -- Search
set ignorecase " Ignore case when searching
set smartcase " If there is an uppercase in your search term

" search case sensitive again
set incsearch " Highlight search results when typing
set hlsearch " Highlight search results
set showmatch " highlight matching brackets

" replace multiple occurances on a line by default
set gdefault

" Turn off highlights with ,<space>
nnoremap <leader><space> :noh<cr>
" Jump between brackets with <tab>
nnoremap <tab> %
vnoremap <tab> %


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


" Handle tabs sensibly
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4

set autoindent
set smartindent

" Backup files and Swap files
" trailing double slash means use full path, so same name in different dirs doesn't clobber
set backupdir^=~/.vim/backup//
set directory^=~/.vim/swap//
set undodir^=~/.vim/undo//

" Enable syntax highlighting
syntax enable

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
colorscheme molokai


" Automatically reload VIM config files when they change
if has ('autocmd') " Remain compatible with earlier versions
 augroup vimrc     " Source vim configuration upon save
    autocmd! BufWritePost $MYVIMRC source % | echom "Reloaded " . $MYVIMRC | redraw
    autocmd! BufWritePost $MYGVIMRC if has('gui_running') | so % | echom "Reloaded " . $MYGVIMRC | endif | redraw
  augroup END
endif " has autocmd
