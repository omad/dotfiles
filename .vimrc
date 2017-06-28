
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

set bg=dark

" Custom Keybindings
let mapleader=","       " leader is comma
" jk is escape
inoremap jk <esc>

" download vim-plug if missing
if empty(glob("~/.vim/autoload/plug.vim"))
  silent! execute '!curl --create-dirs -fsSLo ~/.vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * silent! PlugInstall
endif

" Specify a directory for plugins (for Neovim: ~/.local/share/nvim/plugged)
call plug#begin('~/.vim/plugged')

  Plug 'junegunn/vim-easy-align'
  Plug 'https://github.com/junegunn/vim-github-dashboard.git'
  Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
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
