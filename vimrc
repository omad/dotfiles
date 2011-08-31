
set nocompatible
set background=dark

"allow backspacing over everything in insert mode
set bs=2
"Always show cursor position
set ruler

"Show menu with possible tab completions
set wildmenu
"Ignore these files when completing names and in Explorer
set wildignore=.svn,CVS,.git,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.png,*.xpm,*.gif,.hg

set ignorecase
set smartcase "Only case sensitive if we type a captial

"""""
" Indenting
"""""
"Default to autoindenting of C like languages
"This is overridden per filetype below
set noautoindent smartindent

"The rest deal with whitespace handling and
"mainly make sure hardtabs are never entered
"as their interpretation is too non standard in my experience
set softtabstop=4
" Note if you don't set expandtab, vi will automatically merge
" runs of more than tabstop spaces into hardtabs. Clever but
" not what I usually want.
set expandtab
set shiftwidth=4
set shiftround
set nojoinspaces

""""""""
" Backups (.swp) in central location
""""""""
set backup
set backupdir=$USERPROFILE/temp/vim_backups/    "where to put those backups
set directory=$USERPROFILE/temp/vim_swp/        "this is for swp files

if has('unix')
    set backupdir=~/.vim/backups/
    set directory=~/.vim/swp/
endif


""""""""
" Syntax highlighting
""""""""

"Syntax highlighting if appropriate
if &t_Co > 2 || has("gui_running")
    syntax on
    set hlsearch
    set incsearch "For fast terminals can highlight search string as you type
endif

if &diff
    "I'm only interested in diff colours
    syntax off
endif

"flag problematic whitespace (trailing and spaces before tabs)
"Note you get the same by doing let c_space_errors=1 but
"this rule really applys to everything.
"highlight RedundantSpaces term=standout ctermbg=red guibg=red
"match RedundantSpaces /\s\+$\| \+\ze\t/ "\ze sets end of match so only spaces highlighted

"use :set list! to toggle visible whitespace on/off
set listchars=tab:>-,trail:.,extends:>


set showmatch "matching parenthesis, etc


""""""
" file type handling
""""""
filetype on
filetype plugin on
filetype indent on

augroup sh
    au!
    "smart indent really only for C like languages
    au FileType sh set nosmartindent autoindent
augroup END


" Check out http://www.pixelbeat.org/settings/.vimrc

