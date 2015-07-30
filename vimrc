" vim: set foldmethod=marker foldlevel=0:
set nocompatible
" Settings {{{
" Plugins {{{
if has('vim_starting')
  set rtp+=~/.dotfiles/vim/vim-plug
endif

call plug#begin("~/.dotfiles/vim/plugs")
" Load packs from bundles
source ~/.dotfiles/vim/bundles

call plug#end()
" }}}

let mapleader      = ' '
let maplocalleader = ' '

" Enable mouse because ins mode.
set mouse=a

" Auto chdir to current file.
" set autochdir
autocmd BufEnter * silent! lcd %:p:h

set nu

set autoindent
set smartindent
set lazyredraw

filetype indent on " autoindent based on bracketS

set laststatus=2
set showcmd
" no need for that really
set novisualbell
set noerrorbells

" backspace as in most other apps
set backspace=indent,eol,start
set timeoutlen=400 ttimeoutlen=50

syntax on

set whichwrap=b,s,h,l
set shortmess=aIT
" Search
set hlsearch " CTRL-L / CTRL-R W
set incsearch
set ignorecase
set smartcase

" Encoding
set bomb
set binary
set ttyfast
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8

set splitbelow
set splitright

set hidden
set wildmenu

set tabstop=2
set shiftwidth=2
set expandtab smarttab

set scrolloff=10

set list
set listchars=tab:\ \ ,trail:⬦,nbsp:⬦

set virtualedit=block

set nojoinspaces
set diffopt=filler,vertical

set autoread
set clipboard^=unnamedplus
set foldlevelstart=1
set grepformat=%f:%l:%c:%m,%f:%l:%m
set completeopt=menuone,preview,longest
set nocursorline
set noesckeys

set formatoptions+=1
if has('patch-7.3.541')
  set formatoptions+=j
endif
if has('patch-7.4.338')
  let &showbreak = '↳ '
  set breakindent
  set breakindentopt=sbr
endif
" autoinsert comments
set formatoptions+=ro

if has("statusline")
  set statusline=\ %=\ %h%m%r%k\ %y\ %-8.(%l,%c%V%)\ %P\ %L

  set statusline+=%#warningmsg#
  set statusline+=%{SyntasticStatuslineFlag()}
  set statusline+=%*
endif

" pastetoggle
set pastetoggle=<F9>
" disable pastemode on leave
augroup dispastes
  autocmd!
  autocmd InsertLeave * set nopaste
augroup END


set modelines=0

" Speedup syntax highlighting on long lines
set synmaxcol=1000

" ctags
set tags=./tags;/

" Persistent undo {{{
let vdir = expand("~/.local/share/vim/swap//")
if !isdirectory(vdir)
  call mkdir(vdir)
endif
set directory=~/.local/share/vim/swap//,.
if has('persistent_undo')
  let undodir = expand("~/.local/share/vim/undo//")
  if !isdirectory(undodir)
    call mkdir(undodir)
  endif
  set undodir=~/.local/share/vim/undo//,.
  set undofile
endif
" }}}

set textwidth=0

" Keep the cursor on the same column
set nostartofline

set background=dark
let base16colorspace=256
if has('gui_running')
  set guifont=Source\ Code\ Pro\ 10
  set guitablabel=%-0.12t%M
  set guioptions-=T
  set guioptions-=r
  set guioptions-=L
  set guioptions+=a
  set guioptions+=i
  set guioptions-=m
endif
silent! colo base16-bespin

" Alt workaround
let c='a'
while c <= 'z'
  exec "set <A-".c.">=\e".c
  exec "imap \e".c." <A-".c.">"
  let c = nr2char(1+char2nr(c))
endw

" folding
set foldmethod=marker
set foldcolumn=2
autocmd Syntax c,cpp,xml,html,xhtml,go setlocal foldmethod=syntax
" completion
set completeopt=menu,preview

" save tags, cursor pos, etc {{{
"  '10  :  marks will be remembered for up to 10 previously edited files
"  "100 :  will save up to 100 lines for each register
"  :20  :  up to 20 lines of command-line history will be remembered
"  %    :  saves and restores the buffer list
"  n... :  where to save the viminfo files
set viminfo='10,\"100,:20,%,n~/.viminfo

" save cursor position
function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END
" }}}

"" }}}


" Basic mappings {{{
" Happy abbrevs
nnoremap ; :
cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoreabbrev Qall! qall!
cnoreabbrev Wq wq
cnoreabbrev Wa wa
cnoreabbrev wQ wq
cnoreabbrev WQ wq
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Qall qall

" forward/back one page
noremap <C-F> <C-D>
noremap <C-B> <C-U>

" Save
inoremap <C-s>     <C-O>:update<cr>
nnoremap <C-s>     :update<cr>
nnoremap <leader>s :update<cr>

" Splits like in tmux but without the Shift
nnoremap <leader>\ :split<CR>
nnoremap <leader>5 :vsplit<CR>

" Disable CTRL-A on tmux or on screen
if $TERM =~ 'screen'
  nnoremap <C-a> <nop>
  nnoremap <Leader><C-a> <C-a>
endif

" Disable F1
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" Quit
inoremap <C-Q>     <esc>:q<cr>
nnoremap <C-Q>     :q<cr>
vnoremap <C-Q>     <esc>
nnoremap <Leader>q :q<cr>
nnoremap <Leader>Q :q!<cr>

" Tag stack
nnoremap g[ :pop<cr>

" Jump list (to newer position)
nnoremap <C-p> <C-i>

" Movement hjkl {{{

" Faster moves
nnoremap J 5j
nnoremap K 5k
xmap J 5j
xmap K 5k
nnoremap j gj
nnoremap k gk


" Navigation between buffers
nnoremap <silent> <C-l> :bnext<CR>
nnoremap <silent> <C-h> :bprevious<CR>
nnoremap <silent> <C-q> :Bdelete<CR>
nnoremap <BS> <C-^>


" Navigation between windows
nnoremap <silent> <A-k> :wincmd k<CR>
nnoremap <silent> <A-j> :wincmd j<CR>
nnoremap <silent> <A-h> :wincmd h<CR>
nnoremap <silent> <A-l> :wincmd l<CR>
nnoremap <silent> <A-q> :close<CR>


" Movement in insert mode
inoremap <C-h> <C-o>h
inoremap <C-l> <C-o>l
inoremap <C-j> <C-o>j
inoremap <C-k> <C-o>k
inoremap <C-^> <C-o><C-^>

" Make Y behave like other capitals
nnoremap Y y$

" }}}


" Disable arrows {{{
inoremap <Left> <Nop>
inoremap <Right> <Nop>
inoremap <Up> <Nop>
inoremap <Down> <Nop>

nnoremap <Left> <Nop>
nnoremap <Right> <Nop>
nnoremap <Up> <Nop>
nnoremap <Down> <Nop>

vnoremap <Left> <Nop>
vnoremap <Right> <Nop>
vnoremap <Up> <Nop>
vnoremap <Down> <Nop>
" }}}

" qq to record, Q to replay
nmap Q @q


" lI lA - prepend to same ident
nmap <silent> <leader>I ^vio<C-V>I
nmap <silent> <leader>A ^vio<C-V>$A

" C-e append to word incl. insert mode
nnoremap <C-e> ea
inoremap <C-e> <Esc>ea

" go to start / end of a fold
nmap z] zo]z

nmap z[ zo[z

nnoremap <silent> <leader>n :noh<cr>

" w!! to sudoedit
cmap w!! w !sudo tee % >/dev/null
" }}}

" Plugins {{{ 

" Disable default GoldenView maps
let g:goldenview__enable_default_mapping = 0
"nmap <silent> <Leader>l  <Plug>GoldenViewSplit
nmap <silent> <F8>   <Plug>GoldenViewSwitchMain
nmap <silent> <S-F8> <Plug>GoldenViewSwitchToggle

" <F2> | NERD Tree {{{
inoremap <F2> <esc>:NERDTreeToggle<cr>
nnoremap <F2> :NERDTreeToggle<cr>
let g:NERDTreeChDirMode=2
let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
let g:NERDTreeShowBookmarks=1
let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
let g:NERDTreeWinSize = 20
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite
" }}}

" <F3> | Tagbar
if v:version >= 703
  inoremap <F3> <esc>:TagbarToggle<cr>
  nnoremap <F3> :TagbarToggle<cr>
  let g:tagbar_sort = 0
endif

" Fugitive
nmap     <Leader>gc :Gcommit<CR>i
nmap     <Leader>gs :Gstatus<CR>gg<c-n>
nnoremap <Leader>gd :Gdiff<CR>
nmap     <Leader>gv :Gitv<CR>

" Ack {{{
if executable('ag')
  let &grepprg = 'ag --nogroup --nocolor --column'
else
  let &grepprg = 'grep -rn $* *'
endif
command! -nargs=1 -bar Grep execute 'silent! grep! <q-args>' | redraw! | copen
" }}}

" EasyAlign {{{
let g:easy_align_delimiters = {
      \ '>': { 'pattern': '>>\|=>\|>' },
      \ '\': { 'pattern': '\\' },
      \ '/': { 'pattern': '//\+\|/\*\|\*/', 'delimiter_align': 'l', 'ignore_groups': ['!Comment'] },
      \ ']': {
      \     'pattern':       '[[\]]',
      \     'left_margin':   0,
      \     'right_margin':  0,
      \     'stick_to_left': 0
      \   },
      \ ')': {
      \     'pattern':       '[()]',
      \     'left_margin':   0,
      \     'right_margin':  0,
      \     'stick_to_left': 0
      \   },
      \ 'f': {
      \     'pattern': ' \(\S\+(\)\@=',
      \     'left_margin': 0,
      \     'right_margin': 0
      \   },
      \ 'd': {
      \     'pattern': ' \(\S\+\s*[;=]\)\@=',
      \     'left_margin': 0,
      \     'right_margin': 0
      \   }
      \ }

" Start interactive EasyAlign in visual mode
xmap <Enter> <Plug>(EasyAlign)

" Start interactive EasyAlign with a Vim movement
nmap ga <Plug>(EasyAlign)
nmap gaa ga_
" }}}

" Limelight 
let g:limelight_paragraph_span = 1
nnoremap <Leader>l :Limelight!!0.6<CR>

" Undotree
let g:undotree_WindowLayout = 2
nnoremap <Leader>u :UndotreeToggle<CR>

" fzf
if has('nvim')
  let $FZF_DEFAULT_OPTS .= ' --inline-info'
endif
nnoremap <silent> <Leader><Leader> :FZF -m<CR>

" fzf select buffer {{{
function! s:buflist()
  redir => ls
  silent ls
  redir END
  return split(ls, '\n')
endfunction

function! s:bufopen(e)
  execute 'buffer' matchstr(a:e, '^[ 0-9]*')
endfunction

nnoremap <silent> <Leader><Enter> :call fzf#run({
      \   'source':  reverse(<sid>buflist()),
      \   'sink':    function('<sid>bufopen'),
      \   'options': '+m --prompt="Buf> "',
      \   'down':    len(<sid>buflist()) + 2
      \ })<CR>
" }}}

" Indent guides
let g:indent_guides_enable_on_vim_startup = 1

" Numbers
set number
nnoremap <F4> :NumbersToggle<CR>

" vim-go, filetype go {{{
au FileType go nmap <leader>fr <Plug>(go-run)
au FileType go nmap <leader>fb <Plug>(go-build)
au FileType go nmap <leader>ft <Plug>(go-test)
au FileType go nmap <leader>fc <Plug>(go-coverage)
au FileType go nmap <Leader>fi <Plug>(go-implements)
au FileType go nmap <Leader>i <Plug>(go-info)

au FileType go nmap <Leader>ds <Plug>(go-def-split)
au FileType go nmap <Leader>dv <Plug>(go-def-vertical)
au FileType go nmap <Leader>dt <Plug>(go-def-tab)
au FileType go nmap <Leader>fgd <Plug>(go-doc)
au FileType go nmap <Leader>fgv <Plug>(go-doc-vertical)
au FileType go nmap <Leader>v :GoDef<CR>
let g:go_auto_type_info = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_fmt_command = "goimports"
let g:go_fmt_fail_silently = 1
let g:go_doc_keywordprg_enabled = 0
" }}}

" easyclip
nnoremap <Leader>m m
imap <c-v> <plug>EasyClipInsertModePaste
nmap <leader>pf <plug>EasyClipToggleFormattedPaste
" youcompleteme
let g:ycm_min_num_of_chars_for_completion = 1
let g:UltiSnipsExpandTrigger = '<c-e>'

" syntastic
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_always_populate_loc_list=1
let g:syntastic_error_symbol='✗'
let g:syntastic_warning_symbol='⚠'
let g:syntastic_style_error_symbol = '✗'
let g:syntastic_style_warning_symbol = '⚠'
let g:syntastic_auto_loc_list=2
let g:syntastic_auto_jump = 2
let g:syntastic_aggregate_errors = 1
nnoremap <Leader>es :Errors<CR>
nnoremap <Leader>ec :lclose<CR>

" Buftabs
let g:buftabs_enabled = 1

" smart Home
noremap <expr> <silent> <Home> col('.') == match(getline('.'),'\S')+1 ? '0' : '^'
imap <silent> <Home> <C-O><Home>

" indent movements {{{
" go to start/end of indent
nmap <Leader>[ <Plug>(IndentWiseBlockScopeBoundaryBegin)
nmap <Leader>] <Plug>(IndentWiseBlockScopeBoundaryEnd)
" }}}

" sneak
let g:sneak#streak = 1
"replace 'f' with 1-char Sneak
nmap s <Plug>Sneak_s
nmap S <Plug>Sneak_S
nmap f <Plug>Sneak_f
nmap F <Plug>Sneak_F
xmap f <Plug>Sneak_f
xmap F <Plug>Sneak_F
omap f <Plug>Sneak_f
omap F <Plug>Sneak_F
"replace 't' with 1-char Sneak
nmap t <Plug>Sneak_t
nmap T <Plug>Sneak_T
xmap t <Plug>Sneak_t
xmap T <Plug>Sneak_T
omap t <Plug>Sneak_t
omap T <Plug>Sneak_T
let g:sneak#use_ic_scs = 0

" }}}
