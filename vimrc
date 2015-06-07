" vim: set foldmethod=marker foldlevel=0:

" Settings {{{
" Vundle {{{
filetype off

if has('vim_starting')
  " Make vim incompatbile to vi.
  if &compatible | set nocompatible | endif
  " set the runtime path to include Vundle and initialize
  set rtp+=~/.dotfiles/vim/bundle/Vundle.vim
endif
call vundle#begin("~/.dotfiles/vim/bundle")

Plugin 'gmarik/Vundle.vim'

" Load packs from bundles
source ~/.dotfiles/vim/bundles

call vundle#end()
filetype plugin indent on
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
filetype indent on " autoindent based on brackets

set laststatus=2
set showcmd
set novb
set backspace=indent,eol,start
set timeoutlen=400 ttimeoutlen=50

syntax on

set whichwrap=b,s
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

set visualbell t_vb=
set splitbelow
set splitright
set hidden
set wildmenu
set tabstop=2
set shiftwidth=2
set expandtab smarttab
set scrolloff=10
set list
set listchars=tab:\ \ ,eol:¬,trail:⬦,nbsp:⬦
set virtualedit=block
set nojoinspaces
set diffopt=filler,vertical
set autoread
set clipboard=unnamed
set foldlevelstart=99
set grepformat=%f:%l:%c:%m,%f:%l:%m
set completeopt=menuone,preview,longest
set nocursorline

set formatoptions+=1
if has('patch-7.3.541')
  set formatoptions+=j
endif
if has('patch-7.4.338')
  let &showbreak = '↳ '
  set breakindent
  set breakindentopt=sbr
endif
if has("statusline")
  set statusline=%f\ %=\ %h%m%r%k\ %y\ %{\"[\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"}\ %-8.(%l,%c%V%)\ %P\ %L

  set statusline+=%#warningmsg#
  set statusline+=%{SyntasticStatuslineFlag()}
  set statusline+=%*
endif
set pastetoggle=<F9>
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
if exists('&colorcolumn')
  set colorcolumn=80
endif

" Keep the cursor on the same column
set nostartofline

if has('gui_running')
    set guifont=Source\ Code\ Pro\ 10
    set guitablabel=%-0.12t%M
    set guioptions-=T
    set guioptions-=r
    set guioptions-=L
    set guioptions+=a
    set guioptions+=i
    set guioptions-=m
  silent! colo seoul256
else
  silent! colo seoul256
endif

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

" completion
set completeopt=longest,menuone

" save tags, cursor pos, etc
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

" disable autocomments
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
"" }}}


" Basic mappings {{{
" Happy abbrevs
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
" ??? "

" Movement hjkl {{{

" Faster moves
nnoremap J 5j
nnoremap K 5k
xmap J 5j
xmap K 5k


" Navigation between buffers
nnoremap <silent> <C-l> :bnext<CR>
nnoremap <silent> <C-h> :bprevious<CR>
nnoremap <silent> <C-q> :Bdelete<CR>

" Navigation between windows
nnoremap <silent> <A-k> :wincmd k<CR>
nnoremap <silent> <A-j> :wincmd j<CR>
nnoremap <silent> <A-h> :wincmd h<CR>
nnoremap <silent> <A-l> :wincmd l<CR>
nnoremap <silent> <A-q> :close<CR>


" Movement in insert mode
inoremap <C-h> <C-o>h
inoremap <C-l> <C-o>a
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

" go to start / end of a fold
nmap z] zo]z

nmap z[ zo[z

nnoremap <silent> <leader>h :noh<cr>
" }}}

" Functions {{{

" Replace
function! s:replace()
  if visualmode() ==# 'V'
    if line("'>") == line('$')
      normal! gv"_dp
    else
      normal! gv"_dP
    endif
  else
    if col("'>") == col('$') - 1
      normal! gv"_dp
    else
      normal! gv"_dP
    endif
  endif
endfunction
" xnoremap R "_dP
xnoremap R :<C-U>call <SID>replace()<cr>



function! s:indent_len(str)
  return type(a:str) == 1 ? len(matchstr(a:str, '^\s*')) : 0
endfunction

function! s:indent_object(op, skip_blank, b, e, bd, ed)
  let i = min([s:indent_len(getline(a:b)), s:indent_len(getline(a:e))])
  let x = line('$')
  let d = [a:b, a:e]

  if i == 0 && empty(getline(a:b)) && empty(getline(a:e))
    let [b, e] = [a:b, a:e]
    while b > 0 && e <= line('$')
      let b -= 1
      let e += 1
      let i = min(filter(map([b, e], 's:indent_len(getline(v:val))'), 'v:val != 0'))
      if i > 0
        break
      endif
    endwhile
  endif

  for triple in [[0, 'd[o] > 1', -1], [1, 'd[o] < x', +1]]
    let [o, ev, df] = triple

    while eval(ev)
      let line = getline(d[o] + df)
      let idt = s:indent_len(line)

      if eval('idt '.a:op.' i') && (a:skip_blank || !empty(line)) || (a:skip_blank && empty(line))
        let d[o] += df
      else | break | end
    endwhile
  endfor
  execute printf('normal! %dGV%dG', max([1, d[0] + a:bd]), min([x, d[1] + a:ed]))
endfunction
xnoremap <silent> ii :<c-u>call <SID>indent_object('>=', 1, line("'<"), line("'>"), 0, 0)<cr>
onoremap <silent> ii :<c-u>call <SID>indent_object('>=', 1, line('.'), line('.'), 0, 0)<cr>
xnoremap <silent> ai :<c-u>call <SID>indent_object('>=', 1, line("'<"), line("'>"), -1, 1)<cr>
onoremap <silent> ai :<c-u>call <SID>indent_object('>=', 1, line('.'), line('.'), -1, 1)<cr>
xnoremap <silent> io :<c-u>call <SID>indent_object('==', 0, line("'<"), line("'>"), 0, 0)<cr>
onoremap <silent> io :<c-u>call <SID>indent_object('==', 0, line('.'), line('.'), 0, 0)<cr>

function! s:replace_emojis() range
  for lnum in range(a:firstline, a:lastline)
    let line = getline(lnum)
    let subs = substitute(line,
          \ ':\([^:]\+\):', '\=emoji#for(submatch(1), submatch(0))', 'g')
    if line != subs
      call setline(lnum, subs)
    endif
  endfor
endfunction
command! -range ReplaceEmojis <line1>,<line2>call s:replace_emojis()

function! s:file_type_handler()
  if &ft =~ 'jinja' && &ft != 'jinja'
    call s:syntax_include('jinja', '{{', '}}', 1)
    call s:syntax_include('jinja', '{%', '%}', 1)
  elseif &ft == 'mkd' || &ft == 'markdown'
    let map = { 'bash': 'sh' }
    for lang in ['ruby', 'yaml', 'vim', 'sh', 'bash', 'python', 'java', 'c', 'sql', 'gnuplot']
      call s:syntax_include(get(map, lang, lang), '```'.lang, '```', 0)
    endfor

    highlight def link Snip Folded

    setlocal textwidth=78
    setlocal completefunc=emoji#complete
  elseif &ft == 'sh'
    call s:syntax_include('ruby', '#!ruby', '/\%$', 1)
  endif
endfunction
" }}}

" Plugins {{{ 

" Disable default GoldenView maps
let g:goldenview__enable_default_mapping = 0
nmap <silent> <Leader>l  <Plug>GoldenViewSplit
nmap <silent> <F8>   <Plug>GoldenViewSwitchMain
nmap <silent> <S-F8> <Plug>GoldenViewSwitchToggle

" <F2> | NERD Tree
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


" Ack
if executable('ag')
  let &grepprg = 'ag --nogroup --nocolor --column'
else
  let &grepprg = 'grep -rn $* *'
endif
command! -nargs=1 -bar Grep execute 'silent! grep! <q-args>' | redraw! | copen

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

" Limelight + Goyo {{{
let g:limelight_paragraph_span = 1
function! s:goyo_enter()
  if has('gui_running')
    set fullscreen
    set background=light
    set linespace=7
  elseif exists('$TMUX')
    silent !tmux set status off
  endif
  " hi NonText ctermfg=101
  Limelight
endfunction

function! s:goyo_leave()
  if has('gui_running')
    set nofullscreen
    set background=dark
    set linespace=0
  elseif exists('$TMUX')
    silent !tmux set status on
  endif
  Limelight!
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

nnoremap <Leader>G :Goyo<CR>
nnoremap <Leader>L :Limelight!!0.6<CR>

" }}}

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

augroup vimrc
  autocmd!

  au BufWritePost vimrc,.vimrc if expand('%') !~ 'fugitive' | source % | endif

  " IndentLines
  au FileType slim execute 'IndentLinesEnable' | doautocmd indentLine Syntax

  " File types
  au BufNewFile,BufRead *.icc               set filetype=cpp
  au BufNewFile,BufRead *.pde               set filetype=java
  au BufNewFile,BufRead *.coffee-processing set filetype=coffee
  au BufNewFile,BufRead Dockerfile*         set filetype=dockerfile

  " Included syntax
  au FileType,ColorScheme * call <SID>file_type_handler()

  " http://vim.wikia.com/wiki/Highlight_unwanted_spaces
  au BufNewFile,BufRead,InsertLeave * silent! match ExtraWhitespace /\s\+$/
  au InsertEnter * silent! match ExtraWhitespace /\s\+\%#\@<!$/

  " Unset paste on InsertLeave
  au InsertLeave * silent! set nopaste

  " Close preview window
  if exists('##CompleteDone')
    au CompleteDone * pclose
  else
    au InsertLeave * if !pumvisible() && (!exists('*getcmdwintype') || empty(getcmdwintype())) | pclose | endif
  endif
augroup END

" Indent guides
let g:indent_guides_enable_on_vim_startup = 1

" Numbers
nnoremap <F4> :NumbersToggle<CR>
nnoremap <F5> :NumbersOnOff<CR>

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
" }}}
