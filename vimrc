" ========== Load Vundle ================="
"
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
" ========== END Load Vundle =============="

" ========== Vim Basic Settings ============="
set modelines=0

" Enable mouse because ins mode.
set mouse=a

set timeoutlen=1000 ttimeoutlen=0

" Auto chdir to current file.
" set autochdir
autocmd BufEnter * silent! lcd %:p:h

" Global TAB settings.
set smartindent
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab

" Autoindent based on brackets
filetype indent on

" Show whitespaces
set list
set listchars=tab:▸\ ,eol:¬,trail:⬦,nbsp:⬦

" Highlight searches
set hlsearch

" Use smartcase
set ignorecase
set smartcase

" Search while typing
set incsearch

" Show matching brackets
set showmatch

" Persistent history
if has('persistent_undo')
  set undodir=$HOME/.dotfiles/vim/undo
  set undofile
endif

" Set backup dir for swp files
set backupdir=$HOME/.dotfiles/vim/backup
set directory=$HOME/.dotfiles/vim/backup

" More Common Settings.
set encoding=utf-8
set scrolloff=10
set autoindent
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
set novisualbell
set timeout ttimeoutlen=50

set nospell
set virtualedit=onemore

set foldmethod=syntax
set foldnestmax=10
set nofoldenable
set foldlevel=0
syntax on

set history=1000
set cursorline
set ttyfast
set ruler
set backspace=indent,eol,start

" Set number
set number
set norelativenumber

set splitbelow
set splitright

set shell=/bin/zsh
set lazyredraw
set matchtime=3

" Set tag file
set tags=./tags,tags
let g:easytags_dynamic_files=1
let g:easytags_async=1
let g:easytags_auto_highlight=0
" Set title to window
set notitle

" Let lines be around 80 chars long
"set textwidth=80

"set statusline=%<%f\ %y\ %h%m%r%=%-14.(%l,%c%V%)\ %P\ %L
"set statusline=%-50{buftabs#statusline()}%=\ %h%m%r%k\ %y\ %{\"[\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"}\ %-8.(%l,%c%V%)\ %P\ %L
if has("statusline")
  set statusline=%f\ %=\ %h%m%r%k\ %y\ %{\"[\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"}\ %-8.(%l,%c%V%)\ %P\ %L
endif



" Change status color on different modes
" First, enable status line always
set laststatus=2

" Now set it up to change the status line based on mode
if version >= 700
  au InsertEnter * hi StatusLine term=reverse ctermfg=7 ctermbg=65 guibg=#005f5f guifg=White gui=underline
  au InsertLeave * hi StatusLine term=reverse ctermfg=7 ctermbg=24 gui=bold,reverse
endif

" Make Sure that Vim returns to the same line when we reopen a file"
augroup line_return
  au!
  au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \ execute 'normal! g`"zvzz' |
        \ endif
augroup END

" Working with split screen nicely
" Resize Split When the window is resized"
au VimResized * :wincmd =

" Disable autocomplete scratchpad preview thingy
set completeopt=longest,menuone
"

" =========== END Basic Vim Settings ===========

" =========== Vim Keybindings ==================

" Changing Leader Key
let mapleader = ","

" C-q doesn't reach vim, so lets try this:
silent !stty -ixon > /dev/null 2>/dev/null

" Let Alt key combos to work :
let c='a'
while c <= 'z'
  exec "set <A-".c.">=\e".c
  exec "imap \e".c." <A-".c.">"
  let c = nr2char(1+char2nr(c))
endw

" Make tab in v mode ident code
vnoremap <tab> >gv
vnoremap <s-tab> <gv

" Ctrl+Space
" to accept the autocompletion and exit
" insert mode.
"inoremap <C-Space> <C-y><ESC>
"inoremap <C-@> <C-y><ESC>
"inoremap <Nul> <C-y><ESC>

" Splits like in tmux but without the Shift
nnoremap <leader>\ :split<CR>
nnoremap <leader>5 :vsplit<CR>

" Ctrl+S to save
nnoremap <C-s> :w<CR>
inoremap <C-s> <ESC>:w<CR>
vnoremap <C-s> <ESC>:w<CR>

" <leader><leader> to toggle between buffers
nmap <leader><leader> <C-^>
vmap <leader><leader> <C-^>

" Navigation between windows
nnoremap <silent> <A-k> :wincmd k<CR>
nnoremap <silent> <A-j> :wincmd j<CR>
nnoremap <silent> <A-h> :wincmd h<CR>
nnoremap <silent> <A-l> :wincmd l<CR>
nnoremap <silent> <A-q> :close<CR>

" j and k to move between line wrapped lines
nnoremap j gj
nnoremap k gk
xnoremap j gj
xnoremap k gk

" Faster moves
nmap J 5j
nmap K 5k
xmap J 5j
xmap K 5k

" Navigation between buffers
nnoremap <silent> <C-l> :bnext<CR>
nnoremap <silent> <C-h> :bprevious<CR>
nnoremap <silent> <C-q> :Bdelete<CR>

nnoremap <silent> gt1 :buffer 1<CR>
nnoremap <silent> gt2 :buffer 2<CR>
nnoremap <silent> gt3 :buffer 3<CR>
nnoremap <silent> gt4 :buffer 4<CR>
nnoremap <silent> gt5 :buffer 5<CR>
nnoremap <silent> gt6 :buffer 6<CR>
nnoremap <silent> gt7 :buffer 7<CR>
nnoremap <silent> gt8 :buffer 8<CR>
nnoremap <silent> gt9 :buffer 9<CR>

" Get Rid of stupid Goddamned help keys
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" Make arrows useless
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
"open new tab
nnoremap gtt :tabnew %<CR>
vnoremap gtt :tabnew %<CR>
" Toggle highlight search
let hlstate=0
nnoremap <Leader>n :if (hlstate%2 == 0) \| nohlsearch \| else \| set hlsearch \| endif \| let hlstate=hlstate+1<CR>:echo "toggled visibility for hlsearch"<CR>
  inoremap <Leader>n <ESC>:if (hlstate%2 == 0) \| nohlsearch \| else \| set hlsearch \| endif \| let hlstate=hlstate+1<CR>:echo "toggled visibility for hlsearch"<CR>a

    " Better navigation between results with vimgrep and similar
    " plugins
    nnoremap [q :cprev<CR>
    nnoremap ]q :cnext<CR>

    " Better navigation between results with f<char>
    nnoremap [a ,
    nnoremap ]a ;

    vnoremap [a ,
    vnoremap ]a ;

    " Better navigation in jumplist
    nnoremap [w <C-o>
    nnoremap ]w <C-i>

    vnoremap [w <C-o>
    vnoremap ]w <C-i>

    " Navigation in between tabs
    nnoremap [t :tabprevious<cr>
    nnoremap ]t :tabnext<cr>

    vnoremap [t :tabprevious<cr>
    vnoremap ]t :tabnext<cr>

    nnoremap QQ :QuitTab<cr>
    command! QuitTab call s:QuitTab()
    function! s:QuitTab()
      try
        tabclose
      catch /E784/ " Can't close last tab
        qall
      endtry
    endfunction

    " Zoom in and out (change font size)
    " This one uses the script t located in ~/bin
    nnoremap <silent> [f :r !t zo<CR>
    nnoremap <silent> ]f :r !t zi<CR>

    vnoremap <silent> [f :r !t zo<CR>
    vnoremap <silent> ]f :r !t zi<CR>

    " Map ; to : in normal and visual mode.
    nnoremap ; :
    vnoremap ; :

    " Set vim to save the file on focus out.
    "au FocusLost * silent! :w

    " Adding More Shorcuts keys using leader key.
    " Leader Key provide separate namespace for specific commands.
    ",W Command to remove white space from a file.
    nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

    " ,ft Fold tag, helpful for HTML editing.
    nnoremap <leader>ft vatzf

    " let left and right keys go to the next line
    set whichwrap+=<,>,h,l

    " ,q Re-hardwrap Paragraph
    nnoremap <leader>q gqip

    " ,v Select just pasted text.
    nnoremap <leader>v V`]

    " ,ev Shortcut to edit .vimrc file on the fly on a vertical window.
    nnoremap <leader>ev <C-w><C-v><C-l>:e $MYVIMRC<cr>

    " ,;  For Qicker Escaping between normal and visual mode.
    vnoremap <leader>; <ESC>

    nnoremap g; g;zz

    " Buftabs
    nnoremap gb :BuftabsToggle<CR>

    " Because we're cool right
    nmap <C-\> :vsplit<CR>
    nmap <C-_> :split<CR>

    " =========== END Vim Keybindings ==============

    " =========== Color theme settings =============

    colorscheme lucius
    LuciusDark

    " rm1234
    match TabLineFill /rm\d\+/

    " Highlight trailing whitespaces
    match Error /\s\+$/
    " =========== END Color theme settings =========

    " =========== Gvim Settings =============

    " Removing scrollbars
    if has("gui_running")
      set guifont=Source\ Code\ Pro\ 10
      set guitablabel=%-0.12t%M
      set guioptions-=T
      set guioptions-=r
      set guioptions-=L
      set guioptions+=a
      set guioptions+=i
      set guioptions-=m
    else
      set t_Co=256
    endif

    " Source the vimrc file after saving it
    autocmd bufwritepost vimrc source ~/.dotfiles/vimrc

    " ========== END Gvim Settings ==========

    " ========== Functions ==================

    func! ClearTrailingWS()
      exe "normal mz"
      %s/\s\+$//ge
      exe "normal `z"
    endfunc

    func! ShowTrailingWS()
      match Error /\s\+$/
    endfunc

    " ========== END Functions ==============

    " ========== Plugins Settings =========="
    " vim mode-switch lag fix (related to autoclose)
    if ! has("gui_running")
      set ttimeoutlen=10

      augroup FastEscape
        autocmd!
        au InsertEnter * set timeoutlen=0
        au InsertLeave * set timeoutlen=1000
      augroup END
    endif

    " Mapping for ag
    " Search the word under the cursor
    nnoremap <leader>f mM:Ag! "<C-R><C-W>"<CR>:cw<CR>
    vnoremap <leader>f mM"hy:Ag! <C-R>h<CR>:cw<CR>
    " Mapping to Undotree
    nmap <leader>u <ESC>:UndotreeToggle<CR>
    imap <leader>u <ESC>:UndotreeToggle<CR>

    " Mapping to NERDTree
    nmap <F2> :NERDTreeToggle<CR>
    let NERDTreeIgnore=['.sw?','\~$', '\.pyc$']

    " Tagbar key bindings."
    nmap <F3> :TagbarToggle<CR>
    nmap <leader>l <ESC>:TagbarToggle<CR>
    imap <leader>l <ESC>:TagbarToggle<CR>i

    " Tagbar support for go."
    let g:tagbar_type_go = {
          \ 'ctagstype' : 'go',
          \ 'kinds'     : [
          \ 'p:package',
          \ 'i:imports:1',
          \ 'c:constants',
          \ 'v:variables',
          \ 't:types',
          \ 'n:interfaces',
          \ 'w:fields',
          \ 'e:embedded',
          \ 'm:methods',
          \ 'r:constructor',
          \ 'f:functions'
          \ ],
          \ 'sro' : '.',
          \ 'kind2scope' : {
          \ 't' : 'ctype',
          \ 'n' : 'ntype'
          \ },
          \ 'scope2kind' : {
          \ 'ctype' : 't',
          \ 'ntype' : 'n'
          \ },
          \ 'ctagsbin'  : 'gotags',
          \ 'ctagsargs' : '-sort -silent'
          \ }

    nmap <Leader>tt :TagbarToggle<CR>

    " CtrlP key binding.
    nnoremap <C-p> :CtrlPBufTagAll<CR>
    inoremap <C-p> <ESC>:CtrlPBufTagAll<CR>
    vnoremap <C-p> <ESC>:CtrlPBufTagAll<CR>

    nnoremap <C-b> :CtrlPBuffer<CR>
    inoremap <C-b> <ESC>:CtrlPBuffer<CR>
    vnoremap <C-b> <ESC>:CtrlPBuffer<CR>

    nnoremap <C-o> :CtrlP<CR>
    inoremap <C-o> <ESC>:CtrlP<CR>
    vnoremap <C-o> <ESC>:CtrlP<CR>

    nnoremap <C-t> :CtrlPTag<CR>
    inoremap <C-t> <ESC>:CtrlPTag<CR>
    vnoremap <C-t> <ESC>:CtrlPTag<CR>


    " Symbol for lines which have been added, default: +
    let g:git_diff_added_symbol='⇒'

    " Symbol for lines which have been removed, default: -
    let g:git_diff_removed_symbol='⇐'

    " Symbol for lines which have been changed, default: <>
    let g:git_diff_changed_symbol='⇔'

    " IndentGuides setup
    let g:indent_guides_enable_on_vim_startup=1
    let g:indent_guides_start_level=2
    let g:indent_guides_guide_size=1
    let g:indent_guides_indent_levels=6

    " GoldenrRatio settings
    let g:goldenview__enable_default_mapping = 0
    nmap <silent> <leader>gr  <Plug>GoldenViewSplit

    " SuperTab setting
    " uses omni if enabled
    let g:SuperTabDefaultCompletionType = "context"
    " Omnicompletion close scratch window
    " If you prefer the Omni-Completion tip window to close when a selection is
    " made, these lines close it on movement in insert mode or when leaving
    " insert mode
    autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
    autocmd InsertLeave * if pumvisible() == 0|pclose|endif

    "vim-go settings
    " format with goimports instead of gofmt
    let g:go_fmt_command = "goimports"
    nnoremap <leader>gl :GoLint<CR>
    inoremap <leader>gl <ESC>:GoLint<CR>
    vnoremap <leader>gl <ESC>:GoLint<CR>


    " Numbers
    let g:numbers_exclude = ['tagbar', 'gundo', 'minibufexpl', 'nerdtree']
    nnoremap <F1> :NumbersToggle<CR>
    "nnoremap <F4> :NumbersOnOff<CR>

    " YCM/YouCompleteMe
    let g:ycm_min_num_of_chars_for_completion = 1

    let g:ycm_key_list_select_completion = ['<C-n>', '<Tab>']
    let g:ycm_key_list_previous_completion = ['<C-p>', '<s-Tab>']
    let g:SuperTabDefaultCompletionType = '<C-n>'

    " better key bindings for UltiSnipsExpandTrigger
    let g:UltiSnipsExpandTrigger = "<c-e>"
    let g:UltiSnipsJumpForwardTrigger = "<tab>"
    let g:UltiSnipsJumpBackwardTrigger = "<s-tab>""

    " EasyClip
    " Use gm as add mark
    nnoremap gm m

    cmap <c-v> <plug>EasyClipCommandModePaste
    imap <c-v> <plug>EasyClipInsertModePaste
    set clipboard=unnamed

    let g:EasyClipAutoFormat = 1
    let g:EasyClipYankHistorySize = 150

    " Undotree
    nnoremap <F4> :UndotreeToggle<CR>
    let g:undotree_SetFocusWhenToggle = 1
    " vim-go
    au FileType go nmap <leader>r <Plug>(go-run)
    au FileType go nmap <leader>b <Plug>(go-build)
    au FileType go nmap <leader>t <Plug>(go-test)
    au FileType go nmap <leader>c <Plug>(go-coverage)
    au FileType go nmap <Leader>dd <Plug>(go-doc)
    au FileType go nmap <Leader>gd <Plug>(go-def-vertical)
    au FileType go nmap <Leader>s <Plug>(go-implements)
    au Filetype go nnoremap <leader>v :vsp <CR>:exe "GoDef" <CR>

    " vim-go
    let g:go_highlight_functions = 1
    let g:go_highlight_methods = 1
    let g:go_highlight_structs = 1
    let g:go_highlight_operators = 1
    let g:go_highlight_build_constraints = 1

    " Syntastic
    set statusline+=%#warningmsg#
    set statusline+=%{SyntasticStatuslineFlag()}
    set statusline+=%*

    let g:syntastic_always_populate_loc_list = 1
    let g:syntastic_auto_loc_list = 1
    let g:syntastic_check_on_open = 1
    let g:syntastic_check_on_wq = 0

    " Molasses
    let g:molasses_keys='hjkl'

    " Markdown
    au BufRead,BufNewFile *.md set filetype=markdown
    " Go tags
    au BufWritePost *.go silent! !ctags -R &
    "========== END Plugin Settings =========="

    hi Normal ctermbg=NONE
