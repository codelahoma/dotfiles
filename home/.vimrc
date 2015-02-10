" Rod Knowlton's vimrc
"
" Copy at your own risk.
"
" Use Vim settings. Must run before other settings
set nocompatible

let s:darwin = has('mac')

let g:ft_ignore_pat = '\.org'

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.vim/plugged')
Plug 'mattn/emmet-vim'
Plug 'tacahiroy/ctrlp-funky'
Plug 'bling/vim-airline'
Plug 'vim-misc'
Plug 'nblock/vim-dokuwiki'
Plug 'dogrover/vim-pentadactyl'
Plug 'khorser/vim-qfnotes'
Plug 'airblade/vim-gitgutter'
Plug 'Lokaltog/vim-easymotion'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'majutsushi/tagbar'
Plug 'mattn/webapi-vim'
Plug 'mattn/gist-vim'
Plug 'scrooloose/nerdtree'
" Plug 'sjl/gundo.vim'

Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-scriptease'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-dispatch'

Plug 'vim-scripts/Align'
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/vim-peekaboo'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/vim-github-dashboard'
Plug 'junegunn/vim-xmark', {'do': 'make'}
Plug 'junegunn/seoul256.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/vader.vim'
Plug 'kakkyz81/evervim'
" Language Additions
"  Ruby

" Plug 'tpope/vim-haml'
" Plug 'tpope/vim-rails'
" Plug 'tpope/vim-rake'
" Plug 'tpope/vim-rvm'
" Plug 'vim-ruby/vim-ruby'

" Jade templating
Plug 'jade.vim'

" Javascript
Plug 'pangloss/vim-javascript'
" Plug 'jelera/vim-javascript-syntax'
Plug 'mxw/vim-jsx'

" Coffeescript
Plug 'kchmck/vim-coffee-script'
Plug 'mintplant/vim-literate-coffeescript'

" Clojure
Plug 'https://github.com/tpope/vim-classpath.git'
Plug 'tpope/vim-fireplace', { 'for': 'clojure'}
Plug 'kovisoft/paredit', {'for': ['clojure', 'scheme']}

Plug 'https://github.com/kien/rainbow_parentheses.vim.git'
Plug 'vim-slamhound'

Plug 'davidoc/taskpaper.vim'
" Plug 'zenorocha/dracula-theme'

function! BuildYCM(info)
  " info is a dictionary with 3 fields, passed by Plug
  " - name: name of the plugin
  " - status: 'installed', 'updated', or 'unchanged'
  " - force: set on PlugInstall! or PlugUpdate!
  if a:info.status == 'installed' || a:info.force
    !./install.sh
  endif
endfunction

" Load on nothing
Plug 'SirVer/ultisnips', { 'on': [] }
Plug 'Valloric/YouCompleteMe', {'on': [], 'do': function('BuildYCM')}



Plug 'Syntastic'
Plug 'jiangmiao/auto-pairs'

" The look
Plug 'daylerees/colour-schemes', { 'rtp': 'vim' }
Plug 'nanotech/jellybeans.vim'
Plug 'altercation/vim-colors-solarized'
Plug 'chriskempson/vim-tomorrow-theme'
Plug 'chriskempson/base16-vim'


if s:darwin
  Plug 'rizzatti/dash.vim'
endif

Plug 'https://github.com/guns/vim-clojure-static.git'
" vimscripts.org

Plug 'Zenburn'
Plug 'ZoomWin'
Plug 'ctrlp.vim'
Plug 'dbext.vim'
Plug 'ack.vim'
Plug 'localvimrc'
Plug 'errormarker.vim'
Plug 'AsyncCommand'
Plug 'TVO--The-Vim-Outliner'
" github repos
 " gallery: http://daylerees.github.io/
Plug 'christoomey/vim-tmux-navigator'
Plug 'Yggdroot/indentLine', {'on': 'IndentLinesToggle'}
Plug 'mbbill/undotree', {'on': 'UndotreeToggle'}
" Plug 'roman/golden-ratio'
" Plug 'nathanaelkane/vim-indent-guides'
" Plug 'vimoutliner/vimoutliner'
" Plug 'Puppet-Syntax-Highlighting'
call plug#end()


"  Parentheses colours using Solarized
let g:rbpt_colorpairs = [
      \ [ '13', '#6c71c4'],
      \ [ '5',  '#d33682'],
      \ [ '1',  '#dc322f'],
      \ [ '9',  '#cb4b16'],
      \ [ '3',  '#b58900'],
      \ [ '2',  '#859900'],
      \ [ '6',  '#2aa198'],
      \ [ '4',  '#268bd2'],
      \ ]

" Enable rainbow parentheses for all buffers
augroup rainbow_parentheses
  au!
  au VimEnter * RainbowParenthesesActivate
  au BufEnter * RainbowParenthesesLoadRound
  au BufEnter * RainbowParenthesesLoadSquare
  au BufEnter * RainbowParenthesesLoadBraces
augroup END





" Org Mode  ----------------------------------------"{{{
au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
au BufEnter *.org            call org#SetOrgFileType()
" let g:org_capture_file = '~/org_files/mycaptures.org'
command! OrgCapture :call org#CaptureBuffer()
command! OrgCaptureFile :call org#OpenCaptureFile()"}}}

" Settings ---------------------------------------- {{{
set encoding=utf-8
set autoread


" fix brain-dead change to html indenting
let g:html_indent_inctags = "html,body,head,tbody"


" Persist undo history across sessions
set undodir=~/.vim/undo
set undofile
set undolevels=1000
set undoreload=10000

" Allow backgrounding buffers without writing them, and remember marks/undo
" for backgrounded buffers
set hidden

" Automatically leave insert mode after 'updatetime' (4s by default).
" au CursorHoldI * stopinsert

" Remember more commands and search history
set history=1000

let EasyMotion_leader_key = ','
let mapleader = ' '
let maplocalleader = ','
let $JS_CMD="node"
set number
set ruler
set tildeop
set visualbell



syntax on
let g:syntastic_javascript_checkers = ['jsxhint']

" Text Formatting
set nowrap
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set smarttab
set cindent
let indent_guides_enable_on_vim_startup = 1
set listchars=tab:↠↠,trail:·,eol:↩,extends:>,precedes:<
set foldlevelstart=0
set backspace=indent,eol,start
set numberwidth=5
set scrolloff=14
if has('patch-7.3.541')
  set formatoptions+=j
endif

" Annoying temporary files
set backupdir=/tmp//,.
set directory=/tmp//,.
if v:version >= 703
  set undodir=/tmp//,.
endif


" disable sound on errors
set noerrorbells
set novisualbell
set t_vb=

" Search
set hlsearch   " highlight all matches
nohlsearch     " clear off any residual search when sourcing vimrc
set incsearch  " search as while typing
set ignorecase " case insensitive search
set smartcase  " be case sensitive if search includes caps

" Wildcards
set wildmenu
set wildmode=list:longest,list:full
set wildignore+=*.o,*.obj,.git,*.rbc

" Airline
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

"unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1
nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8
nmap <leader>9 <Plug>AirlineSelectTab9

" Status bar
set laststatus=2
set statusline=%.20F                     " Path to the file
set statusline+=%m                       " modified flag
set statusline+=\                        " Separator
set statusline+=%y                       " File type
set statusline+=\                        " Separator
" if !has('win32')
"   set statusline+=%{rvm#statusline()}      " Current Ruby version
" endif
set statusline+=\                        " Separator
set statusline+=%{fugitive#statusline()} " Git information
set statusline+=%=                       " Switch to the right side
set statusline+=(%c)                     " column number
set statusline+=%l                       " Current line
set statusline+=/                        " Separator
set statusline+=%L                       " Total Lines
let g:airline_theme='hybrid'

" Abbreviations - fixing my common typos
abbreviate ): );

" Font selection
if has('gui_running')
  if has('win32')
    set guifont=Consolas:h14
  else
    set guifont=Menlo:h18
  endif

  set go-=T
  " Don't show scroll bars in the GUI
  set guioptions-=L
  set guioptions-=r

endif

" Use modeline overrides
set modeline
set modelines=10

colorscheme Tomorrow-Night-Bright
"

set t_Co=256

set background=dark

" vim-pad directory

let g:pad_dir = '~/Shared/vim-pad'

" set the thesaurus
set thesaurus=~/.vim/mthesaur.txt
set dictionary=~/.vim/words

" load the plugin and indent settings for the detected filetype
filetype plugin indent on
" }}}
"

" Mouse droppings
" enable for normal mode
set mouse=n
" Set xterm2 mouse mode to allow resizing of splits with mouse inside tmux.
set ttymouse=xterm2

let g:localvimrc_sandbox=0

" NERDTree configuration"{{{
 " let NERDTreeQuitOnOpen=1
"}}}


" Manage vimrc ---------------------------------------- {{{
if has('win32')
  nnoremap <leader>ev :execute "edit ~/vimfiles/vimrc"<cr>
else
  nnoremap <leader>ev :execute "tabedit " . resolve($MYVIMRC)<cr>
  nnoremap <leader>elv :execute "edit ./.lvimrc"<cr>
endif
nnoremap <leader>sv :source $MYVIMRC<cr>
" }}}

" netrw-putty config for remote editing on work win32 machine-------------------- {{{
if has('win32')
  let g:netrw_cygwin = 0
  let g:netrw_ssh_cmd = 'plink -batch -T -ssh'
  let g:netrw_scp_cmd = 'pscp -batch -q -scp'
  let g:netrw_sftp_cmd = 'pscp -batch -q -sftp'
endif
" }}}

" expands %% to current file's directory in command-line mode
cnoremap %% <C-R>=fnameescape(expand('%:h')).'/'<CR>
" [S]plit line (sister to [J]oin lines)
" cc still substitutes the line like S would
nnoremap S i<CR><Esc>^mwgk:silent! s/\v +$//<CR>:noh<CR>


" " Relative numbering for speedy movement -------------------- {{{
" function! NumberToggle()
"   if(&relativenumber == 1)
"     set number
"   else
"     set relativenumber
"   endif
" endfunc

" " nnoremap <C-n> :call NumberToggle()<cr>
" " inoremap <C-n> :call NumberToggle()<cr>
" " vnoremap <C-n> :call NumberToggle()<cr>

" autocmd FocusLost   * :set number
" autocmd FocusGained * :set relativenumber

" autocmd InsertEnter * :set number
" autocmd InsertLeave * :set relativenumber
" " }}}

" Use hybrid numbering
set number
set relativenumber


" Surround word or visual selection with single or double quotes --- {{{
nnoremap <leader>" viw<esc>a"<esc>hbi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>hbi'<esc>lel
vnoremap <leader>" c""<esc>hpl
vnoremap <leader>' c''<esc>hpl
" }}}

" Miscellaneous mappings -------------------- {{{


" EasyAlign
" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" for screen.vim, send block to ScreenSend function
" (eg Scheme interpeter)
" from http://www.ktaylor.name/2009/11/vim-screen-lisp-programming-environment.html
" vnoremap <C-c><C-c> :ScreenSend<cr>
" nnoremap <C-c><C-c> vip:ScreenSend<cr>
"

" No more fat fingering help when I want Esc
nnoremap <silent> <f1> <esc>
nnoremap q: :q


" keep search pattern at the center of the screen (http://vimbits.com/bits/92)
nnoremap <silent> n nzz
nnoremap <silent> N Nzz
nnoremap <silent> * *zz
nnoremap <silent> # #zz
nnoremap <silent> g* g*zz
nnoremap <silent> g# g#zz


" navigate windows
" nnoremap <c-j> <c-w>j
" nnoremap <c-k> <c-w>k
" nnoremap <c-h> <c-w>h
" nnoremap <c-l> <c-w>l

" Indent guides
nnoremap <leader>ig :IndentLinesToggle<cr>

" Give Y a consistent behavior
nnoremap Y y$

" " Toggle NERDTree
" nnoremap <leader>t :NERDTreeFocus<cr>

" Smart Buffer Delete mappings -------------------- {{{
nnoremap <silent> <leader>sbd  :Sbd<cr>
nnoremap <silent> <leader>sbdm :Sbdm<cr>
" }}}


" Save
inoremap <C-s> <C-O>:update<CR>
nnoremap <C-s> :update<CR>

" Disable CTRL-A on tmux or on screen
if $TERM =~ 'screen'
  nnoremap <C-A> <nop>
  nnoremap <Leader><C-a> <C-a>
endif

" Jump List
nnoremap _ <C-O>
nnoremap + <C-I>

" <F4> | Tagbar
if v:version >= 703
  inoremap <F4> <ESC>:TagbarToggle<cr>
  nnoremap <F4> :TagbarToggle<cr>
  let g:tagbar_sort = 0
endif

" Automatically re-indent on paste -------------------- {{{
" nnoremap <leader>p p
" nnoremap <leader>P p
" nnoremap <leader>p p`[v`]=
" nnoremap <leader>P P`[v`]=
" overriding defaults seems to be more trouble than it's worth
" }}}

" Git --------------------"{{{
:nnoremap <leader>gs :Gstatus<cr>"}}}

" CtrlP config to replace Gary Bernhardt's Command-T config --- {{{
" from 'File Navigation with Vim'
" (http://www.destroyallsoftware.com/file-navigation-in-vim.html)
nnoremap <leader>f :CtrlPCurWD<cr>
nnoremap <leader>gf :CtrlPCurFile<cr>
nnoremap <leader>b :CtrlPBuffer<cr>
nnoremap <leader>m :CtrlPMRUFiles<cr>

" operator pending remaps --------------------{{{
" (i)n and (a)round (n)ext or (l)ast
onoremap in( :<c-u>normal! f(vi(<cr>
onoremap il( :<c-u>normal! F)vi(<cr>
onoremap an( :<c-u>normal! f(va(<cr>
onoremap al( :<c-u>normal! F)va(<cr>

onoremap in{ :<c-u>normal! f{vi{<cr>
onoremap il{ :<c-u>normal! F}vi{<cr>
onoremap an{ :<c-u>normal! f{va{<cr>
onoremap al{ :<c-u>normal! F}va{<cr>

onoremap in[ :<c-u>normal! f[vi[<cr>
onoremap il[ :<c-u>normal! F]vi[<cr>
onoremap an[ :<c-u>normal! f[va[<cr>
onoremap al[ :<c-u>normal! F]va[<cr>
" }}}

" Custom Rails specific CtrlP mappings
nnoremap <leader>gv :ClearCtrlPCache<cr>\|:CtrlP app/views<cr>
nnoremap <leader>gc :ClearCtrlPCache<cr>\|:CtrlP app/controllers<cr>
nnoremap <leader>gm :ClearCtrlPCache<cr>\|:CtrlP app/models<cr>
nnoremap <leader>gh :ClearCtrlPCache<cr>\|:CtrlP app/helpers<cr>
nnoremap <leader>gl :ClearCtrlPCache<cr>\|:CtrlP lib<cr>
nnoremap <leader>gp :ClearCtrlPCache<cr>\|:CtrlP public<cr>
" I think I prefer to use this shortcut for Git, but I'll keep it here for
" review at a later date.
" nnoremap <leader>gs :ClearCtrlPCache<cr>\|:CtrlP public/stylesheets<cr>
" }}}

" Emacs command line editing" -------------------- {{{

" start of line
:cnoremap <C-A>		<Home>

" back one character
:cnoremap <C-B>		<Left>

" delete character under cursor
" rk: I'd rather be able to expand help topics
" :cnoremap <C-D>		<Del>

" end of line
:cnoremap <C-E>		<End>

" forward one character
:cnoremap <C-F>		<Right>

" recall newer command-line
:cnoremap <C-N>		<Down>

" recall previous (older) command-line
:cnoremap <C-P>		<Up>

" rk: I don't like the way these slow down escaping from command mode
"" back one word
":cnoremap <Esc><C-B>	<S-Left>

"" forward one word
":cnoremap <Esc><C-F>	<S-Right>"}}}

" " ex commands are more common than finding the next [tTfF], -------------------- {{{
" " so let's swap `:` and `;`
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;
" This mapping along with mapping ; to : allows for quick save with ;w;
cnoremap w; w<CR>
" }}}

" source current file
nnoremap <leader>sf :source %<cr>

nnoremap H 0
"nnoremap L $
nnoremap  <leader><leader> <C-^>
nnoremap <leader>vp :execute "rightbelow vsplit " . bufname("#")<cr>"
nnoremap L      :nohlsearch<cr><c-l>

" always search magically
nnoremap / /\v

" grep in current directory for word under cursor
" nnoremap <leader>g :silent execute "grep! -R " . shellescape("<cWORD>") . " ."<cr>:copen<cr>

" remap ESC to home key combo for super fastness
" (mapped in both directions so you can just mash 'em!)
" inoremap kj <esc>
" inoremap jk <esc>
" allow continuous indent adjustment in visual mode
vnoremap < <gv
vnoremap > >gv
" CTags
map <Leader>rt :!ctags --extra=+f -R *<CR><CR>

" My fingers seem to prefer emacs for saving and quitting

nnoremap <c-x>s :w<cr>
nnoremap <c-x>c :wq<cr>

" Hide/Show NERDTree
nnoremap <Leader>t :NERDTreeToggle<cr>

" }}}

" Burn The Boats ---------------------------------------- {{{
" inoremap <esc>   <nop>
nmap  <Up>    <nop>
nmap  <Down>  <nop>
vmap  <Up>    <nop>
vmap  <Down>  <nop>
noremap  <Left>  <nop>
noremap  <Right> <nop>
" }}}

" File Settings -------------------- {{{
"
:au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

augroup load_us_ycm  "{{{
  autocmd!
  autocmd InsertEnter * call plug#load('ultisnips', 'YouCompleteMe')
                     \| call youcompleteme#Enable() | autocmd! load_us_ycm
augroup END "}}}

    " Real tabs -------------------- {{{
    augroup real_tabs
      " make and python use real tabs
      autocmd FileType make   set noexpandtab
      autocmd FileType python set noexpandtab
      autocmd BufRead,BufNewFile *.plist set noexpandtab
    augroup END
    " }}}
    " Vimscript -------------------- {{{
    augroup filetype_vim
      autocmd!
      autocmd FileType vim setlocal foldmethod=marker
    augroup END
    " }}}

    " " Jasmine specs --------------"{{{
    " augroup filetype_jasmine
    "   autocmd BufRead,BufNewFile *.spec.js setlocal foldmethod=indent foldlevel=3
    " augroup END
    " "}}}

    " Ruby -------------------- {{{
    augroup filetype_ruby
      " Thorfile, Rakefile and Gemfile are Ruby
      au BufRead,BufNewFile {Gemfile,Rakefile,Thorfile,config.ru}    set ft=ruby
    augroup END
    " }}}
    " Markdown -------------------- {{{
    augroup filetype_markdown
      " md, markdown, and mk are markdown and define buffer-local preview
      au BufRead,BufNewFile *.{md,markdown,mdown,mkd,mkdn} call s:setupMarkup()

      au BufRead,BufNewFile *.txt call s:setupWrapping()
      au BufRead,BufNewFile *.md setlocal ft=markdown
    augroup END
    " }}}
    "{{{
    augroup filetype_dokuwiki
      " *.docuwiki.txt files come from pentadactyl + dokuft plugin
      au BufRead,BufNewFile *.dokuwiki.txt setlocal ft=dokuwiki textwidth=0 wrapmargin=0
    augroup END
    "}}}
    function! s:setupWrapping()
      set wrap
      set wm=2
      set textwidth=72
    endfunction

    function! s:setupMarkup()
      call s:setupWrapping()
      map <buffer> <Leader>p :Mm <CR>
    endfunction
    " }}}
    " Git Commits ------------------------- {{{
    augroup git_commit
      au BufNewFile,BufRead COMMIT_EDITMSG setlocal spell
      au BufNewFile,BufRead COMMIT_EDITMSG call feedkeys('ggi', 't')
    augroup END

    "  }}}

    " augroup CursorLine"{{{
    "   au!
    "   au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    "   au VimEnter,WinEnter,BufWinEnter * setlocal cursorcolumn
    "   au WinLeave * setlocal nocursorline
    "   au WinLeave * setlocal nocursorcolumn
    " augroup END"
    " }}}

    " Functions --"{{{



    function! ToggleFoldWithNoHL()
      if foldlevel('.')
        normal! za
      endif
    endfunction

    nnoremap <silent> <space> :nohlsearch<cr>:call ToggleFoldWithNoHL()<cr>

    function! StripWhiteSpace()
      let save_cursor = getpos(".")
      let old_query = getreg('/')
      :%s/\s\+$//e
      call setpos('.', save_cursor)
      call setreg('/', old_query)
    endfunction

    noremap <leader>ss :call StripWhiteSpace()<CR>
    "}}}


    " Commands -- "{{{
    command! InsertTime :normal a<c-r>=strftime('%F %H:%M')<cr>
    "}}}
    " UltiSnips configuration"{{{
    "
    " source:  http://stackoverflow.com/questions/14896327/ultisnips-and-youcompleteme

    " function! g:UltiSnips_Complete()
    "   call UltiSnips_ExpandSnippet()
    "   if g:ulti_expand_res == 0
    "     if pumvisible()
    "       return "\<C-n>"
    "     else
    "       call UltiSnips_JumpForwards()
    "       if g:ulti_jump_forwards_res == 0
    "         return "\<TAB>"
    "       endif
    "     endif
    "   endif
    "   return ""
    " endfunction

    let g:UltiSnipsExpandTrigger="<c-j>"

    " au BufEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"
    let g:UltiSnipsJumpForwardTrigger="<c-f>"
    let g:UltiSnipsJumpBackwardTrigger="<c-k>"
    let g:UltiSnipsListSnippets="<c-e>"
    "}}}

    " Include user's local vim config
    if filereadable(expand("~/.vimrc.local"))
      source ~/.vimrc.local
    endif
