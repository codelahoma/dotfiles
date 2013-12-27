" Rod Knowlton's vimrc
"
" Copy at your own risk.
"
" Use Vim settings. Must run before other settings
set nocompatible

let g:ft_ignore_pat = '\.org'

" Vundle----------------------------------------"{{{

" Setting up Vundle - the vim plugin bundler
let iCanHazVundle=1
let vundle_readme=expand('~/.vim/bundle/vundle/README.md')
if !filereadable(vundle_readme)
  echo "Installing Vundle.."
  echo ""
  silent !mkdir -p ~/.vim/bundle
  silent !git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle
  let iCanHazVundle=0
endif
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'
"Add your bundles here
filetype off
if has('win32')
  set rtp+=~/vimfiles/bundle/vundle
else
  set rtp+=~/.vim/bundle/vundle
endif
call vundle#rc()

" github repos

Bundle 'bling/vim-airline'
Bundle 'vim-misc'
Bundle 'dogrover/vim-pentadactyl'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'SirVer/ultisnips'
Bundle 'gmarik/vundle'
Bundle 'majutsushi/tagbar'
Bundle 'mattn/gist-vim'
Bundle 'nanotech/jellybeans.vim'
Bundle 'programble/itchy.vim'
Bundle 'roman/golden-ratio'
Bundle 'scrooloose/nerdtree'
Bundle 'sjl/gundo.vim'
Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'vim-scripts/Align'
Bundle 'kakkyz81/evervim'
Bundle 'nathanaelkane/vim-indent-guides'
Bundle 'Valloric/YouCompleteMe'
Bundle 'Syntastic'
Bundle 'chrisbra/NrrwRgn'
Bundle 'tpope/vim-speeddating'
Bundle 'calendar.vim'

" Language Additions
"  Ruby

Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-rake'
Bundle 'tpope/vim-rvm'
Bundle 'vim-ruby/vim-ruby'

" Javascript
Bundle 'pangloss/vim-javascript'
Bundle 'jelera/vim-javascript-syntax'

" Coffeescript
Bundle 'kchmck/vim-coffee-script'

" Clojure
Bundle 'https://github.com/tpope/vim-classpath.git'
Bundle 'https://github.com/tpope/vim-fireplace.git'

Bundle 'https://github.com/kien/rainbow_parentheses.vim.git'
Bundle 'vim-slamhound'

Bundle 'altercation/vim-colors-solarized'
Bundle 'zenorocha/dracula-theme'

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

Bundle 'https://github.com/guns/vim-clojure-static.git'
Bundle 'paredit.vim'
" vimscripts.org

Bundle 'ZoomWin'
Bundle 'ctrlp.vim'
Bundle 'dbext.vim'
Bundle 'matchit.zip'
Bundle 'ack.vim'
Bundle 'localvimrc'
Bundle 'errormarker.vim'
Bundle 'AsyncCommand'
Bundle 'WebAPI.vim'
Bundle 'Puppet-Syntax-Highlighting'

filetype plugin indent on
if iCanHazVundle == 0
  echo "Installing Bundles, please ignore key map error messages"
  echo ""
  :BundleInstall
endif
" Setting up Vundle - the vim plugin bundler end


"}}}


" Other Sourcings----------------------------------------"{{{
source $VIMRUNTIME/ftplugin/man.vim
" }}}

" Org Mode  ----------------------------------------"{{{
au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
au BufEnter *.org            call org#SetOrgFileType()
" let g:org_capture_file = '~/org_files/mycaptures.org'
command! OrgCapture :call org#CaptureBuffer()
command! OrgCaptureFile :call org#OpenCaptureFile()"}}}

" Settings ---------------------------------------- {{{
set encoding=utf-8

" Persist undo history across sessions
set undofile
set undodir=~/.vim/undo

" Allow backgrounding buffers without writing them, and remember marks/undo
" for backgrounded buffers
set hidden

" Automatically leave insert mode after 'updatetime' (4s by default).
" au CursorHoldI * stopinsert

" Remember more commands and search history
set history=1000

let EasyMotion_leader_key = '\'
let mapleader = ','
let maplocalleader = '\'
let $JS_CMD="node"
set number
set ruler
set scrolloff=5
set tildeop
set visualbell
syntax on

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
set so=14

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

" Status bar
set laststatus=2
set statusline=%.20F                     " Path to the file
set statusline+=%m                       " modified flag
set statusline+=\                        " Separator
set statusline+=%y                       " File type
set statusline+=\                        " Separator
if !has('win32')
  set statusline+=%{rvm#statusline()}      " Current Ruby version
endif
set statusline+=\                        " Separator
set statusline+=%{fugitive#statusline()} " Git information
set statusline+=%=                       " Switch to the right side
set statusline+=(%c)                     " column number
set statusline+=%l                       " Current line
set statusline+=/                        " Separator
set statusline+=%L                       " Total Lines

" Abbreviations - fixing my common typos
abbreviate ): );

" Font selection
if has('gui_running')
  if !has('win32')
    set guifont=Menlo:h18
  else
    set guifont=Consolas:h14
  endif

  set go-=T
  " Don't show scroll bars in the GUI
  set guioptions-=L
  set guioptions-=r

endif

" Use modeline overrides
set modeline
set modelines=10

" Default color scheme
" if has('win32') || !has('gui_running')
" 	colorscheme blackboard
" else
" 	colorscheme solarized
" endif
"
" colorscheme jellybeans
"

set t_Co=256
" set background=light
" colorscheme solarized

set background=dark
colorscheme torte
" if !has('gui_running')
"   colorscheme slate
" endif

" vim-pad directory

let g:pad_dir = '~/Shared/vim-pad'

"Directories for swp files
set backupdir=~/.vim/backup
set directory=~/.vim/backup
"
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
let NERDTreeQuitOnOpen=1
"}}}

" UltiSnips configuration"{{{
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
"}}}

" Manage vimrc ---------------------------------------- {{{
if has('win32')
  nnoremap <leader>ev :execute "edit ~/vimfiles/vimrc"<cr>
else
  nnoremap <leader>ev :execute "edit " . resolve($MYVIMRC)<cr>
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


" Relative numbering for speedy movement -------------------- {{{
function! NumberToggle()
if(&relativenumber == 1)
set number
        else
          set relativenumber
        endif
      endfunc

      " nnoremap <C-n> :call NumberToggle()<cr>
      " inoremap <C-n> :call NumberToggle()<cr>
      " vnoremap <C-n> :call NumberToggle()<cr>

      autocmd FocusLost   * :set number
      autocmd FocusGained * :set relativenumber

      autocmd InsertEnter * :set number
      autocmd InsertLeave * :set relativenumber
      " }}}

      " Surround word or visual selection with single or double quotes --- {{{
      nnoremap <leader>" viw<esc>a"<esc>hbi"<esc>lel
      nnoremap <leader>' viw<esc>a'<esc>hbi'<esc>lel
      vnoremap <leader>" c""<esc>hpl
      vnoremap <leader>' c''<esc>hpl
      " }}}

      " Miscellaneous mappings -------------------- {{{

      " for screen.vim, send block to ScreenSend function
      " (eg Scheme interpeter)
      " from http://www.ktaylor.name/2009/11/vim-screen-lisp-programming-environment.html
      " vnoremap <C-c><C-c> :ScreenSend<cr>
      " nnoremap <C-c><C-c> vip:ScreenSend<cr>
      "

      " keep search pattern at the center of the screen (http://vimbits.com/bits/92)
      nnoremap <silent> n nzz
      nnoremap <silent> N nzz
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
      nnoremap <leader>ig :IndentGuidesToggle<cr>

      " Give Y a consistent behavior
      nnoremap Y y$

      " Toggle NERDTree
      nnoremap <leader>t :NERDTreeToggle<cr>

      " Smart Buffer Delete mappings -------------------- {{{
      nnoremap <silent> <leader>sbd  :Sbd<cr>
      nnoremap <silent> <leader>sbdm :Sbdm<cr>
      " }}}

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
      nnoremap <f2> :NERDTreeToggle<cr>

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

      " Jasmine specs --------------"{{{
      augroup filetype_jasmine
        autocmd BufRead,BufNewFile *.{spec.js} setlocal foldmethod=indent foldlevel=2
      augroup END
      "}}}

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

  augroup CursorLine"{{{
    au!
    au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    au VimEnter,WinEnter,BufWinEnter * setlocal cursorcolumn
    au WinLeave * setlocal nocursorline
    au WinLeave * setlocal nocursorcolumn
  augroup END"}}}
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

" Include user's local vim config
if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
endif
