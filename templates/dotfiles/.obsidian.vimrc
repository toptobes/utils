let mapleader=" "

"" -- Suggested options --
" Show a few lines of context around the cursor. Note that this makes the
" text scroll if you mouse-click near the start or end of the window.
set scrolloff=5

" Do incremental searching.
set incsearch

" Don't use Ex mode, use Q for formatting.
map Q gq

" Use system clipboard when pasting (or something)
set clipboard+=unnamed

" esc in insert & command mode
inoremap kj <Esc>
cnoremap kj <C-C>

" Quickly insert an empty new line without entering insert mode
nnoremap <Leader>o o<Esc>0
nnoremap <Leader>O O<Esc>0

" Forces me to use hjkl
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

nmap H ^
nmap L $
