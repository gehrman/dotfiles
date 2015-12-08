" Sanity for python tabbing.
set ts=4
set expandtab
syntax on

" Show relative line numbers by default.
set relativenumber

" Leader is basically another mod-key.
let mapleader = ','

" Toggle between relative and absolute line numbering, and bind it to ^n.
function! NumberToggle()
  if(&number == 0 && &relativenumber == 0)
    if(w:old_type == "absolute")
      set number
    else
      set relativenumber
    endif
  elseif(&number == 1)
    set relativenumber
  else
    set number
  endif
endfunc
"nnoremap <C-n> :call NumberToggle()<cr>
nnoremap <leader>n :call NumberToggle()<cr>

" Toggle between current line numbering and no numbering, and bind it to ^m.
function! NumberingToggle()
  if(&number == 1 || &relativenumber == 1)
    "Save old number type so we can restore it later.
    if(&number == 1)
      let w:old_type = "absolute"
      set nonumber
    else
      let w:old_type = "relative"
      set norelativenumber
    endif
  else
    if (w:old_type == "absolute")
      set number
    else
      set relativenumber
    endif
  endif
endfunc
nnoremap <leader>m :call NumberingToggle()<cr>

" Swap absolute and relative sometimes.
":au FocusLost * :set number
":au FocusGained * :set relativenumber


"Comment/uncomment a line.
" This doesn't work as is. :-(
"echo b:current_syntax
"if b:current_syntax == 'sql'
"    let @c = '0i-- j0'
"    let @u = '03xj'
"elseif b:current_syntax == 'python'
"    let @c = '0i# j0'
"    let @u = '02xj'
"elseif b:current_syntax == 'vim'
"    let @c = '0i" j0'
"    let @u = '02xj'
"endif
"
nnoremap <leader>p let @c = '0i# j0'
nnoremap <leader>P let @u = '02xj'
nnoremap <leader>v let @c = '0i" j0'
nnoremap <leader>V let @u = '02xj'
nnoremap <leader>s let @c = '0i# j0'
nnoremap <leader>S let @u = '03xj'
nnoremap <C-c> @c
nnoremap <C-u> @u

nnoremap <leader>J r<Cr><Esc>

" Diff the current file state with the state on disk.
" Taken from
" http://vim.wikia.com/wiki/Diff_current_buffer_and_the_original_file
"
" This can probably be modified to enable diffing with git head, as
" there are versions for cvs diff and svn diff. But maybe silly since
" git is a dvcs.
function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSaved call s:DiffWithSaved()
