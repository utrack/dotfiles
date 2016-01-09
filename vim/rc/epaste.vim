function InsertPaste()
  set paste
  " mark current pos to p
  normal! mp
  normal! "+p
  " go to start, write as prev move
  normal! `[
  normal! m'
  " go to paste's finish
  normal! `]
  set nopaste
endfunction

inoremap <c-v> <c-o>:call InsertPaste()<CR>
