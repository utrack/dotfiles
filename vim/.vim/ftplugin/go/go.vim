let g:unite_source_menu_menus.gol = {
    \ 'description' : '            Golang commands
        \                            ⌘ f',
    \}
let g:unite_source_menu_menus.gol.command_candidates = [
    \['▷ go fmt',
        \'GoFmt'],
    \['▷ Fix imports (fi)',
        \'GoImports'],
    \['▷ Go to definition (ds)',
        \'GoDef'],
    \['▷ Show docs (dd)',
        \'GoDoc'],
    \['▷ Implements (ii)',
        \'GoImplements'],
    \['▷ Run tests',
        \'GoTest'],
    \]
nnoremap <Leader>f :Unite -silent -start-insert menu:gol<CR>
