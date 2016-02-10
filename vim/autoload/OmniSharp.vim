

let s:save_cpo = &cpo
set cpo&vim

function! OmniSharp#Complete(findstart, base) abort
  if a:findstart
    "store the current cursor position
    let s:column = col('.')
    "locate the start of the word
    let line = getline('.')
    let start = col('.') - 1
    let s:textBuffer = getline(1, '$')
    while start > 0 && line[start - 1] =~# '\v[a-zA-z0-9_]'
      let start -= 1
    endwhile

    return start
  else
    return pyeval('Completion().get_completions("s:column", "a:base")')
  endif
endfunction

function! OmniSharp#GetCodeActions(mode) range abort
  if g:OmniSharp_selector_ui ==? 'unite'
    let context = {'empty': 0, 'auto_resize': 1}
    call unite#start([['OmniSharp/findcodeactions', a:mode]], context)
  elseif g:OmniSharp_selector_ui ==? 'ctrlp'
    let actions = pyeval(printf('getCodeActions(%s)', string(a:mode)))
    if empty(actions)
      echo 'No code actions found'
      return
    endif
    call ctrlp#OmniSharp#findcodeactions#setactions(a:mode, actions)
    call ctrlp#init(ctrlp#OmniSharp#findcodeactions#id())
  else
    echo 'No selector plugin found.  Please install unite.vim or ctrlp.vim'
  endif
endfunction

function! OmniSharp#AppendCtrlPExtensions() abort
  " Don't override settings made elsewhere
  if !exists('g:ctrlp_extensions')
    let g:ctrlp_extensions = []
  endif
  if !exists('g:OmniSharp_ctrlp_extensions_added')
    let g:OmniSharp_ctrlp_extensions_added = 1
    let g:ctrlp_extensions += ['findcodeactions']
  endif
endfunction


let &cpo = s:save_cpo
unlet s:save_cpo
