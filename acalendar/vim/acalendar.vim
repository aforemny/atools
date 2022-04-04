if exists("b:current_syntax")
  finish
endif

" syn match Constant '.'
" syn keyword Statement FROM UNTIL
" syn keyword Type Mon Tue Wed Thu Fri Sat Sun
" syn keyword Type Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
" syn match Type '\d\{1,2\}'
" syn match Type '\d\{4\}'
" syn match Type '\d\{2\}:\d\{2\}-\d\{2\}:\d\{2\}'
" syn match Type '\d\{4\}-\d\{2\}-\d\{2\}'

syn region Constant start='^' skip='\$' end='$' contains=Type
