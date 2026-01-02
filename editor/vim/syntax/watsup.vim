if exists("b:current_syntax")
  finish
endif

" Comments
syn match watsupComment ";;.*$"

" Keywords (with nextgroup for function names)
syn keyword watsupKeyword syntax var if otherwise hint input
syn keyword watsupKeyword dec nextgroup=watsupFunction skipwhite
syn keyword watsupKeyword def nextgroup=watsupFunction skipwhite
syn keyword watsupKeyword rule nextgroup=watsupRuleName skipwhite
syn keyword watsupKeyword relation nextgroup=watsupFunction skipwhite

" Built-in types
syn keyword watsupType int nat text bool set map

" Constants
syn keyword watsupConstant eps true false infinity

" String literals
syn region watsupString start='"' end='"'

" Numbers
syn match watsupNumber '\<\d\+\>'

" Function/relation names (contained - only after keywords, no slash)
syn match watsupFunction "[a-zA-Z_$][a-zA-Z0-9_$'-]*" display contained

" Rule names (contained - only after rule keyword, base name part)
syn match watsupRuleName "[a-zA-Z_$][a-zA-Z0-9_$'-]*" display contained nextgroup=watsupRuleComment skipnl skipwhite

" Rule comment part (contained - only after rule name, starts with /)
syn match watsupRuleComment "/[a-zA-Z0-9_$'-]*" display contained

" Type parameters in angle brackets
syn match watsupTypeParameter '\v\<\zs[a-zA-Z_][a-zA-Z0-9_]*\ze\>'

" Operators and delimiters (ordered as in lexer.mll)
" Basic delimiters
syn match watsupDelimiter '[(){}[\]]'            " LPAREN, RPAREN, LBRACE, RBRACE, LBRACK, RBRACK
syn match watsupOperator ':'                     " COLON
syn match watsupOperator '::'                    " COLONCOLON
syn match watsupOperator ':/'                    " COLONSLASH
syn match watsupOperator ';'                     " SEMICOLON
syn match watsupOperator ','                     " COMMA
syn match watsupOperator '\.'                    " DOT
syn match watsupOperator '\.\.'                  " DOTDOT
syn match watsupOperator '\.\.\.'                " DOTDOTDOT
syn match watsupOperator '|'                     " BAR
syn match watsupOperator '--'                    " DASH
syn match watsupOperator '='                     " EQ
syn match watsupOperator '=/='                   " NE
syn match watsupOperator '<'                     " LANGLE
syn match watsupOperator '>'                     " RANGLE
syn match watsupOperator '>('                    " RANGLE_LPAREN
syn match watsupOperator '<='                    " LE
syn match watsupOperator '>='                    " GE
syn match watsupOperator '\~\~'                  " APPROX
syn match watsupOperator '<:'                    " SUB
syn match watsupOperator ':>'                    " SUP
syn match watsupOperator ':='                    " ASSIGN
syn match watsupOperator '=='                    " EQUIV
syn match watsupOperator '\~'                    " NOT
syn match watsupOperator '/\\'                   " AND
syn match watsupOperator '\\/'                   " OR
syn match watsupOperator '(/\\)'                 " BIGAND
syn match watsupOperator '(\\/)'                 " BIGOR
syn match watsupOperator '(+)'                   " BIGADD
syn match watsupOperator '(*)'                   " BIGMUL
syn match watsupOperator '(++)'                  " BIGCAT
syn match watsupOperator '?'                     " QUEST
syn match watsupOperator '+'                     " PLUS
syn match watsupOperator '-'                     " MINUS
syn match watsupOperator '*'                     " STAR
syn match watsupOperator '/'                     " SLASH
syn match watsupOperator '\\'                    " BACKSLASH
syn match watsupOperator '\^'                    " UP
syn match watsupOperator '++'                    " CAT
syn match watsupOperator '<-'                    " MEM
syn match watsupOperator '->'                    " ARROW
syn match watsupOperator '=>'                    " ARROW2
syn match watsupOperator '->_'                   " ARROWSUB
syn match watsupOperator '=>_'                   " ARROW2SUB
syn match watsupOperator '<=>'                   " DARROW2
syn match watsupOperator '\~>'                   " SQARROW
syn match watsupOperator '\~>*'                  " SQARROWSTAR
syn match watsupOperator '<<'                    " PREC
syn match watsupOperator '>>'                    " SUCC
syn match watsupOperator '|-'                    " TURNSTILE
syn match watsupOperator '-|'                    " TILESTURN
syn match watsupOperator '\$'                    " DOLLAR
syn match watsupOperator '_|_'                   " BOT
syn match watsupOperator '\^|\^'                 " TOP
syn match watsupOperator '%'                     " HOLE
syn match watsupOperator '%%'                    " MULTIHOLE
syn match watsupOperator '!%'                    " NOTHING
syn match watsupOperator '#'                     " FUSE
syn match watsupOperator '##'                    " FUSEFUSE
syn match watsupOperator '`'                     " TICK

" Highlighting
hi def link watsupComment Comment
hi def link watsupKeyword Keyword
hi def link watsupType Type
hi def link watsupTypeParameter Type
hi def link watsupConstant Constant
hi def link watsupString String
hi def link watsupNumber Number
hi def link watsupFunction Function
hi def link watsupRuleName Function
hi def link watsupRuleComment Identifier
hi def link watsupOperator Operator
hi def link watsupDelimiter Delimiter

let b:current_syntax = "watsup"
