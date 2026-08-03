" Vim syntax file for WatSup (P4-SpecTec DSL)
" Derived from p4spec/lib/frontend/lexer.mll

if exists("b:current_syntax")
  finish
endif

" Comments
"   ;; line comment
"   (; ... ;) block comment (nestable)
syn match  watsupLineComment ";;.*$" contains=@Spell
syn region watsupBlockComment start="(;" end=";)" contains=watsupBlockComment,@Spell

" Keywords
syn keyword watsupKeyword syntax extern tbl relation if otherwise debug builtin
syn keyword watsupKeyword var                                nextgroup=watsupName        skipwhite
syn keyword watsupKeyword dec def                            nextgroup=watsupFunction    skipwhite
syn keyword watsupKeyword rule rulegroup                     nextgroup=watsupRuleName    skipwhite

" Built-in types
syn keyword watsupType bool nat int text

" Constants / literals
syn keyword watsupConstant eps true false

" %latex directive
syn match watsupDirective "%latex\>"

" Hint clause: hint( ... )
syn match watsupHint "hint\ze("

" Names after declaration keywords (relation/var name — usually UPID)
syn match watsupName     "[A-Za-z_$][A-Za-z0-9_$']*"  display contained

" Function names after dec/def (may be $-prefixed)
syn match watsupFunction "\$\?[A-Za-z_][A-Za-z0-9_']*" display contained

" Rule names after rule/rulegroup, with optional /variant suffix
syn match watsupRuleName "[A-Za-z_][A-Za-z0-9_']*" display contained nextgroup=watsupRuleVariant skipwhite
syn match watsupRuleVariant "/[A-Za-z0-9_']*" display contained

" Silent tag: _UPID
syn match watsupTag "\<_[A-Z][A-Za-z0-9_']*"

" Concrete operator literal: '...'
syn region watsupOperatorLit start="'" skip="\\'" end="'" oneline

" Dot-prefixed field id: .id
syn match watsupField "\.[A-Za-z_][A-Za-z0-9_']*"

" Strings
syn region watsupString start='"' skip='\\"' end='"' contains=watsupEscape
syn match  watsupEscape contained '\\\(u{[0-9A-Fa-f]\+}\|x\?[0-9A-Fa-f]\{2}\|[nrt\\''"]\)'

" Numbers
syn match watsupNumber '\<0x[0-9A-F]\+\(_[0-9A-F]\+\)*\>'
syn match watsupNumber '\<\d\+\(_\d\+\)*\>'
syn match watsupNumber '[+-]\d\+\(_\d\+\)*\>'

" Type arguments in angle brackets: <foo>
syn match watsupTypeParameter '\v\<\zs[a-z_][a-zA-Z0-9_]*\ze\>'

" Backtick target brackets: `( `) `[ `] `{ `} `< `>
syn match watsupTickBracket "`[()[\]{}<>]"

" Holes: % %N %% !%
syn match watsupHole "!\?%\%(%\|\d\+\)\?"

" Delimiters
syn match watsupDelimiter "[(){}[\]]"

" Notational / turnstile operators (multi-char first)
syn match watsupOperator "==>"
syn match watsupOperator "<=>"
syn match watsupOperator "=>_\?"
syn match watsupOperator "->_\?"
syn match watsupOperator "\~>\*\?"
syn match watsupOperator "|-"
syn match watsupOperator "-|"

" Logical
syn match watsupOperator "/\\"
syn match watsupOperator "\\/"

" Comparison / assignment
syn match watsupOperator "=/="
syn match watsupOperator "<="
syn match watsupOperator ">="
syn match watsupOperator ":="
syn match watsupOperator "<:"
syn match watsupOperator "<-"
syn match watsupOperator ">("
syn match watsupOperator "\~\~"

" Single / short operators and punctuation
syn match watsupOperator "[:;,.|=<>~?^$#*/\\+-]"

" Highlighting links
hi def link watsupLineComment    Comment
hi def link watsupBlockComment   Comment
hi def link watsupKeyword        Keyword
hi def link watsupType           Type
hi def link watsupTypeParameter  Type
hi def link watsupConstant       Constant
hi def link watsupDirective      PreProc
hi def link watsupHint           PreProc
hi def link watsupString         String
hi def link watsupEscape         SpecialChar
hi def link watsupOperatorLit    String
hi def link watsupNumber         Number
hi def link watsupName           Identifier
hi def link watsupFunction       Function
hi def link watsupRuleName       Function
hi def link watsupRuleVariant    Identifier
hi def link watsupTag            Special
hi def link watsupField          Identifier
hi def link watsupTickBracket    Delimiter
hi def link watsupHole           Special
hi def link watsupOperator       Operator
hi def link watsupDelimiter      Delimiter

let b:current_syntax = "watsup"
