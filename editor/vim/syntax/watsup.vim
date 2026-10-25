" Vim syntax file for WatSup (P4-SpecTec DSL)
" Syntax reference: p4spectec/src/frontend/lexer.rs

if exists("b:current_syntax")
  finish
endif

" Identifiers may contain primes: x', C'', typedExp_out'*
syn iskeyword @,48-57,_,39

" Plain identifiers; consuming them keeps a prime from opening an operator
syn match watsupIdent "\<\h\k*" contains=NONE

" Keywords
syn keyword watsupKeyword extern tbl builtin if otherwise debug
syn keyword watsupKeyword syntax                             nextgroup=watsupTypeName    skipwhite
syn keyword watsupKeyword relation                           nextgroup=watsupRelName     skipwhite
syn keyword watsupKeyword var                                nextgroup=watsupName        skipwhite
syn keyword watsupKeyword dec def                            nextgroup=watsupFunction    skipwhite
syn keyword watsupKeyword rule rulegroup                     nextgroup=watsupRuleName    skipwhite

" Built-in types
syn keyword watsupType bool nat int text

" Constants / literals
syn keyword watsupConstant eps true false

" Hint clause: hint( ... )
syn match watsupHint "\<hint\ze("

" Names after declaration keywords
syn match watsupTypeName "\<\h\k*"        display contained
syn match watsupRelName  "\<\h\k*"        display contained
syn match watsupName     "\<\h\k*"        display contained
syn match watsupFunction "\$\?\<\h\k*"    display contained

" Rule names after rule/rulegroup, with optional /variant suffix
syn match watsupRuleName "\<\h\k*" display contained nextgroup=watsupRuleVariant
syn match watsupRuleVariant "/\k*" display contained

" Delimiters
syn match watsupDelimiter "[(){}[\]]"

" Single-character operators first, so longer spellings below win
syn match watsupOperator "[:;,.|=<>~?^$#*/\\+%-]"

" Notational / turnstile operators
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

" Comparison / assignment / list
syn match watsupOperator "=/="
syn match watsupOperator "<="
syn match watsupOperator ">="
syn match watsupOperator ":="
syn match watsupOperator "::"
syn match watsupOperator ":/"
syn match watsupOperator "<:"
syn match watsupOperator "<-"
syn match watsupOperator ">("
syn match watsupOperator "\~\~"
syn match watsupOperator "++"
syn match watsupOperator "##"
syn match watsupOperator "\.\.\.\?"

" Silent tag: _UPID, unless a ( or < follows (then it is a call)
syn match watsupTag "\<_\u\k*\>[(<]\@!"

" Dot-prefixed field id: .id
syn match watsupField "\.\h\k*"

" Strings
syn region watsupString start='"' skip='\\\\\|\\"' end='"' oneline contains=watsupEscape
syn match  watsupEscape contained '\\\(u{\x\+}\|[0-9A-F]\{2}\|[nrt\\''"]\)'

" Numbers: decimal and uppercase hex, with single _ separators
syn match watsupNumber '\<0x[0-9A-F]\+\(_[0-9A-F]\+\)*\>'
syn match watsupNumber '\<\d\+\(_\d\+\)*\>'

" Backtick target brackets: `( `) `[ `] `{ `} `< `>
syn match watsupTickBracket "`[()[\]{}<>]"

" Premise marker, naming the relation for -- Rel: premises
syn match watsupOperator "--" nextgroup=watsupPremiseRel skipwhite
syn match watsupPremiseRel "\<\h\k*\ze\s*:[:=/]\@!" display contained

" Function call: $name
syn match watsupCall "\$\h\k*"

" Holes: % %N %% !%
syn match watsupHole "%\d\+\(_\d\+\)*"
syn match watsupHole "%%"
syn match watsupHole "!%"

" %latex directive (after holes, so % does not split it)
syn match watsupDirective "%latex\>"

" Type arguments fused to a name: $f<X>(...), map<K, V>
syn region watsupTypeArgs matchgroup=watsupOperator start="\k\@1<=<" end=">" oneline
      \ contains=watsupTypeArg,watsupType,watsupTypeArgs,watsupOperator,watsupDelimiter
syn match watsupTypeArg "\<\h\k*" display contained

" Concrete operator literal: '...', closed on the same line, no escapes
syn region watsupOperatorLit start="'" end="'" oneline

" Comments last, so ; and ( never claim their start
"   ;; line comment
"   (; ... ;) block comment (nestable)
syn match  watsupLineComment ";;.*$" contains=@Spell
syn region watsupBlockComment start="(;" end=";)" contains=watsupBlockComment,@Spell

syn sync ccomment watsupBlockComment minlines=50

" Highlighting links
hi def link watsupLineComment    Comment
hi def link watsupBlockComment   Comment
hi def link watsupKeyword        Keyword
hi def link watsupType           Type
hi def link watsupTypeName       Type
hi def link watsupTypeArg        Type
hi def link watsupConstant       Constant
hi def link watsupDirective      PreProc
hi def link watsupHint           PreProc
hi def link watsupString         String
hi def link watsupEscape         SpecialChar
hi def link watsupOperatorLit    String
hi def link watsupNumber         Number
hi def link watsupName           Identifier
hi def link watsupFunction       Function
hi def link watsupCall           Function
hi def link watsupRelName        Function
hi def link watsupRuleName       Function
hi def link watsupPremiseRel     Function
hi def link watsupRuleVariant    Identifier
hi def link watsupTag            Special
hi def link watsupField          Identifier
hi def link watsupTickBracket    Delimiter
hi def link watsupHole           Special
hi def link watsupOperator       Operator
hi def link watsupDelimiter      Delimiter

let b:current_syntax = "watsup"
