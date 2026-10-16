require "rouge"

module Rouge
  module Lexers
    class Watsup < RegexLexer
      title "Watsup"
      desc "SpecTec language definition language"
      tag "watsup"
      aliases "spectec"
      filenames "*.watsup"

      IDENTIFIER = /[A-Za-z_][A-Za-z0-9_']*/
      FUNCTION = /\$[A-Za-z_][A-Za-z0-9_']*/
      RULE_ID = /(?:[A-Za-z_][A-Za-z0-9_']*|[0-9]+)(?:\.[A-Za-z_][A-Za-z0-9_']*)*/
      RULE_NAME = /#{IDENTIFIER}(?:[\/-]#{RULE_ID})*/

      state :root do
        rule %r/\s+/, Text::Whitespace
        rule %r/;;[^\n]*/, Comment::Single
        rule %r/\(;/, Comment::Multiline, :block_comment
        rule %r/"/, Str::Double, :string
        rule %r/'[^'\n]*'/, Str::Single

        rule %r/\b(rulegroup|rule)(\s+)(#{RULE_NAME})/ do
          groups Keyword, Text::Whitespace, Name::Label
        end
        rule %r/\b(syntax|relation|var)(\s+)(#{IDENTIFIER})/ do
          groups Keyword, Text::Whitespace, Name::Class
        end
        rule %r/(--)(\s+)(#{IDENTIFIER})(:\/?)/ do
          groups Keyword, Text::Whitespace, Name::Class, Operator
        end

        rule FUNCTION, Name::Function
        rule %r/\b(bool|nat|int|text)\b/, Keyword::Type
        rule %r/\b(eps|true|false)\b/, Keyword::Constant
        rule %r/\b(syntax|extern|tbl|relation|rulegroup|rule|var|builtin|dec|def|if|otherwise|debug)\b/,
          Keyword
        rule %r/--/, Keyword
        rule %r/hint(?=\()/, Name::Decorator
        rule %r/%latex\b/, Comment::Preproc

        rule %r/_[A-Z][A-Za-z0-9_']*/, Name::Tag
        rule %r/\.[A-Za-z_][A-Za-z0-9_']*/, Name::Attribute
        rule %r/!?(?:%%|%[0-9]+|%)/, Str::Symbol
        rule %r/0x[0-9A-F]+(?:_[0-9A-F]+)*/, Num::Hex
        rule %r/[+-]?[0-9]+(?:_[0-9]+)*/, Num::Integer
        rule %r/`[()\[\]{}<>]/, Punctuation
        rule %r/(?:==>|<=>|=>_|=>|->_|->|~>\*|~>|\|-|-\||\/\\|\\\/|=\/=|<=|>=|:=|<:|<-|>\(|~~|::|\.\.\.|\.\.|[\[\](){}:;,.|=<>~?^$#*\/\\+\-])/,
          Operator
        rule %r/[A-Z][A-Za-z0-9_']*/, Name::Constant
        rule IDENTIFIER, Name::Variable
        rule %r/./m, Text
      end

      state :block_comment do
        rule %r/\(;/, Comment::Multiline, :push
        rule %r/;\)/, Comment::Multiline, :pop!
        rule %r/(?!(?:\(;|;\)))[\s\S]/, Comment::Multiline
      end

      state :string do
        rule %r/\\(?:[nrt\\'"]|[0-9A-Fa-f]{2}|u\{[0-9A-F]+\})/,
          Str::Escape
        rule %r/[^"\\]+/, Str::Double
        rule %r/"/, Str::Double, :pop!
        rule %r/\\./, Error
      end
    end
  end
end
