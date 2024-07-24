{
module Parsing.Parser (happyParseCCSEnvironment) where

import Expression (
  BExpr (..),
  BOp (..),
  Expr (..),
  LOp (..),
  Name,
  Op (Div, Minus, Mul),
  Ty (..),
  Var,
  natMax )
import qualified Expression as E (Op (Plus))
import Parsing.Lexer
import Process (Act (..), Chan, Proc (..))
}

%lexer { lexer } { Token _ EOF }
%monad { Alex } { (>>=) } { return }
%name happyParseCCSEnvironment
%tokentype { Token }

%token
  "\cdot"      { Token _ BackslashCdot }
  "\land"      { Token _ BackslashLand }
  "\tau"       { Token _ BackslashTau }
  "\neg{"      { Token _ BackslashNegLeftCurlyBracket }
  "\overline{" { Token _ BackslashOverlineLeftCurlyBracket }
  "\\"         { Token _ Backslash }
  capitalized  { Token _ (Capitalized $$) }
  ":"          { Token _ Colon }
  ","          { Token _ Comma }
  "."          { Token _ Dot }
  "="          { Token _ Equal }
  integer      { Token _ (Integer $$) }
  "if"         { Token _ If }
  "<"          { Token _ LeftAngledBracket }
  "("          { Token _ LeftBracket }
  "{"          { Token _ LeftCurlyBracket }
  "["          { Token _ LeftSquareBracket }
  "then"       { Token _ Then }
  lowercase    { Token _ (Lowercase $$) }
  "-"          { Token _ Hyphen }
  "OPTIONS_CCS"{ Token _ OptionsCCS }
  "|"          { Token _ Pipe }
  "+"          { Token _ Plus }
  ")"          { Token _ RightBracket }
  "}"          { Token _ RightCurlyBracket }
  "]"          { Token _ RightSquareBracket }
  ";"          { Token _ Semicolon }
  "#"          { Token _ Sharp }
  "/"          { Token _ Slash }

%left "\land"
%left "+" "-"
%left "|"
%left "." "\cdot" "/"
%left "\\" "["
%nonassoc ";" "then"

%%

Whole :: { ((Int, Int), [(Name, [(Var, Ty)], Proc)]) }
      : "{" "-" "#" "OPTIONS_CCS" "-" lowercase "-" lowercase integer "#" "-" "}" Declarations {% if $6 /= "nat" || $8 /= "max" then
                                                                                                    happyError
                                                                                                  else if $9 < 0 then
                                                                                                    alexError ("chosen int. roof is less than 0")
                                                                                                  else if $9 > natMax then
                                                                                                    alexError ("chosen int. roof exceeds " ++ show natMax)
                                                                                                  else
                                                                                                    return ((0, $9), $13) }

Action :: { Act }
       : lowercase                                     { In ($1, Nothing) }
       | "\overline{" lowercase "}"                    { Out ($2, Nothing) }
       | lowercase "(" lowercase ":" capitalized ")"   {% if $5 `notElem` ["Enum", "Nat"] then
                                                            happyError
                                                          else if $5 == "Enum" then
                                                            return (In ($1, Just ($3, Enum)))
                                                          else
                                                            return (In ($1, Just ($3, Nat)))  }
       | "\overline{" lowercase "}" "(" Expression ")" { Out ($2, Just $5) }
       | "\tau"                                        { Tau }
                     -- is for input?
Channels :: { [(Chan,   Bool         )] }
         : lowercase                               { [($1, True)] }
         | "\overline{" lowercase "}"              { [($2, False)] }
         | lowercase "," Channels                  { ($1, True) : $3 }
         | "\overline{" lowercase "}" "," Channels { ($2, False) : $5 }

                    -- name   signature   body
Declarations :: { [(   Name, [(Var, Ty)], Proc)] }
             : capitalized "=" Process ";"                             { [($1, [], $3)] }
             | capitalized "=" Process ";" Declarations                { ($1, [], $3) : $5 }
             | capitalized "(" Params ")" "=" Process ";"              { [($1, $3, $6)] }
             | capitalized "(" Params ")" "=" Process ";" Declarations { ($1, $3, $6) : $8 }

Expression :: { Expr }
           : capitalized                   { EEnum $1 }
           | lowercase                     { Var $1 }
           | integer                       { ENat $1 }
           | Expression "+" Expression     { Arithmetic $1 E.Plus $3 }
           | Expression "-" Expression     { Arithmetic $1 Minus $3 }
           | Expression "\cdot" Expression { Arithmetic $1 Mul $3 }
           | Expression "/" Expression     { Arithmetic $1 Div $3 }
           | "(" Expression ")"            { $2 }

Expressions :: { [Expr] }
            : Expression                 {[$1]}
            | Expression "," Expressions {$1 : $3}

Guard :: { BExpr }
      : Expression "=" Expression { Comparison $1 Eq $3 }
      | Expression "<" Expression { Comparison $1 Lt $3 }
      | "\neg{" Guard "}"         { Not $2 }
      | Guard "\land" Guard       { Binary $1 And $3 }
      | "(" Guard ")"             { $2 }

LabelChanges :: { [(String, String)] }
             : lowercase "/" lowercase                  { [($1, $3)] }
             | lowercase "/" lowercase "," LabelChanges { ($1, $3) : $5 }

Params :: { [(Var, Ty)] }
       : lowercase ":" capitalized            {% if $3 `notElem` ["Enum", "Nat"] then
                                                   happyError
                                                 else if $3 == "Enum" then
                                                   return [($1, Enum)]
                                                else
                                                   return [($1, Nat)] }
       | lowercase ":" capitalized "," Params {% if $3 `notElem` ["Enum", "Nat"] then
                                                   happyError
                                                 else if $3 == "Enum" then
                                                   return (($1, Enum) : $5)
                                                 else
                                                   return (($1, Nat)  : $5) }

Process :: { Proc }
        : Action "." Process              { ActPrefx $1 $3 }
        | Process "+" Process             { Choice [$1, $3] }
        | "if" Guard "then" Process       { Cond $2 $4 }
        | capitalized                     { Const $1 Nothing }
        | capitalized "(" Expressions ")" { Const $1 (Just $3) }
        | integer                         {% if $1 /= 0 then happyError else return Inaction }
        | Process "|" Process             { Par $1 $3 }
        | Process "\\" "{" Channels "}"   { Restriction $1 $4 }
        | Process "[" LabelChanges "]"    { Relabeling $1 $3 }
        | "(" Process ")"                 { $2 }

{
happyError :: Alex a
happyError = alexGetPosition >>= (\(AlexPn _ line column) ->
                                    alexError (unwords ["syntax error at line", show line ++ ",", "column", show column]))

lexer :: (Token -> Alex a) -> Alex a
lexer f = alexMonadScan >>= f
}
