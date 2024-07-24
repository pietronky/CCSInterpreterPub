{
module Parsing.Lexer
  ( Alex(..),
    AlexPosn(..),
    Token(..),
    TokenClass(..),
    alexError,
    alexGetPosition,
    alexMonadScan,
    runAlex
  )
where
}

%encoding "utf8"
%wrapper "monad"

@integer = 0|\-?[1-9][0-9]*
@capitalized = [A-Z][_a-z0-9]*
@lowercase = [a-z][_a-z0-9]*

tokens :-
  \\cdot       { alexGetAction (const BackslashCdot) }
  \\land       { alexGetAction (const BackslashLand) }
  \\tau        { alexGetAction (const BackslashTau) }
  \\neg\{      { alexGetAction (const BackslashNegLeftCurlyBracket) }
  \\overline\{ { alexGetAction (const BackslashOverlineLeftCurlyBracket) }
  \\           { alexGetAction (const Backslash) }
  @capitalized { alexGetAction Capitalized }
  \:           { alexGetAction (const Colon) }
  \,           { alexGetAction (const Comma) }
  \.           { alexGetAction (const Dot) }
  =            { alexGetAction (const Equal) }
  if           { alexGetAction (const If) }
  @integer     { alexGetAction (Integer .read) }
  \(           { alexGetAction (const LeftBracket) }
  \{           { alexGetAction (const LeftCurlyBracket) }
  \[           { alexGetAction (const LeftSquareBracket) }
  then         { alexGetAction (const Then) }
  @lowercase   { alexGetAction Lowercase }
  \<           { alexGetAction (const LeftAngledBracket) }
  \-           { alexGetAction (const Hyphen) }
  OPTIONS_CCS  { alexGetAction (const OptionsCCS) }
  \|           { alexGetAction (const Pipe) }
  \+           { alexGetAction (const Plus) }
  \)           { alexGetAction (const RightBracket) }
  \}           { alexGetAction (const RightCurlyBracket) }
  \]           { alexGetAction (const RightSquareBracket) }
  \;           { alexGetAction (const Semicolon) }
  \#           { alexGetAction (const Sharp) }
  \/           { alexGetAction (const Slash) }
  $white+ ;

{
data Token = Token AlexPosn TokenClass

data TokenClass
  = Backslash
  | BackslashCdot
  | BackslashLand
  | BackslashNegLeftCurlyBracket
  | BackslashOverlineLeftCurlyBracket
  | BackslashTau
  | Capitalized String
  | Cdot
  | Colon
  | Comma
  | Dot
  | EOF
  | Equal
  | Hyphen
  | If
  | Integer Int
  | LeftAngledBracket
  | LeftBracket
  | LeftCurlyBracket
  | LeftSquareBracket
  | Lowercase String
  | OptionsCCS
  | Pipe
  | Plus
  | RightBracket
  | RightCurlyBracket
  | RightSquareBracket
  | Semicolon
  | Sharp
  | Slash
  | Then

alexEOF :: Alex Token
alexEOF = alexGetInput >>= (\(position, _, _, _) -> return (Token position EOF))

alexGetPosition :: Alex AlexPosn
alexGetPosition = Alex (\state@AlexState {alex_pos = position} -> Right (state, position))

alexGetAction :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
alexGetAction f (position, _, _, input) tokenLength = return (Token position (f (take tokenLength input)))
}
