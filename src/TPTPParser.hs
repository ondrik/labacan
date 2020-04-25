-- TODO: missing header
module TPTPParser where

import Control.Applicative ((<*))
import Control.Exception (assert)

import qualified Data.List as DL

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P
import qualified Text.Parsec.Language as P
import Text.Parsec.Prim ((<?>),(<|>))
import qualified Text.Parsec.Prim as P
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Token as P



-- definition of logic
import qualified Logic

------------------------------- DATA STRUCTURES ------------------------------
-- representation of a TPTP file
newtype TPTPFile = TPTPFile [AnnotatedFormula]

instance Show TPTPFile where
  show (TPTPFile fs) = ((unlines . (map show)) fs)


-- formula with name, role, and annotations
data AnnotatedFormula
  -- |     name   role   formula
  = AF_FOF String String FOFormula

instance Show AnnotatedFormula where
  show (AF_FOF name role form) = "fof(" ++ name ++ ", " ++ role ++ ", " ++
    show form ++ ")."

-- variable
type Var = String

-- ######################## FIRST-ORDER FORMULA ###########################
data FOFormula
  = FOPred     String [FOTerm]
  | FONot      FOFormula
  | FOAnd      FOFormula FOFormula
  | FOOr       FOFormula FOFormula
  | FONand     FOFormula FOFormula
  | FONor      FOFormula FOFormula
  | FOImplLR   FOFormula FOFormula
  | FOImplRL   FOFormula FOFormula
  | FOEquiv    FOFormula FOFormula
  | FONonEquiv FOFormula FOFormula
  | FOForAll   [Var]     FOFormula
  | FOExists   [Var]     FOFormula

instance Show FOFormula where
  show (FOPred pred ls) =
    case pred of
      "="   -> showBinary pred
      "!="  -> showBinary pred
      _     -> pred ++ showArgs ls
    where showBinary op = assert (length ls == 2) $
            (show $ ls!!0) ++ " " ++ op ++ " " ++ (show $ ls!!1)
  show (FONot      form)    = "~ (" ++ show form ++ ")"
  show (FOAnd      lhs rhs) = "(" ++ show lhs ++ ") & (" ++ show rhs ++ ")"
  show (FOOr       lhs rhs) = "(" ++ show lhs ++ ") | (" ++ show rhs ++ ")"
  show (FONand     lhs rhs) = "(" ++ show lhs ++ ") ~& (" ++ show rhs ++ ")"
  show (FONor      lhs rhs) = "(" ++ show lhs ++ ") ~| (" ++ show rhs ++ ")"
  show (FOImplLR   lhs rhs) = "(" ++ show lhs ++ ") => (" ++ show rhs ++ ")"
  show (FOImplRL   lhs rhs) = "(" ++ show lhs ++ ") <= (" ++ show rhs ++ ")"
  show (FOEquiv    lhs rhs) = "(" ++ show lhs ++ ") <=> (" ++ show rhs ++ ")"
  show (FONonEquiv lhs rhs) = "(" ++ show lhs ++ ") <~> (" ++ show rhs ++ ")"
  show (FOForAll vars form) = showQuant "!" vars form
  show (FOExists vars form) = showQuant "?" vars form

-- shows a quantified formula
showQuant :: String      -- quantifier
          -> [String]    -- variables
          -> FOFormula   -- formula
          -> String
showQuant q vars form = q ++ " [" ++ (DL.intercalate ", " vars) ++ "] : " ++ show form

-- outputs a list of values separated by commas
commatize :: (Show a) => [a] -> String
commatize ls = DL.intercalate ", " (map show ls)

-- outputs parameters of function or predicate symbols
showArgs :: [FOTerm] -> String
showArgs ls = if ls == [] then "" else args
  where args = "(" ++ commatize ls ++ ")"


-- first-order term
data FOTerm
  = FOTerm String [FOTerm]
  deriving (Eq)

instance Show FOTerm where
  show (FOTerm fun ls) = fun ++ showArgs ls



----------------------------- TOP-LEVEL FUNCTINOS -----------------------------
parseFile :: FilePath
          -> IO TPTPFile
parseFile filename = do
  str <- readFile filename
  return $ parseString str


-- parser a string with TPTP file content
parseString :: String -> TPTPFile
parseString str = case (P.parse tptpFileParser "TPTP parser" str) of
  Left err   -> error $ show err
  Right res  -> res


test :: IO ()
test = do
  tptp_file <- parseFile "../PUZ001+1.p"
  -- tptp_file <- parseFile "test/test1.p"
  putStr $ show tptp_file

langDef = P.emptyDef{ P.commentStart = "/*"
                    , P.commentEnd = "*/"
                    , P.commentLine = "%"
                    , P.caseSensitive = True
                    , P.opStart =  P.oneOf "~&|<=>!?+-,:"
                    , P.opLetter = P.oneOf "~&|<=>!?+-,:"
                    , P.reservedOpNames =
                      [ "~"     -- NOT
                      , "&"     -- AND
                      , "|"     -- OR
                      , "~&"    -- NAND
                      , "~|"    -- NOR
                      , "=>"    -- IMPLIES LEFT2RIGHT
                      , "<="    -- IMPLIES RIGHT2LEFT
                      , "<=>"   -- EQUIV
                      , "<~>"   -- NONEQUIV
                      , "!"     -- UNIVERSAL QUANTIFIER
                      , "?"     -- EXISTENTIAL QUANTIFIER
                      , "="     -- EQUALS
                      , "!="    -- NEQUALS
                      , ":"     -- COLON IN QUANTIFICATION
                      ]
--               , nestedComments = False
--               , identStart = letter <|> char '_' <|> char '$' <|> char '\''
--               , identLetter = alphaNum <|> char '_' <|> char '$' <|> char '\''
--               , opStart = oneOf "~&:|<=>+-,\"\\^"
--               , opLetter = oneOf "~&:|<=>+-,\"\\^"
--               , reservedNames =
--                 [ "ex0"
--                 , "ex1"
--                 ]
--               }
                  }


-- generate parsers for tokens
P.TokenParser{ P.parens = m_parens
             , P.brackets = m_brackets
             , P.dot = m_dot
             , P.comma = m_comma
             , P.colon = m_colon
             , P.commaSep1 = m_commaSep1
             , P.reservedOp = m_reservedOp
             , P.lexeme = m_lexeme
             -- , reserved = m_reserved
             -- , commaSep = m_commaSep
             -- , semi = m_semi
             -- , semiSep1 = m_semiSep1
             -- , natural = m_natural
             -- , stringLiteral = m_stringLiteral
             , P.whiteSpace = m_whiteSpace } = P.makeTokenParser langDef

-- ############################## TOKENS ##############################
alpha_numeric :: P.Parser Char
alpha_numeric = P.lower
            <|> P.upper
            <|> P.digit
            <|> P.char '_'

-- word starting with a given parser
word_starting_with :: P.Parser Char -> P.Parser String
word_starting_with p = do { first <- p
                          ; rest <- m_lexeme $ P.many alpha_numeric
                          ; return $ first:rest
                          }
                   <?> "word_starting_with"

-- word starting with upper-case character
upper_word :: P.Parser String
upper_word = word_starting_with P.upper

-- word starting with lower-case character
lower_word :: P.Parser String
lower_word = word_starting_with P.lower

-- atomic word
atomic_word :: P.Parser String
atomic_word = lower_word
          -- <|> single_quoted

-- single quoted string
single_quoted :: P.Parser String
single_quoted = error "Unimplemented"
                -- do { char '\''
                --    ; 
                --    ; char '\''
                --    ; return 
                --    }

-- functor names
functor :: P.Parser String
functor = atomic_word


-- annotations
annotations :: P.Parser String
annotations = m_lexeme (P.many $ P.noneOf ")")

-- fragment
fragment :: P.Parser String
fragment = P.string "fof"
       <|> P.string "tpi"
       <|> P.string "thf"
       <|> P.string "tff"
       <|> P.string "tcf"
       <|> P.string "cnf"

-- variable
variable :: P.Parser String
variable = upper_word


-- ############################# FORMULAS #############################

-- parses formulae (needed to make my own since Parsec's buildExpressionParser
-- has problems with two associative operators with the same priority: '&' and
-- '|')
-- GRAMMAR:
-- <formula>     ->  <formulaTerm> "&" <andList> | <formulaTerm> "|" <orList> | <nonassoc> | <formulaTerm>
-- <formulaTerm> ->  "(" <formula> ")" | <atomicFormula> | "~" <formulaTerm> | <quantifiedFormula>
-- <quantified>  ->  ("?" | "!") "[" <variables> "]" ":" <formulaTerm>
-- <andList>     ->  <formulaTerm> | <formulaTerm> "&" <andList>
-- <orList>      ->  <formulaTerm> | <formulaTerm> "|" <orList>
-- <nonassoc>    ->  <formulaTerm> (operator) <formulaTerm>
formula :: P.Parser FOFormula
formula = operListTop "&" FOAnd
      <|> operListTop "|" FOOr
      <|> nonassoc
      <|> formulaTerm
      <?> "formula"

nonassoc :: P.Parser FOFormula
nonassoc = P.buildExpressionParser table formulaTerm <?> "nonassoc"
  where table = [ [ binary "~&"  FONand      P.AssocNone
                  , binary "~|"  FONor       P.AssocNone
                  , binary "=>"  FOImplLR    P.AssocNone
                  , binary "<="  FOImplRL    P.AssocNone
                  , binary "<=>" FOEquiv     P.AssocNone
                  , binary "<~>" FONonEquiv  P.AssocNone
                  ]
                ]
        binary name fun assoc = P.Infix (m_reservedOp name >> return fun) assoc

-- top nonterminal of an operator list
operListTop :: String
            -> (FOFormula -> FOFormula -> FOFormula)
            -> P.Parser FOFormula
operListTop op fun = (P.try $ do { hd <- formulaTerm
                                 ; m_reservedOp op
                                 ; tl <- operList op fun
                                 ; return $ fun hd tl
                                 })
                 <?> "operListTop"

-- generalizes andList and orList
operList :: String
         -> (FOFormula -> FOFormula -> FOFormula)
         -> P.Parser FOFormula
operList op fun = operListTop op fun
              <|> formulaTerm
              <?> "operList"

-- formula term
formulaTerm :: P.Parser FOFormula
formulaTerm = m_parens formula
          <|> do { m_reservedOp "~"
                 ; form <- formulaTerm
                 ; return $ FONot form
                 }
          <|> quantified
          <|> atomicFormula
          <?> "formula term"

-- quantified formula
quantified :: P.Parser FOFormula
quantified = do { quant <- P.oneOf "?!"
                ; m_whiteSpace
                ; vars <- (m_brackets $ m_commaSep1 variable)
                ; m_colon
                ; form <- formula
                ; return $ if quant == '?' then FOExists vars form else FOForAll vars form
                }
         <?> "quantified"

-- first-order plain term
fof_plain_term :: P.Parser (String, [FOTerm])
fof_plain_term = (P.try $ do { fnc <- functor
                             ; trms <- P.option [] (m_parens (m_commaSep1 term))
                             ; return (fnc, trms)
                             })
             <|> do { var <- variable
                    ; return (var, [])
                    }
             <?> "fof_plain_term"

-- atomic formula
atomicFormula :: P.Parser FOFormula
atomicFormula = (P.try $ do { lhs <- term
                            ; m_reservedOp "="
                            ; rhs <- term
                            ; return $ FOPred "=" [lhs, rhs]
                            })
            <|> (P.try $ do { lhs <- term
                            ; m_reservedOp "!="
                            ; rhs <- term
                            ; return $ FOPred "!=" [lhs, rhs]
                            })
            <|> do { (prd, trms) <- fof_plain_term
                   ; return $ FOPred prd trms
                   }
            <?> "atomic formula"

-- terms
term :: P.Parser FOTerm
term = do { (fnc, trms) <- fof_plain_term
          ; return $ FOTerm fnc trms
          }
   <?> "term"

-- parser an annotated formula
annotFormParser :: P.Parser AnnotatedFormula
annotFormParser = do { form_type <- fragment
                     ; P.char '('
                     ; name <- atomic_word    -- TODO: can also be integer
                     ; m_comma
                     ; role <- lower_word
                     ; m_comma
                     ; form <- formula
                     -- ; annotations
                     ; (P.optional (P.char ',' *> annotations ))
                     ; P.char ')'
                     ; m_dot
                     ; return $ if form_type == "fof" then AF_FOF name role form else error "Unsupported"
                     }

-- parses a TPTP file
tptpFileParser :: P.Parser TPTPFile
tptpFileParser = do { m_whiteSpace
                    -- ; formulae <- P.many (annotFormParser <* m_dot)
                    ; formulae <- P.many (annotFormParser)
                    ; return $ TPTPFile formulae
                    }
                 <* P.eof
