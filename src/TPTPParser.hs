-- TODO: missing header
module TPTPParser where

import Control.Applicative ((<*))

-- import Data.List
--
import qualified Text.Parsec as P
-- import Text.Parsec.Char
import Text.Parsec.Expr as P
import Text.Parsec.Language as P
import Text.Parsec.Prim as P
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Token as P



-- definition of logic
import qualified Logic

------------------------------- DATA STRUCTURES ------------------------------
-- representation of a TPTP file
newtype TPTPFile = TPTPFile [AnnotatedFormula]

-- formula with name, role, and annotations
data AnnotatedFormula
  -- |     name   role   formula
  = AF_FOF String String FOFormula
  deriving (Show)

-- variable
type Var = String

-- first-order formula
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
  deriving (Show)

-- first-order term
data FOTerm
  = FOTerm String [FOTerm]
  deriving (Show)

instance Show TPTPFile where
  show (TPTPFile fs) = ((unlines . (map (\x -> (show x) ++ ";"))) fs)


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
--
--
-- -- terms in MONA expressions
-- data MonaTerm
--   = MonaTermVar String
--   | MonaTermConst Integer
--   | MonaTermPlus MonaTerm MonaTerm
--   | MonaTermMinus MonaTerm MonaTerm
--
--
-- -- parses terms
-- termParser :: Parser MonaTerm
-- termParser = buildExpressionParser termOpTable termP <?> "term"
--
-- -- term operator table
-- termOpTable = [ [ Infix (m_reservedOp "+" >> return MonaTermPlus) AssocLeft
--                 , Infix (m_reservedOp "-" >> return MonaTermMinus) AssocLeft
--                 ]
--               ]
--
-- termP :: Parser MonaTerm
-- termP = m_parens termParser
--     <|> fmap MonaTermVar m_identifier
--     <|> fmap MonaTermConst m_natural
--
--
-- -- formulae in MONA
-- data MonaFormula
--   = MonaFormulaAtomic String           -- TODO: needs to be refined
--   | MonaFormulaVar String
--   | MonaFormulaNeg MonaFormula
--   | MonaFormulaDisj MonaFormula MonaFormula
--   | MonaFormulaConj MonaFormula MonaFormula
--   | MonaFormulaImpl MonaFormula MonaFormula
--   | MonaFormulaEquiv MonaFormula MonaFormula
--   | MonaFormulaEx0 [String] MonaFormula
--   | MonaFormulaEx1 [(String, Maybe MonaFormula)] MonaFormula
--   | MonaFormulaEx2 [(String, Maybe MonaFormula)] MonaFormula
--   | MonaFormulaAll0 [String] MonaFormula
--   | MonaFormulaAll1 [(String, Maybe MonaFormula)] MonaFormula
--   | MonaFormulaAll2 [(String, Maybe MonaFormula)] MonaFormula
--   | MonaFormulaPredCall String [MonaTerm]
--
--
-- -- parses a binary atom
-- binAtomParser :: String -> Parser MonaFormula
-- binAtomParser op = do { lhs <- termParser
--                       ; m_reservedOp op
--                       ; rhs <- termParser
--                       ; return (MonaFormulaAtomic ((show lhs) ++ " " ++ op ++ " " ++ (show rhs)))
--                       }
--
-- -- parses atomic formulae
-- atomicFormulaParser :: Parser MonaFormula
-- atomicFormulaParser = try (binAtomParser "=")
--                   <|> try (binAtomParser "~=")
--                   <|> try (binAtomParser "<")
--                   <|> try (binAtomParser "<=")
--                   <|> try (binAtomParser ">")
--                   <|> try (binAtomParser ">=")
--                   <|> try (binAtomParser "in")
--                   <|> try (binAtomParser "notin")
--                   <|> try (binAtomParser "sub")
--                   <?> "atomic formula"
--
--
-- -- parses a "varwhere" string
-- varWhereParser :: Parser (String, Maybe MonaFormula)
-- varWhereParser = do { varname <- m_identifier
--                     ; phi <- optionMaybe (m_reserved "where" >> formulaParser)
--                     ; return (varname, phi)
--                     }
--
-- -- parses formulae
-- formulaParser :: Parser MonaFormula
-- formulaParser = buildExpressionParser formulaOpTable formulaTerm <?> "formula"
--
-- -- formula operator table
-- formulaOpTable = [ [ Prefix (m_reservedOp "~"   >> return MonaFormulaNeg) ]
--                  , [ Infix  (m_reservedOp "&"   >> return MonaFormulaConj) AssocLeft ]
--                  , [ Infix  (m_reservedOp "|"   >> return MonaFormulaDisj) AssocLeft ]
--                  , [ Infix  (m_reservedOp "=>"  >> return MonaFormulaImpl) AssocRight ]
--                  , [ Infix  (m_reservedOp "<=>" >> return MonaFormulaEquiv) AssocRight ]
--                  ]
--
-- formulaTerm :: Parser MonaFormula
-- formulaTerm = m_parens formulaParser
--           <|> atomicFormulaParser
--           <|> (m_reserved "true" >> return (MonaFormulaAtomic "true"))
--           <|> (m_reserved "false" >> return (MonaFormulaAtomic "false"))
--           <|> do { m_reserved "ex0"
--                  ; (varWhereList, phi) <- parseQuantSuffix
--                  ; let varList = map fst varWhereList
--                  ; return (MonaFormulaEx0 varList phi)
--                  }
--           <|> do { m_reserved "ex1"
--                  ; (varWhereList, phi) <- parseQuantSuffix
--                  ; return (MonaFormulaEx1 varWhereList phi)
--                  }
--           <|> do { m_reserved "ex2"
--                  ; (varWhereList, phi) <- parseQuantSuffix
--                  ; return (MonaFormulaEx2 varWhereList phi)
--                  }
--           <|> do { m_reserved "all0"
--                  ; (varWhereList, phi) <- parseQuantSuffix
--                  ; let varList = map fst varWhereList
--                  ; return (MonaFormulaAll0 varList phi)
--                  }
--           <|> do { m_reserved "all1"
--                  ; (varWhereList, phi) <- parseQuantSuffix
--                  ; return (MonaFormulaAll1 varWhereList phi)
--                  }
--           <|> do { m_reserved "all2"
--                  ; (varWhereList, phi) <- parseQuantSuffix
--                  ; return (MonaFormulaAll2 varWhereList phi)
--                  }
--           <|> try (do { predname <- m_identifier
--                       ; args <- m_parens $ m_commaSep m_identifier
--                       ; return (MonaFormulaPredCall predname $ map MonaTermVar args)
--                       })
--           <|> fmap MonaFormulaVar m_identifier
--   where
--     parseQuantSuffix :: Parser ([(String, Maybe MonaFormula)], MonaFormula)
--     parseQuantSuffix = do { varWhereList <- m_commaSep1 varWhereParser
--                           ; m_reservedOp ":"
--                           ; phi <- formulaParser
--                           ; return (varWhereList, phi)
--                           }
--
-- -- intercalate with commas
-- commatize :: [String] -> String
-- commatize list = intercalate ", " list
--
-- -- MONA file declarations
-- data MonaDeclaration
--   = MonaDeclFormula MonaFormula
--   | MonaDeclVar0 [String]
--   | MonaDeclVar1 [(String, Maybe MonaFormula)]
--   | MonaDeclVar2 [(String, Maybe MonaFormula)]
--   | MonaDeclAllpos String
--   | MonaDeclDefWhere1 String MonaFormula
--   | MonaDeclDefWhere2 String MonaFormula
--   | MonaDeclMacro String [MonaMacroParam] MonaFormula
--   | MonaDeclExport String MonaFormula
--
-- instance Show MonaDeclaration where
--   show (MonaDeclFormula phi) = show phi
--   show (MonaDeclVar0 varList) = "var0 " ++ (commatize varList)
--   show (MonaDeclVar1 varWhereList) = "var1 " ++ showVarWhereClause varWhereList
--   show (MonaDeclVar2 varWhereList) = "var2 " ++ showVarWhereClause varWhereList
--   show (MonaDeclAllpos var) = "allpos " ++ var
--   show (MonaDeclDefWhere1 var phi) = "defaultwhere1(" ++ var ++ ") = " ++ (show phi)
--   show (MonaDeclDefWhere2 var phi) = "defaultwhere2(" ++ var ++ ") = " ++ (show phi)
--   show (MonaDeclMacro name params phi) = "macro " ++ name ++ "(" ++ (commatize $ map show params) ++ ") = " ++ (show phi)
--   show (MonaDeclExport name phi) = "export(\"" ++ name ++ "\", " ++ (show phi) ++ ")"
--
--
-- showVarWhereClause :: [(String, Maybe MonaFormula)] -> String
-- showVarWhereClause = commatize . (map showVarWhere)
--   where
--     showVarWhere (var, whereCl) = var ++
--       (case whereCl of
--          Nothing  -> ""
--          Just phi -> " where " ++ (show phi)
--       )
--
--
-- -- parses a MONA declaration
-- declarationParser :: Parser MonaDeclaration
-- declarationParser = do { m_reserved "var0"
--                        ; varList <- m_commaSep1 m_identifier
--                        ; return (MonaDeclVar0 varList)
--                        }
--                 <|> do { m_reserved "macro"
--                        ; name <- m_identifier
--                        ; args <- optionMaybe (m_parens $ m_commaSep paramParser)
--                        ; let sanArgs = (case args of 
--                                           Nothing -> []
--                                           Just ls -> ls
--                                        )
--                        ; m_reservedOp "="
--                        ; phi <- formulaParser
--                        ; return (MonaDeclMacro name sanArgs phi)
--                        }
--                 <|> do { m_reserved "export"
--                        ; (filename, phi) <- m_parens
--                            (do { fn <- m_stringLiteral
--                                ; m_comma
--                                ; phi' <- formulaParser
--                                ; return (fn, phi')
--                                }
--                            )
--                        ; return (MonaDeclExport filename phi)
--                        }
--                 <|> fmap MonaDeclFormula formulaParser
--                 <?> "declaration"
--
--
-- -- macro parameters
-- data MonaMacroParam
--   = MonaMacroParamVar0 [String]
--   | MonaMacroParamVar1 [(String, Maybe MonaFormula)]
--   | MonaMacroParamVar2 [(String, Maybe MonaFormula)]
--   | MonaMacroParamUniv String
--
-- -- parses macro/pred parameters
-- paramParser :: Parser MonaMacroParam
-- paramParser = do { m_reserved "var0"
--                  ; varWhereList <- parseParamNames
--                  ; return (MonaMacroParamVar0 $ map fst varWhereList)
--                  }
--           <|> do { m_reserved "var1"
--                  ; varWhereList <- parseParamNames
--                  ; return (MonaMacroParamVar1 varWhereList)
--                  }
--           <|> do { m_reserved "var2"
--                  ; varWhereList <- parseParamNames
--                  ; return (MonaMacroParamVar2 varWhereList)
--                  }
--           <?> "parameter"
--   where
--     parseParamNames :: Parser [(String, Maybe MonaFormula)]
--     parseParamNames = sepBy1 varWhereParser (try parseComma)
--     parseComma :: Parser ()
--     parseComma = do { m_comma
--                     ; notFollowedBy (m_reserved "var0" <|> m_reserved "var1" <|> m_reserved "var2")
--                     }
--
--
-- -- parses a MONA file header
-- headerParser :: Parser String
-- headerParser = (m_reserved "ws1s" >> return "ws1s")
--            <|> (m_reserved "ws2s" >> return "ws2s")
--            <|> (m_reserved "m2l-str" >> return "m2l-str")
--            <|> (m_reserved "m2l-tree" >> return "m2l-tree")
--


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
  where table = [ [ binary "~&"  FONand     AssocNone
                  , binary "~|"  FONor      AssocNone
                  , binary "=>"  FOImplLR   AssocNone
                  , binary "<="  FOImplRL   AssocNone
                  , binary "<=>" FOEquiv    AssocNone
                  , binary "<~>" FONonEquiv AssocNone
                  ]
                ]
        binary name fun assoc = Infix  (m_reservedOp name >> return fun) assoc

-- top nonterminal of an operator list
operListTop :: String
            -> (FOFormula -> FOFormula -> FOFormula)
            -> P.Parser FOFormula
operListTop op fun = (try $ do { hd <- formulaTerm
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
fof_plain_term = (try $ do { fnc <- functor
                           ; trms <- P.option [] (m_parens (m_commaSep1 term))
                           ; return (fnc, trms)
                           })
             <|> do { var <- variable
                    ; return (var, [])
                    }
             <?> "fof_plain_term"

-- atomic formula
atomicFormula :: P.Parser FOFormula
atomicFormula = (try $ do { lhs <- term
                          ; m_reservedOp "="
                          ; rhs <- term
                          ; return $ FOPred "=" [lhs, rhs]
                          })
            <|> (try $ do { lhs <- term
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
