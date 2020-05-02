-- TODO: add header

import Test.QuickCheck

import qualified TPTPParser as TPTP

prop_parseCorrect :: FOFormula -> Bool
prop_parseCorrect f = (f == parseString (show f))
