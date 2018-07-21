module IsZero where

import Text.ParserCombinators.ReadP

-- |
-- λ parser $$ "0"
-- [(Z,"")]

($$) = readP_to_S

-- |
-- λ parser $$$ "0"
-- Z

p $$$ s = fst . head $ readP_to_S (p >>= \x -> eof >> return x) s

-- | The language.
data L = T        -- True.
       | F        -- False.
       | I L L L  -- If.
       | Z        -- Zero.
       | S L      -- Successor.
       | P L      -- Predecessor.
       | E L      -- Is zero?
    deriving Show

-- | The parser.
--
-- λ parser $$ " if true then succ 0 else if iszero succ 0 then 0 else succ 0 "
-- [(I T (S Z) (I (E (S Z)) Z (S Z)),"")]
--
-- λ parser $$ "true"
-- [(T,"")]
-- λ parser $$ "false"
-- [(F,"")]
-- λ parser $$ "0"
-- [(Z,"")]
-- λ parser $$ "succ 0"
-- [(S Z,"")]
-- λ parser $$ "pred 0"
-- [(P Z,"")]
-- λ parser $$ "iszero 0"
-- [(E Z,"")]
-- λ parser $$ "if true then 0 else 0"
-- [(I T Z Z,"")]

parser :: ReadP L
parser = choice $ between skipSpaces skipSpaces <$> [ t, f, i, z, s, p, e ]
  where
    t = string "true" >> return T
    f = string "false" >> return F
    i = do
        string "if"
        iPred <- parser
        string "then"
        iThen <- parser
        string "else"
        iElse <- parser
        return (I iPred iThen iElse)
    z = string "0" >> return Z
    s = do
        string "succ"
        x <- parser
        return (S x)
    p = do
        string "pred"
        x <- parser
        return (P x)
    e = string "iszero" >> E <$> parser
