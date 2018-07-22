module IsZero where

import Text.ParserCombinators.ReadP

-- $setup
-- λ import Test.QuickCheck
-- λ import Test.Invariant
-- λ import GHC.Generics
-- λ import Generic.Random
-- λ :set -XStandaloneDeriving
-- λ :set -XDeriveGeneric
-- λ :set -XFlexibleInstances
-- λ :set -XUndecidableInstances
-- λ deriving instance Generic L
-- λ deriving instance Generic N
-- λ instance Arbitrary N where arbitrary = genericArbitraryU
-- λ instance Arbitrary L where arbitrary = genericArbitraryU

-- |
-- λ string "a" $$ "ab"
-- [("a","b")]

($$) = readP_to_S

-- |
-- λ string "ab" $$$ "ab"
-- "ab"

p $$$ s = fst . head $ readP_to_S (p >>= \x -> eof >> return x) s

-- |
-- λ whitespaced (string "meow") $$ " meow "
-- [("meow","")]
whitespaced :: ReadP a -> ReadP a
whitespaced = between skipSpaces skipSpaces

-- |
-- λ parenthesized (string "meow") $$ "(meow)"
-- [("meow","")]
parenthesized :: ReadP a -> ReadP a
parenthesized = between (string "(") (string ")")

-- | The language.
data L = T        -- True.
       | F        -- False.
       | I L L L  -- If.
       | N N      -- Numeral.
       | E L      -- Is zero?
    deriving (Show, Eq)

-- | Numerals.
data N = Z        -- Zero.
       | S N      -- Successor.
       | P N      -- Predecessor.
    deriving (Show, Eq)

-- | The parser.
--
-- λ parser $$ " if true then succ 0 else if iszero succ 0 then 0 else succ 0 "
-- [(I T (N (S Z)) (I (E (N (S Z))) (N Z) (N (S Z))),"")]
--
-- λ parser $$ " if true then (succ 0) else (if (iszero (succ 0)) then 0 else (succ 0)) "
-- [(I T (N (S Z)) (I (E (N (S Z))) (N Z) (N (S Z))),"")]
--
-- λ parser $$ "if true then 1 else if iszero 1 then 0 else 1"
-- [(I T (N (S Z)) (I (E (N (S Z))) (N Z) (N (S Z))),"")]
--
-- λ parser $$ "true"
-- [(T,"")]
-- λ parser $$ "false"
-- [(F,"")]
-- λ parser $$ "0"
-- [(N Z,"")]
-- λ parser $$ "succ 0"
-- [(N (S Z),"")]
-- λ parser $$ "pred 0"
-- [(N (P Z),"")]
-- λ parser $$ "succ pred 0"
-- [(N (S (P Z)),"")]
-- λ parser $$ "iszero 0"
-- [(E (N Z),"")]
-- λ parser $$ "if true then 0 else 0"
-- [(I T (N Z) (N Z),"")]
-- λ parser $$ "7"
-- [(N (S (S (S (S (S (S (S Z))))))),"")]
-- λ parser $$ "-3"
-- [(N (P (P (P Z))),"")]

parser :: ReadP L
parser = choice $ fmap whitespaced
                $                         parsers
                    ++ fmap parenthesized parsers
  where
    parsers = [ t, f, i, e, n ]

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
    e = string "iszero" >> E <$> parser
    n = N <$> parseN

parseN :: ReadP N
parseN = choice [ s, p, n ]
  where
    s = do
        string "succ"
        x <- parseN
        return (S x)
    p = do
        string "pred"
        x <- parseN
        return (P x)
    n = toN <$> readS_to_P (reads :: ReadS Integer)

-- |
-- λ toN 7
-- S (S (S (S (S (S (S Z))))))
-- λ toN (-3)
-- P (P (P Z))

toN :: Integer -> N
toN 0 = Z
toN x | x > 0 = S (toN $ pred x)
            | x < 0 = P (toN $ succ x)

-- |
-- λ fromN (S (S (S (S (S (S (S Z)))))))
-- 7
-- λ fromN (P (P (P Z)))
-- -3
--
-- α fromN `inverts` toN
--
-- Notice that to every Integer correspond many N, since `S . P = id`.

fromN :: N -> Integer
fromN Z = 0
fromN (S x) = succ (fromN x)
fromN (P x) = pred (fromN x)

-- |
-- λ simplify $ S (P (S Z))
-- S Z
--
-- α (fromN . simplify) <=> fromN
--
-- α \x -> (toN `inverts` fromN) x == (simplify <=> id) x

simplify :: N -> N
simplify = toN . fromN

-- |
-- λ evaluator $ parser $$$ "7"
-- 7
-- λ evaluator $ parser $$$ "-3"
-- -3

evaluator :: L -> Integer
evaluator (N x) = fromN x

-- TODO:
-- [*] Allow optional parentheses.
-- [*] Allow positional arabic numerals.
-- [ ] Write an evaluator.
