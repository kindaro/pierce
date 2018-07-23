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
-- λ parser $$ " ( 0 ) "
-- [(N Z,"")]
-- λ parser $$ "iszero 0"
-- [(E (N Z),"")]
-- λ parser $$ "if true then 0 else 0"
-- [(I T (N Z) (N Z),"")]
-- λ parser $$ "7"
-- [(N (S (S (S (S (S (S (S Z))))))),"")]
-- λ parser $$ "-3"
-- [(N (P (P (P Z))),"")]

parser :: ReadP L
parser = choice $ fmap whitespaced $ [ t, f, i, e, n ] ++ fmap parenthesized [ t, f, i, e ]
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
    e = string "iszero" >> E <$> parser
    n = N <$> parseN

-- |
-- λ parseN $$ "0"
-- [(Z,"")]
-- λ parseN $$ "(0)"
-- [(Z,"")]
-- λ parseN $$ " 0 "
-- [(Z,"")]
-- λ parseN $$ "succ 0"
-- [(S Z,"")]
-- λ parseN $$ "pred 0"
-- [(P Z,"")]
-- λ parseN $$ "succ pred 0"
-- [(S (P Z),"")]
-- λ parseN $$ "(succ (pred (0)))"
-- [(S (P Z),"")]

parseN :: ReadP N
parseN = choice $ fmap whitespaced $ [ s, p, n ] ++ fmap parenthesized [ s, p ]
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
-- Left 7
-- λ evaluator $ parser $$$ "-3"
-- Left (-3)
-- λ evaluator $ parser $$$ "if true then 1 else 0"
-- Left 1
-- λ evaluator $ parser $$$ "if false then 1 else 0"
-- Left 0
-- λ evaluator $ parser $$$ "if (iszero 0) then 1 else 0"
-- Left 1
-- λ evaluator $ parser $$$ "if (iszero 1) then 1 else 0"
-- Left 0
-- λ evaluator $ parser $$$ "iszero (pred (succ 0))"
-- Right True

evaluator :: L -> Either Integer Bool
evaluator x@ T = Right . evaluatorB $ x
evaluator x@ F = Right . evaluatorB $ x
evaluator x@ (E _) = Right . evaluatorB $ x
evaluator x@ (I p t f) = case evaluator p of
    Right True -> evaluator t
    Right False -> evaluator f
    Left _ -> error "Condition is not boolean."
evaluator x@ (N _) = Left . evaluatorI $ x

evaluatorI :: L -> Integer
evaluatorI (N x) = fromN x

evaluatorB :: L -> Bool
evaluatorB T = True
evaluatorB F = False
evaluatorB (E x) | evaluator x == Left 0 = True
                 | otherwise             = False

-- | The language as presented originally, with numerals sewn in.
data L' = T'           -- True.
        | F'           -- False.
        | I' L' L' L'  -- If.
        | Z'           -- Zero.
        | S' L'        -- Successor.
        | P' L'        -- Predecessor.
        | E' L'        -- Is zero?
    deriving (Show, Eq, Ord)

-- | Generate L'.
--
-- Exactly in accordance with Benjamin Pierce's answer:
-- λ length $ s 3
-- 59439

s 0 = [ ]
s n = [Z', T', F']
   ++ [S' x | x <- s']
   ++ [P' x | x <- s']
   ++ [E' x | x <- s']
   ++ [I' x y z | x <- s', y <- s', z <- s']
  where s' = s (pred n)

-- | Compute the size of L'.
--
-- In accordance with Benjamin Pierce's answer, again:
-- λ c 3
-- 59439

c 0 = 0
c n = 3 + 3 * c' + c'^3
  where c' = c (pred n)
