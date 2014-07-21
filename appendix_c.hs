{- p.469 -}

non:: a -> String
non x = "non"

-- "id" conflict with Main.id
id':: a -> a
id' x = x

add1:: Integer -> Integer
add1 n = n + 1

sub1:: Integer -> Integer
sub1 n = n - 1

{- p.470 -}

{-
除算
(x quot y)*y + (x rem y) == x`
ただし、quot は 0 に向かって丸める

(x div y)*y + (x mod y) == x
ただし、div は負の無限大に向かって丸める

累乗
> 2 ** 3
> 8.0
> :type exp
> exp :: Floating a => a -> a
-}

{- p.471 -}

makeEven:: Integer -> Integer
makeEven n = 2 * n

makeOdd:: Integer -> Integer
makeOdd n = (2 * n) + 1

{-
odd は基数かどうか判定する関数
Prelude> :type odd
odd :: Integral a => a -> Bool
Prelude> odd 2
False
-}

parityOf::Integer -> Integer
parityOf n
    | odd n     = (-1)
    | otherwise = 1


{- p.471 -}

{-
Haskell のまるめ関係の関数

truncate    : 0 に向かって丸める
round       : 最も近い整数に丸める
ceiling     : 切り上げ
floor       : 切り下げ
-}

{- p.475-}

{-
Prelude> reverse [-5..5]
[5,4,3,2,1,0,-1,-2,-3,-4,-5]
-}

{- p.476-}

-- [min..max] の自作版
iota:: Integer -> Integer -> [Integer]
iota min max = iotaLoop min max []
    where
        iotaLoop:: Integer -> Integer -> [Integer] -> [Integer]
        iotaLoop min i l
            | i < min   = []
            | otherwise = (iotaLoop min (i - 1) l) ++ [i]

-- [max, (max - 1)..min] の自作版
iotaReverse:: Integer -> Integer -> [Integer]
iotaReverse min max = reverse iota min max

{-
*Main> iota 12 10
[12,11,10]
-}

{- p.476-}
-- Haskell では可変長引数を使うのはリストを引数にすればよい？


