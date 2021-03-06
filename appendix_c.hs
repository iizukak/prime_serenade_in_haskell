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
iotaReverse min max = reverse $ iota min max

{-
*Main> iota 12 10
[12,11,10]
-}

{- p.476-}
-- Haskell では可変長引数を使うのはリストを引数にすればよい？


{- p.481-}

succ':: [Int] -> [Int]
succ' l = [1] ++ l

pred':: [Int] -> [Int]
pred' l = drop 1 l

{-
e.g.

*Main> succ' [1, 1, 1]
[1,1,1,1]
*Main> pred' [1, 1, 1]
[1,1]
-}

-- 加算のリストでの表現
-- ガードを使ってみるテスト
plus:: [Int] -> [Int] -> [Int]
plus x y
    | y == []   = x
    | otherwise = succ' $ plus x (pred' y)

{-
e.g.
*Main> plus [1, 1, 1] [1, 1]
[1,1,1,1,1]
-}

{-
Haskell では
Int 型は C 言語の int
Integer 型は多倍長整数を表す
-}

{- p.483 -}

-- 乗算のリストでの表現
-- パターンマッチを使ってみるテスト
mult:: [Int] -> [Int] -> [Int]
mult _ []     = []
mult x (y:ys) = plus x $ mult x ys

{-
e.g.
*Main> mult [1,1] [1, 1, 1]
[1,1,1,1,1,1]
*Main> mult [1,1] [1, 1, 1, 1]
[1,1,1,1,1,1,1,1]
-}

-- 累乗のリストでの表現
pow:: [Int] -> [Int] -> [Int]
pow _ []     = [1]
pow x (y:ys) = mult x $ pow x ys

{-
e.g.
*Main> pow [1, 1] [1, 1, 1]
[1,1,1,1,1,1,1,1]
-}


-- 無限集合を使った自然数の定義
-- 型をうまく定義できなかった
-- kakko:: Integer -> [a]
-- kakko n = if n == 0 then [] else [kakko (n-1)]


-- 普通の整数の加算、乗算、べき乗を再帰で定義
plus', mult', pow':: Integer -> Integer -> Integer
plus' x 0 = x
plus' x y = 1 + (plus' x $ y - 1)

mult' x 0 = 0
mult' x y = x + (mult' x $ y - 1)

pow' x 0 = 1
pow' x y = x * (pow' x $ y - 1)

{-
*Main> plus' 2 3
5
*Main> mult' 2 3
6
*Main> pow' 2 3
8
-}

-- 代数的データ型で自然数を表してみる
data NaturalNumber = Zero | Push (NaturalNumber) deriving Show

{-
GHCi の :info コマンドが非常に有用であることに気づく
なぜいままで気付かなかったのか

例えば、以下のように Num 型クラスを :info してみると
その型クラスのインスタンスの型までわかる

*Main> :info Num
class Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  	-- Defined in `GHC.Num'
instance Num Integer -- Defined in `GHC.Num'
instance Num Int -- Defined in `GHC.Num'
instance Num Float -- Defined in `GHC.Float'
instance Num Double -- Defined in `GHC.Float'
-}


