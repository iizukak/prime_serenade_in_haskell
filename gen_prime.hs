import Data.List

-- 篩関数。x の倍数をリスト l から取り除く
sieve:: Integer -> [Integer] -> [Integer]
sieve x [] = []
sieve x l  = l \\ [x, x * 2..(last l)]

-- エラトステネスの篩のループ部分
-- 引数は、被篩リスト、素数リスト
loop:: [Integer] -> [Integer] -> [Integer]
loop [] ans     = 2:ans
loop (x:xs) ans = loop (sieve x xs) (x:ans)

-- エラトステネスの篩メイン関数
primes:: Integer -> [Integer]
primes max = loop [3, 5..max] []


primes' = sieve' [2..]
sieve' (p:xs) = p : sieve' [x | x <- xs, x `mod` p /= 0]
