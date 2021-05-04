-- Explicit import of the simplified prelude (contains +, -,...)
import SimplePrelude

-- Computes the n. Fibonacci number with a local `iter` function:
fib3 :: Int -> Int
fib3 n = iter 0 1 n
 where
  -- An accumulator function to compute the next Fibonacci number:
  iter fibn fibnp1 n = if n == 0
                         then fibn
                         else iter fibnp1 (fibn + fibnp1) (n - 1)

-- Computes the n. Fibonacci number with a local `let`:
fib4 :: Int -> Int
fib4 n =
 let iter fibn fibnp1 n = if n == 0
                            then fibn
                            else iter fibnp1 (fibn + fibnp1) (n - 1)
 in iter 0 1 n

-- Structure a complex expression with `let`:
f1 x y = y * (1 -y) + (1 + x * y) * (1 - y) + (x * y)

f2 x y = let a = 1 - y
             b = x * y
         in y * a + (1 + b) * a + b

f3 x y = let a = 1 - y
         in   let b = x * y
              in y * a + (1 + b) * a + b

-- Is a number a prime number?
isPrim :: Int -> Bool
isPrim n = n /= 1 && checkDiv (n - 1)
 where
  -- Is a number (second argument) dividable by a number equal or smaller
  -- than the first argument?
  checkDiv m = m == 1 || (mod n m /= 0 && checkDiv (m - 1))
