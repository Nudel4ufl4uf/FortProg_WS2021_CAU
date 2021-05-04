-- Explicit import of the simplified prelude (contains +, -,...)
import SimplePrelude

-- Computes the square of a number.
square :: Int -> Int
square x = x * x

-- Computes the minimum of two numbers
mymin :: Int -> Int -> Int
mymin x y = if x <= y then x else y

-- Computes the factorial of a number:
fac :: Int -> Int
fac n = if n == 0 then 1 else n * fac (n - 1)
{-
fac 2
if 2 == 0 then 1 else 2 * fac (2 - 1)
if False  then 1 else 2 * fac (2 - 1)
2 * fac (2 - 1)
2 * fac 1
2 * if 1 == 0 then 1 else 1 * fac (1 - 1)
2 * if False  then 1 else 1 * fac (1 - 1)
2 * (1 * fac (1 - 1))
2 * (1 * fac 0)
2 * (1 * if 0 == 0 then 1 else 0 * fac (0 - 1))
2 * (1 * if True   then 1 else 0 * fac (0 - 1))
2 * (1 * 1)
2 * 1
2
-}

-- Computes the n. Fibonacci number:
fib1 :: Int -> Int
fib1 n = if n == 0
           then 0
           else if n == 1
                  then 1
                  else fib1 (n - 1) + fib1 (n - 2)

-- An accumulator function to compute the next Fibonacci number:
fib2iter :: Int -> Int -> Int -> Int
fib2iter fibn fibnp1 n = if n == 0
                           then fibn
                           else fib2iter fibnp1 (fibn + fibnp1) (n - 1)

-- Computes the n. Fibonacci number:
fib2 :: Int -> Int
fib2 n = fib2iter 0 1 n

-- with local definitions:

-- Computes the n. Fibonacci number:
fib3 :: Int -> Int
fib3 n = iter 0 1 n
 where
  -- An accumulator function to compute the next Fibonacci number:
  iter fibn fibnp1 n = if n == 0
                         then fibn
                         else iter fibnp1 (fibn + fibnp1) (n - 1)

-- Next function