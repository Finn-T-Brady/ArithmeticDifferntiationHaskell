primesOf::Int -> Int -> [Int]
primesOf x q | (x==q) = x:[]
             | (x `rem` q == 0) = q:primesOf (x `quot` q) q
             | otherwise = primesOf x (1+q)

-- (abcde)' = a'*bcde + a *(bcde')
-- (bcde)' = b'*cde + b*(cde')

-- (abcde)' = 1*bcde + a*(bcde')
-- (bcde)' = 1*cde + b*(cde')

-- f (x:xs) = product xs + x * f xs

arithD::[Int]->Int
arithD (x:xs:[]) = xs+x
arithD (x:xs) = product xs + x * arithD xs

main = do
  let p = primesOf 70 2
  let d = arithD p
  print(p)
  print(d)
