module Test (
    test
) where

toStr' :: (Show a) => a -> Int -> String
toStr' x trunc =
  let xs = show x in
  let n = min (length xs) trunc in
  let xs' = take n xs in
  if length xs > trunc
  then xs' ++ "..."
  else xs'

toStr x = toStr' x 40

test :: (Show a, Show b, Show c, Eq c) => String -> (a -> b -> c) -> a -> b -> c -> IO ()
test name f a b want =
  let got = f a b in
  if got == want
  then putStrLn $ "[PASS] " ++ name ++ " (" ++ toStr a ++ ") (" ++ toStr b ++ ") == " ++ toStr got
  else putStrLn $ "[FAIL] " ++ name ++ " (" ++ toStr a ++ ") (" ++ toStr b ++ ") /= " ++ toStr want ++ " , got: " ++ toStr got
