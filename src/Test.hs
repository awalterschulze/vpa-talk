module Test (
    test
) where

test :: (Show a, Show b, Show c, Eq c) => String -> (a -> b -> c) -> a -> b -> c -> IO ()
test name f a b want =
  let got = f a b in
  if got == want
  then putStrLn $ "[PASS] " ++ name ++ " (" ++ show a ++ ") (" ++ show b ++ ") == " ++ show got
  else putStrLn $ "[FAIL] " ++ name ++ " (" ++ show a ++ ") (" ++ show b ++ ") /= " ++ show want ++ " , got: " ++ show got
