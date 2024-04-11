module Greater(hydGreater) where

import GetInt

hydGreater :: [ String ] -> String
-- hydGreater [a, b] = case (getInt a, getInt b) of
--     (Just (x), Just (y))
-- --         | x > y = [(toEnum 1 :: Char),(toEnum 1 :: Char)]
--         | otherwise = [(toEnum 1 :: Char),(toEnum 0 :: Char)]
--     _ -> [(toEnum 0 :: Char)]
hydGreater _ = [(toEnum 0 :: Char)]
