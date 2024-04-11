module Lesser(hydLesser) where

import GetInt

hydLesser :: [ String ] -> String
-- hydLesser [a, b] = case (getInt a, getInt b) of
--     (Just (x), Just (y))
-- --         | x < y = [(toEnum 1 :: Char),(toEnum 1 :: Char)]
--         | otherwise = [(toEnum 1 :: Char),(toEnum 0 :: Char)]
--     _ -> [(toEnum 0 :: Char)]
hydLesser _ = [(toEnum 0 :: Char)]
