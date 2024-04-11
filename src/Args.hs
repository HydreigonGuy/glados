module Args (handleArgs) where

handleArgs :: [ String ] -> (String, [String])
handleArgs ("-o":x:xs) = case handleArgs xs of
    (_, ys) -> (x, ys)
handleArgs (x:xs) = case handleArgs xs of
    (y, ys) -> (y, x:ys)
handleArgs _ = ("output.hyd", [])
