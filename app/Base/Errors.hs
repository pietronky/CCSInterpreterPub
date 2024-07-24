module Base.Errors (panic) where

-- | Stops execution and displays an error message having "Panic: " as prefix
panic :: String -> t
panic msg = error ("Panic: " ++ msg)
