formatList s e sep xs = s ++ (intercalate sep (map show xs)) ++ e

-- adds the start (s) and end (e) to an array (xs) with a separator (sep)