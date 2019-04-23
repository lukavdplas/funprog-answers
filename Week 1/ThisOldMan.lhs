> {-# LANGUAGE UnicodeSyntax #-}
> module ThisOldMan
> where
> import Unicode


> numberstring :: Integer → String
> numberstring x
>     | x ≤ 10 ∧ x ≥ 0    = let numberstrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]
>                           in numberstrings !! (fromIntegral x - 1)

> rhymestring :: Integer → String
> rhymestring x
>     | x == 7            = "up in heaven"
>     | x == 10           = "once again"
>     | x ≤ 10 ∧ x ≥ 0    = let rhymes = ["thumb", "shoe", "knee", "door", "hive", "sticks", "", "gate", "spine", ""]
>                           in "on my " ++ rhymes !! (fromIntegral x - 1)

> stanza :: Integer → String
> stanza x = "This old man, he played " ++ (numberstring x) ++ ",\n"
>            ++ "He played knick-knack " ++ (rhymestring x) ++ ",\n"
>            ++ "With a knick-knack paddywhack,\n"
>            ++ "Give the dog a bone,\n"
>            ++ "This old man came rolling home.\n"

> thisOldMan :: String
> thisOldMan = let stanzas    = map stanza [1..10]
>                  join s1 s2 = s1 ++ "\n" ++ s2
>              in foldr join [] stanzas
