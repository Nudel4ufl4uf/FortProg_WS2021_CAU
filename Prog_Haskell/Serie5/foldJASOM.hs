import SimplePrelude

--data type JSON
data JSON = JNull
          | JBool Bool
          | JInt Int
          | JFloat Float
          | JString String
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving Show

--fold method for the JSON data
foldJSON∷ a -> (Bool -> a) -> (Int -> a) -> (Flaot -> a) -> (String -> a) -> ([a] -> a) -> (String -> a -> a) -> JSON -> a
foldJSON jnull bool int float string array object doc = case doc of
        JNull           -> jnull
        JBool   x       -> bool x
        JInt    x       -> int x
        JFloat  x       -> float x
        JString x       -> string x
        JArray  xs      -> array(map(foldJSON jnull bool int float string array object) xs)
        JObject xs      -> object(map ()xs)
