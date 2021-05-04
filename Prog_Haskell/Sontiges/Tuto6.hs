--data type JSON
data JSON = JNull
          | JBool Bool
          | JInt Int
          | JFloat Float
          | JString String
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving (Show, Eq)

data IntTree a = Empty | Node (Tree a) a (Tree a)

instance Eq => Eq (Tree a) where
  -- (==) :: Tree a -> Tree a -> JBool
  Empty           == Empty           = True
  Node t1l n1 t1r == Node t2l n2 t2r = n1 == n2 && t1l == t2l && t1r == t2r
  _               == _               = False

{- class Ord a where
    (<)  :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    ...
    -}

--Präsenz Serie 6.1

class ToJSON a where
  toJSON :: a -> JSON

instance ToJSON Bool where
  --toJSON:: Bool -> JSON
  --toJSON False = JBool Falss
  --toJSON True  = JBool True
  toJSON b     = JBool b

instance ToJSON Int where
  -- toJSON :: Int -> jsons
  toJSON i = JInt i

instance ToJSON [a] where
  --toJSON:: [a] -> JSON
  toJSON xs     = JArray (map toJSON xs)
  -- toJSON []     = JArray []
  -- toJSON (x:xs)   = case JArray xs of
  --                        JArray jsons -> JArray (toJSON x : jsons)

instance (ToJSON a, ToJSON b) =>  ToJSon (a,b) where
  --toJSON:: (a,b) -> JSON
  toJSON (x,y) = JObject [("fst", to JSON x), ("snd", toJSON y)]
  -- toJSON (x,y) = JArray[toJSON x, toJSON y]

instance ToJSON => ToJSON (IntTree a) where
  -- toJSON:: Tree a -> JSON
  toJSON Empty          = JNull
  toJSON (Node tl n tr) = JArray[("leftTree", toJSON tl),("node", toJSON n), ("rightTree", toJSON tr)]
