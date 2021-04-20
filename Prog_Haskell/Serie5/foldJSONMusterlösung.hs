import           SimplePrelude
foldJSON :: a                    -- ^ jnull
         -> (Bool -> a)          -- ^ jbool
         -> (Int -> a)           -- ^ jint
         -> (Float -> a)         -- ^ jfloat
         -> (String -> a)        -- ^ jstring
         -> ([a] -> a)           -- ^ jarray
         -> ([(String, a)] -> a) -- ^ jobject
         -> JSON                 -- ^ JSON document to be folded
         -> a                    -- ^ result of folding a JSON value
foldJSON jnull jbool jint jfloat jstring jarray jobject doc = case doc of
           JNull     -> jnull
           JBool   b -> jbool b
           JInt    i -> jint i
           JFloat  f -> jfloat f
           JString s -> jstring s
           JArray  a -> jarray (map (foldJSON jnull jbool jint jfloat jstring jarray jobject) a)
           JObject o -> jobject (map foldKV o)
             where
             foldKV (k, v) = (k, foldJSON jnull jbool jint jfloat jstring jarray jobject v)

prettyJSON :: JSON -> String
prettyJSON doc = foldJSON jnull jbool jint jfloat jstring jarray jobject doc
               where
               jnull         = "null"
               jbool   False = "false"
               jbool   True  = "true"
               jint    i     = showInt i
               jfloat  f     = showFloat f
               jstring s     = '\"' : s ++ "\""
               jarray  a     = '[' : commaSep a ++ "]"
               jobject o     = '{' : commaSep (map (\(k, v) -> '\"' : k ++ "\": " ++ v) o) ++ "}"

               commaSep [] = ""
               commaSep s  = foldr1 (\s1 s2 -> s1 ++ ", " ++ s2) s
