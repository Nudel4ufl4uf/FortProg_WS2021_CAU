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

--method to represent a given JSPN file
jsondoc:: JSON 
jsondoc j = JArray [JObject[("name", JString "meier"),
                            ("besuchte_kurse",JArray[JString "Logik",JString "Programmierung",JString "Compilerbau"]),
                            ("bachelor_note" , JNull null),
                            ("zugelassen",JBool true)],
                    JObject[("name", JString "schmidt"),
                            ("besuchte_kurse", JArray[JString "Programmierung",JString"Informationssysteme"]
                            ("bachelor_note", JInt 2.7),
                            ("zugelassen",JBool false)]]
