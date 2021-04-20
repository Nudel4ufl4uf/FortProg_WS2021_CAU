--Wir rekapitulieren zunächst noch einmal die Datenstruktur.
data Rose a = Rose a [Rose a]

--Die Eq-Instanz vergleicht zunächst die Konstruktoren und dann die Argumente.
--Man beachte, dass die Einträge des Rose Trees dafür ebenfalls vergleichbar sein müssen.
--Dies wird durch das Typconstraint Eq a ausgedrückt.
instance Eq a => Eq (Rose a) where
  Rose x ts == Rose y us = x == y && ts == us

--Die Ord-Instanz vergleicht ebenfalls zuerst die Konstruktoren und dann die Argumente:
instance Ord a => Ord (Rose a) where
  compare (Rose x ts) (Rose y us) = case compare x y of
    EQ -> compare ts us
    cp -> cp

--Die Typklasse Pretty definieren wie folgt.
class Pretty a where
  pretty :: a -> String

--Für die Pretty-Instanz benötigen wir noch eine Hilfsfunktion, bei der wir die Einrückungstiefe zählen.
instance Pretty a => Pretty (Rose a) where
  pretty t = prettyTree 0 t
    where
    prettyTree n (Rose x ts) = intercalate "\n"
                             $ prettyIndented n x : map (prettyTree (n + 1)) ts
    prettyIndented 0 x = pretty x
    prettyIndented n x = concat (replicate (n - 1) "|   ") ++ "+-- " ++ pretty x

intercalate :: [a] -> [[a]] -> [a]
intercalate _   []     = []
intercalate _   [x]    = x
intercalate sep (x:xs) = concat [x, sep, intercalate sep xs]

--Damit wir unsere Implementierung testen können, benötigen wir noch Pretty-Instanzen für die Typen möglicher Baumeinträge.
--Diese könnten zum Beispiel wie folgt aussehen.
instance Pretty Int where
  pretty n = show n

instance Pretty Bool where
  pretty b = show b
