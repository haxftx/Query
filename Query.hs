module Query where

import UserInfo
import Rating
import Movie
import Data.List

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

read_table :: ColSeparator -> LnSeparator -> String -> Table

--citeste table si creeaza o tabela
read_table colS lnS str = Table (head $create_entry colS lnS [] [] [] $table_schema lnS [] str)
                                (create_entry colS lnS [] [] [] $del_table_schema lnS str)

--sterge prima linie din string-l tabelei
del_table_schema :: LnSeparator -> String -> String
del_table_schema lnS (x:xs) = if lnS == x then xs else del_table_schema lnS xs

--intoarce prima linie din string-l tabelei
table_schema :: LnSeparator -> Field -> String -> Field
table_schema lnS res (x:xs) = if lnS == x then (res ++ (x:[]))
                                else table_schema lnS (res ++ (x: [])) xs

--creaza din string tabela dupa delimtoare
create_entry :: ColSeparator -> LnSeparator -> Field -> Entry -> [Entry] -> String -> [Entry]
create_entry _ _ c l acc [] = reverse acc
create_entry colS lnS c l acc (x:xs)
        | x == colS = create_entry colS lnS [] (flip (:) l (reverse c)) acc xs
        | x == lnS = create_entry colS lnS [] [] (flip (:) acc $reverse (flip (:) l $reverse c)) xs
        | otherwise = create_entry colS lnS (flip (:) c x) l acc xs

--citesc tabelele
user_info = read_table '|' '\n' user_info_str
rating = read_table ' ' '\n' rating_str
movie = read_table '|' '\n' movie_str


instance Show Table where
    show (Table header entries) = show_table (Table header entries)

--intoarce o lista cu dimensiunea stringurilor din lista
list_size :: [String] -> [Int]
list_size = map length

--intoarce o lista cu maximul de pe fiecare coloana din tabela
calculate_max_len_col :: Table -> [Int]
calculate_max_len_col (Table t e) = foldr (zipWith (max.length)) (list_size t) e


--afiseaza o linie "----"
show_ch :: Int -> String -> String
show_ch 0 acc = acc ++ "\n"
show_ch n acc = show_ch (n - 1) ("-" ++ acc)

--intoarce numarul de coloane a tabelei
nr_col :: Table -> Int
nr_col (Table t e) = length t

--creeaza o linei "---"
show_line :: Int -> Table -> String
show_line n t = show_ch (n + (nr_col t) + 1) []

--creeaza string dintr-un tabel
create_table :: [Int] -> Table -> String
create_table size t = show_line (sum size) t ++ show_sche size t ++ show_line (sum size) t
                 ++ show_date size t ++ show_line (sum size) t

--Afiseaza tabela
show_table :: Table -> String
show_table t = create_table (calculate_max_len_col t) t

--afiseaza schema tabelei
show_sche :: [Int] -> Table -> String
show_sche l (Table t e) = foldr (++) [] ((zipWith (\x n -> add_space x (n - length x) []) t) l)
                                                                                         ++ "|\n"

--adauga spatii pentru coloane
add_space :: Field -> Int -> Field -> Field
add_space x 0 acc = "|" ++ x ++ acc
add_space x n acc = add_space x (n -1) (acc ++ " ")

--afiseaza datele tabelei
--parcurg lista de Entry si la fiecare Entry adaug spatii necesare pe coloane
show_date :: [Int] -> Table -> String
show_date l (Table t e) = foldr (++) []--prentru a crea un string
                        $map (\x -> (foldr (++) []
                                            $zipWith (\cl n -> add_space cl (n - length cl) []) x l
                                    ) ++ "|\n") e


data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

data Query = Filter FilterCondition Query |
             Select [String] Query |
             SelectLimit [String] Int Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

--evalueaza un Query
eval :: Query -> Table
eval (Atom t) = t
eval (Select f (Atom t)) = Table (f) (eval_entry f t)
eval (Select f q) = eval (Select f $Atom (eval q))
eval (SelectLimit f n (Atom t)) = Table f (take n (eval_entry f t))
eval (SelectLimit f n q) = eval (SelectLimit f n $Atom (eval q))
eval (Filter filt_cond (Atom (Table t e))) = Table t $filter_date filt_cond (Table t e)
eval (Filter f q) = eval (Filter f $Atom (eval q))
eval (q :|| q') = my_or (eval q) (eval q')
--concateneaza liniile a 2-a tabele
my_or :: Table -> Table -> Table
my_or (Table a b) (Table a' b') = Table a (b ++ b')

--filtreaza datele unei tabele
filter_date :: FilterCondition -> Table -> [Entry]
filter_date filt_cond (Table t e) = filter (\x -> my_filter filt_cond (Table t e) x) e

--filtreaza dupa o funtie
filt_f _ i [] _ = False
filt_f (c:cs) i (x:xs) f = if c == 0 then filt_f cs i xs f
                            else f i x

lt :: Integer -> String -> Bool
lt a b = a > (read b :: Integer)
eq :: String -> String -> Bool
eq a b = a == b

--funtia de filtrare In, Lt, Eq si Not
my_filter :: FilterCondition -> Table -> Entry -> Bool
my_filter (In _ []) _ _ = False
my_filter (In field s) t l = my_filter (Eq field (head s) ) t l || my_filter (In field $tail s) t l
my_filter (Eq field s) (Table t e) l = filt_f (f [field] t) s l eq
my_filter (Not filt) t l = not (my_filter filt t l)
my_filter (Lt field i) (Table t e) l =  filt_f (f [field] t) i l lt

--selecteaza elementele din tabela
eval_select :: [Integer] -> Table -> [Entry]
eval_select l (Table t e) = map (\x -> g l x) e

--pentru fiecare fild il adauuga pe rand intr-o lista
--care vor fi liniile tabelei
eval_entry :: [String] -> Table -> [Entry]
eval_entry [] t = []
eval_entry (x:[]) t = eval_select (list_fild [x] t) t
eval_entry (x:xs) t =  zipWith (++) (eval_select (list_fild [x] t) t)  $eval_entry xs t

--creeaza o lista cu coloanele folosite din tabela
list_fild :: [String] -> Table -> [Integer]
list_fild cmp (Table t e) = f cmp t

--creaza o lista cu 1 si 0 pentru schema tabelei
--care contine sau nu valoare
f :: [String] -> [String] -> [Integer]
f _ [] = []
f str (x:xs) = if elem x str then [1] ++ f str xs
                    else [0] ++ f str xs

--filtreaza elemetele , il adauga sau nu in lista
g :: [Integer] -> [String] -> [String]
g [] (x:xs) = [x] ++ g [] xs
g [] [] = []
g (c:cs) (x:xs) = if c == 1 then [x] ++ g cs xs
                    else g cs xs


same_zone :: String -> Query
same_zone id = Atom $eval $Select ["user_id", "occupation"]
                $Atom $eval $Filter (Not (Eq "user_id" id) )
                $Atom $eval $Filter (Eq "zone" (zone $id_zone id)) $Atom user_info
                    where
                        zone (Table a [b]) = head b
                        id_zone id = eval $Select ["zone"] 
                                     $ Atom $eval $Filter (Eq "user_id" id) 
                                     $ Atom user_info

male_within_age :: Integer -> Integer -> Query
male_within_age x y = Atom $eval $Select ["occupation", "zone"]
                        $Atom $eval $Filter (Eq "sex" "M")
                        $Atom $eval $Filter (Lt "age" y)
                        $Atom $eval $Filter (Not (Lt "age" (x + 1))) $ Atom user_info

mixed :: [String] -> [String] -> Integer -> Query
mixed zone ocupatii x = Atom $eval $Select ["user_id"]
                        $Atom $eval $ Filter (In "occupation" ocupatii)
                        $Atom $eval $ Filter (In "zone" zone)
                        $Atom $eval $Filter (Lt "age"x)
                        $Atom user_info
