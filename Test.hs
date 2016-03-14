import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Modifiers
import System.Process
import System.IO
import System.Directory
import Data.List (union,nub)
import Data.Text (unpack, pack, split)
import Data.Char (isLetter, isSpace, toLower)

import Data.Int
import Database.SQLite.Types
import Database.SQLite.Base
import Database.SQLite
import Data.Either

import qualified Data.ByteString.Char8 as T
import Data.Text.Encoding (decodeUtf8)

toPairs :: [a] -> [(a,a)]
toPairs [] = []
toPairs (x:[]) = [(x,x)]
toPairs (x:y:xs)  = [(x,y)] ++ (toPairs xs) 

main :: IO ()
main = do --quickCheckWith stdArgs {maxSuccess = 500} prop_create_select
          --let strings = [("prop_create_select",prop_create_select),("prop_delete_select",prop_delete_select),("prop_dyn_delete_select",prop_dyn_delete_select),("prop_join_select",prop_join_select),("prop_update",prop_update),("prop_dyn_update",prop_dyn_update)]
          --mapM_ putStrLn ["prop_create_select","prop_dyn_create_select"]
          --mapM_ quickCheck [prop_create_select,prop_delete_select]

-- >          --mapM_ (\(x,y) -> putStrLn x >> quickCheck y) strings

          --mapM_ quickCheck(mapM_ putStrLn ["prop_create_select","prop_dyn_create_select"])

          --concat [ quickCheck list | list <- strings ]

          --putStrLn "prop_create_select :"
          quickCheck prop_create_select 
          ----putStrLn "prop_dyn_create_select :"
          ----quickCheck prop_dyn_create_select
          --putStrLn "prop_delete_select :" 
          --quickCheck prop_delete_select
          --putStrLn "prop_dyn_delete_select :"
          --quickCheck prop_dyn_delete_select
          --putStrLn "prop_join_select :"
          --quickCheck prop_join_select
          --putStrLn "prop_update :"
          --quickCheck prop_update
          --putStrLn "prop_dyn_update :"
          --quickCheck prop_dyn_update
         


          --dBTables <- runDB $ "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;"
          --apa <- getDBTables
          --putStrLn "All tables in the DateBase: "
          --putStrLn $ show apa

safeHead :: [String] -> String  
safeHead list = case take 1 list of 
        [] -> "" 
        otherwise -> head list

safeHead' :: [[String]] -> [String]
safeHead' list = case take 1 list of 
        [] -> [""]
        otherwise -> head list

safeLast :: [String] -> String
safeLast list = case drop ((length list)-1) list of 
        [] -> ""  
        otherwise -> last list

safeRead :: String -> Int
safeRead list = case list of
        [] -> 0
        otherwise -> read list

safeInit :: [a] -> [a]
safeInit list = take (length list -1) list

decodeString :: String -> String
decodeString str = unpack $  decodeUtf8 (T.pack $ str)

fromRight' :: Either a b -> b
fromRight' (Right b) = b

getDBVals :: String -> [(String,String)]
getDBVals ans = toPairs ans'''
    where ans' = if length ans > 0 then  pack $ init ans else pack ""
          ans'' = split (',' ==) ans'
          ans''' = map unpack ans''

forbidden :: [String]
forbidden = ["abort", "action", "add", "after", "all", "alter", "analyze", "and", "as", "asc", "attach", "autoincrement", 
              "before", "begin", "between", "by", "cascade", "case", "cast", "check", "collate", "column", "commit", 
              "conflict", "constraint", "create", "cross" ,"current_date", "current_time", "current_timestamp",
              "database", "default", "deferrable", "deferred", "delete", "desc", "detach", "distinct", "drop", "each", 
              "else", "end", "escape", "except", "exclusive", "exists", "explain", "fail", "for", "foreign", "from", "full", 
              "glob", "group", "having", "if", "ignore", "immediate", "in", "index", "indexed", "initially", "inner", "insert", 
              "instead", "intersect", "into", "is", "isnull", "join", "key", "left", "like", "limit", "match", "natural", "no", 
              "not", "notnull", "null", "of", "offset", "on", "or", "order", "outer", "plan", "pragma", "primary", "query", "raise", 
              "recursive", "references", "regexp", "reindex", "release", "rename", "replace", "restrict", "right", "rollback", 
              "row", "savepoint", "select", "set", "table", "temp","temporary", "then", "to", "transaction", "trigger", "union", 
              "unique", "update", "using", "vacuum", "values", "view", "virtual", "when", "where", "with", "without"]

getValue :: Value -> String
getValue (Int a) = show a
getValue (Text a) = a
getValue (Double a) = show a
getValue (Blob a) = show a
getValue (Null) = ""

getDBTables :: IO [String]
getDBTables = do
    tables <- runDB $ "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;"
    return $ map ((map toLower). snd) tables

removeDuplicate :: [String] -> [String] 
removeDuplicate strList = findDup lowerStrList strList
    where lowerStrList = map (map toLower) strList
          findDup [] [] = []
          findDup (x:xs) (y:ys) | x `elem` xs  = findDup xs ys
                                | otherwise = [y] ++ findDup xs ys

isNotDuplicate :: [String] -> Bool
isNotDuplicate strList = findDup lowerStrList strList
    where lowerStrList = map (map toLower) strList
          findDup [] [] = True
          findDup (x:xs) (y:ys) | x `elem` xs  = False
                                | otherwise = findDup xs ys

runDB :: String -> IO [(String,String)]
runDB str = do
  db  <- openConnection "test.db"
  val <- execStatement db str :: IO (Either String [[Row Value]])
  closeConnection db
  if isRight val then return $ map (\(x,y) -> (x, decodeString $ getValue y)) $ concat $ concat $ fromRight' val else return []

prop_create_select :: Word -> Word -> Int ->  Property
prop_create_select (Word table) (Word str) num = monadicIO $ do
    tables <- run getDBTables
    if map toLower table `elem` tables then run $ runDB $ dropTable table else return [("","")]
    
    run $ runDB $ createTable table ["t","b"]
    run $ runDB $ insertInTable table ["t","b"] [str,show num] [num] 
    ans <- run $ runDB $ selectTable "*" table "" 

    let colVals = map snd ans
    assert(safeHead colVals == str && ((safeRead (safeLast colVals)) :: Int) == num )  

--prop_dyn_create_select :: Word -> FixedStringList ->  Property
--prop_dyn_create_select (Word table) (Word str) num = monadicIO $ do
--    tables <- run getDBTables
--    if map toLower table `elem` tables then run $ runDB $ dropTable table else return [("","")]
    
--    run $ runDB $ createTable table ["t","b"]
--    run $ runDB $ insertInTable table ["t","b"] [str,show num] [num] 
--    ans <- run $ runDB $ selectTable "*" table "" 

--    let colVals = map snd ans
--    assert(safeHead colVals == str && ((safeRead (safeLast colVals)) :: Int) == num )  

prop_delete_select :: Word -> Word -> Int ->  Property
prop_delete_select (Word table) (Word str) num = monadicIO $ do
    tables <- run getDBTables
    if map toLower table `elem` tables then run $ runDB $ dropTable table else return [("","")]
    
    run $ runDB $ createTable table ["t","b"]
    run $ runDB $ insertInTable table ["t","b"] [str,show num] [num] 

    run $ runDB $ deleteTable table (whereClause [("b",show num)]) 
    ans <- run $ runDB $ selectTable "*" table "" 

    assert(null ans)
  
prop_dyn_delete_select :: Word -> FixedStringList -> [Int] ->  Property
prop_dyn_delete_select (Word table) (FixedStringList strings) num = forAll (choose (0, length strings - 1)) $ \delelem -> monadicIO $  do
    tables <- run getDBTables
    if map toLower table `elem` tables then run $ runDB $ dropTable table else return [("","")]

    let colums = head strings
    let rowToDelete = head $ strings !! delelem
    run $ runDB $ createTable table colums
    run $ sequence [ runDB $ insertInTable table colums list num | list <- strings ]

    ansBefore <- run $ runDB $ selectTable "*" table ""
    run $ runDB $ deleteTable table (whereClause [(head colums, rowToDelete)])
    ansAfter <- run $ runDB $ selectTable "*" table ""
    
    ansDelCheck <- run $ runDB $ selectTable "*" table (whereClause [(head colums,rowToDelete)]) 

    assert(length ansBefore - length colums == length ansAfter)
    assert(null ansDelCheck)


prop_join_select :: Word -> Word -> Word -> Int ->  Property
prop_join_select (Word table1) (Word table2) (Word str) num = monadicIO $ do
    tables <- run getDBTables
    if map toLower table1 `elem` tables then run $ runDB $ dropTable table1 else return [("","")]
    if map toLower table2 `elem` tables then run $ runDB $ dropTable table2 else return [("","")]

    run $ runDB $ createTable table1 ["t","b"]
    run $ runDB $ createTable table2 ["t","b"]
    run $ runDB $ insertInTable table1 ["t","b"] [str,show num] [num] 
    run $ runDB $ insertInTable table2 ["t","b"] [str,show num] [num] 

    ans <- run $ runDB $ selectTable "*" table1 (joinClause table2 "t") 

    ans1 <- run $ runDB $ selectTable "b" table1 "" 
    ans2 <- run $ runDB $ selectTable "b" table2 "" 

    assert (length (map fst ans)-1 == length (map fst ans1) + length (map fst ans2))
    assert (str `elem` (map snd ans))
    assert (show num `elem` (map snd ans))


prop_update :: Word -> Word -> Word -> Int ->  Property
prop_update (Word table) (Word str) (Word newStr) num = (str /= newStr) ==> monadicIO $ do
    tables <- run getDBTables
    if map toLower table `elem` tables then run $ runDB $ dropTable table else return [("","")]
    
    run $ runDB $ createTable table ["t","b"]
    run $ runDB $ insertInTable table ["t","b"] [str,show num] [num] 
    ansOld <- run $ runDB $ selectTable "t" table (whereClause [("b",show num),("t",str)])  

    run $ runDB $ updateTable table "t" newStr (whereClause [("b",show num),("t",str)])
    ansNew <- run $ runDB $ selectTable "t" table (whereClause [("b",show num),("t",newStr)])    

    assert(concat(map snd ansOld) /= concat(map snd ansNew))

prop_dyn_update :: Word -> FixedStringList -> [Int] ->  Property
prop_dyn_update (Word table) (FixedStringList strings) num = monadicIO $ do
    tables <- run getDBTables
    if map toLower table `elem` tables then run $ runDB $ dropTable table else return [("","")]
    
    let colums = head strings
    let str = head (head strings)
    let newStr = head(drop 1 (head strings))

    run $ runDB $ createTable table colums 
    run $ sequence [ runDB $ insertInTable table colums list num | list <- (take 5 strings) ]
    ansOld <- run $ runDB $ selectTable str table (whereClause [(str,str)])  

    run $ runDB $ updateTable table str newStr (whereClause [(str,str)])
    ansNew <- run $ runDB $ selectTable str table (whereClause [(str,newStr)])    

    assert(concat(map snd ansOld) /= concat(map snd ansNew))





createTable :: String -> [String] -> String
createTable table colNames =  do
    let newColnames' = safeInit $ foldr (++) "" $ map (++",") colNames
    let newColnamesType = safeInit $ foldr (++) "" $ map (++" text,") $ colNames
    "create table " ++ table ++" (" ++ newColnamesType ++ ");"  

insertInTable :: String -> [String] -> [String] -> [Int] -> String
insertInTable table colNames values num = do
    let val = safeInit $ foldr (++) "" (map (\x -> "\'"++ x ++ "\',") $ values) 
    let newColnames' = safeInit $ foldr (++) "" $ map (++",") colNames
    "insert into " ++ table ++ " (" ++  newColnames' ++ ") values (" ++ val ++ " );"

dropTable :: String -> String
dropTable table = "drop table " ++ table ++ ";"

deleteTable :: String -> String -> String
deleteTable table wher = "delete from " ++ table ++ " " ++ wher 

selectTable :: String -> String -> String -> String
selectTable name table "" = "select " ++ name ++  " from " ++ table ++ ";" 
selectTable name table wher = "select " ++ name ++  " from " ++ table ++ " " ++ wher 

updateTable :: String -> String -> String -> String -> String
updateTable table column newStr wher = "update " ++ table ++ " set " ++ column ++ " = \'" ++ newStr ++ " " ++ wher 

letterGen :: Gen Char
letterGen = suchThat arbitrary isLetter

stringGen :: Gen String
stringGen = suchThat (listOf1 letterGen) (\x -> map toLower x `notElem` forbidden)

stringListGen :: Gen [[String]]
stringListGen = sized $ \s -> do 
  let s' = if s < 2 then 2 else s
  listOf1 $ suchThat (vectorOf s' $ stringGen) isNotDuplicate

stringListGen' :: Gen [[String]]
stringListGen' = sized $ \s -> do 
  let s' = if s < 2 then 2 else s
  vectorOf 5 $ suchThat (vectorOf s' $ stringGen) isNotDuplicate

joinClause :: String -> String -> String
joinClause table column = "cross join " ++ table ++ " using (" ++ column ++ ");" 

whereClause :: [(String,String)] -> String
whereClause wher = "where " ++ (removeNLastElements 5 $ foldr (++) "" (map (\(x,y) -> x ++ " = \'" ++ y ++ "\' and ")wher)) ++ ";" 

removeNLastElements :: Int -> String -> String
removeNLastElements int list = take (length list-int) list
 

newtype FixedStringList = FixedStringList [[String]] deriving (Eq,Show)

instance Arbitrary FixedStringList where
  arbitrary = do
                list <- stringListGen'
                return (FixedStringList list)


newtype Word = Word String
  deriving (Show,Eq)

instance Arbitrary Word where
  arbitrary = sized $ \s -> do
    let s' = if s == 0  then 1 else s
    l <- suchThat (vectorOf s' letterGen) (\x -> map toLower x `notElem` forbidden)
    return (Word l)





 --prop_dyn_delete_select :: String -> [String] -> [String] -> [Int] ->  Property
--prop_dyn_delete_select table' colNames' str' num = not(null table) && (and $ map (not . null) colNames) && ((map toLower table) `notElem` forbidden) 
--    && (and $ map  ((`notElem` forbidden) . map toLower) colNames) && and(map (`notElem` str) forbidden) && (length colNames'>0)&& (length str'>0)&& (length num>0) ==> monadicIO $ do
--    tables <- run getDBTables
--    if map toLower table `elem` tables then run $ runDB $ "drop table " ++ table ++ ";" else return [("",Null)]
--    let numOfCols = (length str + length num) `min` (length colNames)
--    let minTemp = (length str `min` length num)
--    let minNum = length num < length str
--    let numOfNum = if numOfCols == (length colNames) then (if minTemp > ((length colNames) `div` 2) then (length colNames `div` 2) else (if (minNum) then minTemp else (numOfCols - minTemp))) else length num
--    let numOfStr  = (numOfCols - numOfNum)

--    run $ runDB $ createTable table numOfCols colNames
--    run $ runDB $ insertInTable table numOfCols colNames numOfNum numOfStr str num
--    run $ runDB $ deleteTable table numOfCols colNames numOfStr num str

--    ans <- run $ runDB $ selectTable "*" table 
--    assert (null ans)
--  where table = filter (not . isSpace) $ filter isLetter table'
--        colNames = removeDuplicate $ map ( filter (\x -> (isLetter x)&&(not $ isSpace x))) colNames'        
--        str = map (filter isLetter) str'

--    run $ runDB $ createTable table numOfCols colNames



--prop_join_select :: String -> String -> String -> Int ->  Property
--prop_join_select table1' table2' str' num = not(null table1) && not(null table2) && ((map toLower table1) `notElem` forbidden) && ((map toLower table2) `notElem` forbidden) && (str `notElem` forbidden)  ==> monadicIO $ do
--    tables <- run getDBTables
--    if map toLower table1 `elem` tables then run $ runDB $ "drop table " ++ table1 else return [("","")]
--    if map toLower table2 `elem` tables then run $ runDB $ "drop table " ++ table2 else return [("","")]

--    run $ runDB $ "create table " ++ table1 ++ " (t text, b text);" 
--    run $ runDB $ "create table " ++ table2 ++ " (t text, b text);" 
--    run $ runDB $ "insert into " ++ table1 ++ " (t,b) values (\'" ++ str ++ "\'," ++ show num ++ " );"
--    run $ runDB $ "insert into " ++ table2 ++ " (t,b) values (\'" ++ str ++ "\'," ++ show num ++ " );"

--    ans <- run $ runDB $ "select * from " ++ table1 ++ " cross join " ++ table2 ++ " using (t)"
--    ans1 <- run $ runDB $ "select b from " ++ table1 
--    ans2 <- run $ runDB $ "select b from " ++ table2 

--    assert (length (map fst ans)-1 == length (map fst ans1) + length (map fst ans2))
--    assert (str `elem` (map snd ans))
--    assert (show num `elem` (map snd ans))


--  where table1 = filter (not . isSpace) $ filter isLetter table1'
--        table2 = filter (not . isSpace) $ filter isLetter table2'
--        str = filter isLetter str'


    --run $ putStrLn $ "ANS STRING :" ++ concat (map fst ans) ++ ":"
    --run $ putStrLn $ "ANS VALUE :" ++ concat (map (getValue . snd) ans) ++ ":"

    --run $ putStrLn $ "Length ANS str-1 :" ++ show ((length (map fst ans))-1) 
    --run $ putStrLn $ "Length table1 str :" ++ show (length (map fst ans1))    
    --run $ putStrLn $ "Contents table1 str :" ++ concat(map fst ans1)
    --run $ putStrLn $ "Length table2 str :" ++ show (length (map fst ans2))   ++ "\n" 

    -- run $ putStrLn $ "Length ANS values :" ++ show (length ((map (getValue . snd) ans))) ++ ":"
    -- run $ putStrLn $ "Length STRRRR :" ++ show (length str) 

    --run $ putStrLn $ "STR IS :" ++ str ++ ":"
    --run $ putStrLn $ "NUM IS :" ++ show num ++ ":"
    --run $ putStrLn $ "ANS CONTENTS :" ++ show(map (decodeString . getValue . snd) ans) ++ ":" 

--    assert (str `elem` (map (getValue . snd) ans))

-- lista med tabellerna istället.
-- sorterar på dom som har en och samma värde i kolumnen t

--    let kok =  concat $ concat $ fromRight' lol -- ::(ColumnName, String)







--prop_update :: String -> String -> String -> Int ->  Property
--prop_update table' str' newStr' num = not(null table) && ((map toLower table) `notElem` forbidden) && (str `notElem` forbidden) &&
--   (newStr `notElem` forbidden) && (str /= newStr) && not(null newStr) && not(null str) ==> monadicIO $ do
--    tables <- run getDBTables
--    if map toLower table `elem` tables then run $ runDB $ "drop table " ++ table else return [("","")]
--    run $ runDB $ "create table " ++ table ++ " (t text, b text);" 
--    run $ runDB $ "insert into " ++ table ++ " (t,b) values (\'" ++ str ++ "\'," ++ show num ++ " );"
--    ansOld <- run $ runDB $ "select t from " ++ table ++ " where b = \'" ++ show num  ++ "\' and t = \'" ++ str ++ "\';"
--    run $ putStrLn $ "Contents table BEFORE changed (Value) :" ++ concat(map snd ansOld)   ++ "\n" 

--    run $ runDB $ "update " ++ table ++ " set t = '" ++ newStr ++ "' where b = \'" ++ show num  ++ "\' and t = \'" ++ str ++ "\';"
--    ansNew <- run $ runDB $ "select t from " ++ table ++ " where b = \'" ++ show num  ++ "\' and t = \'" ++ newStr ++ "\';"
--    run $ putStrLn $ "Contents table AFTER changed (Value) :" ++ concat(map snd ansNew)   ++ "\n" 
    
--    assert(concat(map snd ansOld) /= concat(map snd ansNew))

--  where table = filter (not . isSpace) $ filter isLetter table'
--        str = filter isLetter str'
--        newStr = filter isLetter newStr'





--type NELetters = NonEmptyList Letter

--prop_dyn_delete_select' :: NELetters -> [String] -> [String] -> [Int] ->  Property
--prop_dyn_delete_select' (NonEmpty table') colNames' str' num = (and $ map (not . null) colNames) && ((map toLower table) `notElem` forbidden) 
--    && (and $ map  ((`notElem` forbidden) . map toLower) colNames) && (length colNames'>0)&& (length str'>0)&& (length num>0) ==> monadicIO $ do
--    tables <- run getDBTables
--    if map toLower table `elem` tables then run $ runDB $ "drop table " ++ table ++ ";" else return ""
--    let numOfCols = (length str + length num) `min` (length colNames)
--    let minTemp = (length str `min` length num)
--    let minNum = length num < length str
--    let numOfNum = if numOfCols == (length colNames) then (if minTemp > ((length colNames) `div` 2) then (length colNames `div` 2) else (if (minNum) then minTemp else (numOfCols - minTemp))) else length num
--    let numOfStr  = (numOfCols - numOfNum)

    
--    --let temp = (foldr (++) "" (map (\x -> "\'"++ x ++ "\',") $  map show (take numOfNum num)))
--    --let temp' = if length temp == 0 && numOfNum >0 then "\'\'," else temp 
--    --let val = safeInit $ foldr (++) "" (map (\x -> "\'"++ x ++ "\',") $ take numOfStr str) ++ temp'
    
--    --let newColnames  = take numOfCols colNames
--    --let newColnames' = safeInit $ foldr (++) "" $ map (++",") newColnames

--    --run $ putStrLn $ "numOfCols:" ++ show numOfCols ++ " L:" ++ show (length colNames) ++ "  numOfNum:" ++ show numOfNum ++ " L:" ++ show (length num) ++ "  numOfStr:" ++ show numOfStr ++ " L:" ++ show  (length str)

--    run $ runDB $ createTable table numOfCols colNames
--    run $ runDB $ insertInTable table numOfCols colNames numOfNum numOfStr str num
--    run $ runDB $ deleteTable table numOfCols colNames numOfStr num str
--    --run $ runDB $ "create table " ++ table ++" (" ++ newColnamesType ++ ");"  -- " (t text, b num)"
--    --run $ runDB $ "insert into " ++ table ++ " (" ++  newColnames' ++ ") values (" ++ val ++ " );"
--    --run $ runDB $ "delete from " ++ table ++ " where " ++ head newColnames ++ " = \'" ++ (if numOfStr == 0 then show (head num) else  head str) ++ "\' ;"

--    ans <- run $ runDB $ selectTable "*" table 
----    ans <- run $ runDB $ "select * from " ++ table ++ ";"
--    let result = getDBVals ans
--    assert(length result == 1 && (fst (head result)) == "" && (snd(head result)) =="")
--  where table = [ c | Letter c <- table']
--        colNames = removeDuplicate $ map ( filter (\x -> (isLetter x)&&(not $ isSpace x))) colNames'        
--        str = map (filter isLetter) str'


--createTable :: String -> Int -> [String] -> String
--createTable table numOfCols colNames =  do
--    let newColnames  = take numOfCols colNames
--    let newColnames' = safeInit $ foldr (++) "" $ map (++",") newColnames
--    let newColnamesType = safeInit $ foldr (++) "" $ map (++" text,") $ newColnames
--    "create table " ++ table ++" (" ++ newColnamesType ++ ");"  

--insertInTable :: String -> Int -> [String] -> Int -> Int -> [String] -> [Int] -> String
--insertInTable table numOfCols colNames numOfNum numOfStr str num = do
--    let temp = (foldr (++) "" (map (\x -> "\'"++ x ++ "\',") $  map show (take numOfNum num)))
--    let temp' = if length temp == 0 && numOfNum >0 then "\'\'," else temp 
--    let val = safeInit $ foldr (++) "" (map (\x -> "\'"++ x ++ "\',") $ take numOfStr str) ++ temp'
    
--    let newColnames  = take numOfCols colNames
--    let newColnames' = safeInit $ foldr (++) "" $ map (++",") newColnames
--    "insert into " ++ table ++ " (" ++  newColnames' ++ ") values (" ++ val ++ " );"

--deleteTable :: String -> Int -> [String] -> Int -> [Int] -> [String] -> String
--deleteTable table numOfCols colNames numOfStr num str = do 
--    let newColnames  = take numOfCols colNames
--    "delete from " ++ table ++ " where " ++ head newColnames ++ " = \'" ++ (if numOfStr == 0 then show (head num) else  head str) ++ "\' ;"

--selectTable :: String -> String -> String
--selectTable name table = "select " ++ name ++  " from " ++ table ++ ";"






--stringListGen :: Int -> Int -> Gen [String]
--stringListGen num size = vectorOf num $ vectorOf size $ stringGen

--stringListGen = sized $ \s -> listOf1 $ vectorOf s $ stringGen





--newtype FixedStringList = FixedStringList [String] deriving (Eq,Show)

--instance Arbitrary FixedStringList where
--  arbitrary = do
--                list <- listOf1 stringGen
--                let xs = removeDuplicate list
--                return (FixedStringList xs)





-- let filterChar = filter (\x -> (isLetter x)&&(not $ isSpace x))
-- ((map toLower x) `notElem` forbidden)
--
-- map removeDuplicate $ map (map ( filter (\x -> (isLetter x)&&(not $ isSpace x)))) xs


--newtype FixedStringList = FixedStringList [[String]] deriving (Eq,Show)

--instance Arbitrary FixedStringList where
--  arbitrary = sized $ \s -> do
--                 xs <- vectorOf 3 $ vectorOf s $ resize s arbitrary
--                 let filtedChar =  map (map ( filter (\x -> (isLetter x)&&(not $ isSpace x)))) xs
--                 let allowedList = map (filter $ (\x -> (map toLower x) `notElem` forbidden)) filtedChar
--                 let xs' = map removeDuplicate allowedList
--                 return (FixedStringList xs')

--newtype Letter = Letter Char
--  deriving (Show)

--instance Arbitrary Letter where
--  arbitrary = do
--    l <- suchThat arbitrary isLetter
--    return $ Letter l

-- let filterChar = filter (\x -> (isLetter x)&&(not $ isSpace x))
-- ((map toLower x) `notElem` forbidden)
--
-- map removeDuplicate $ map (map ( filter (\x -> (isLetter x)&&(not $ isSpace x)))) xs
