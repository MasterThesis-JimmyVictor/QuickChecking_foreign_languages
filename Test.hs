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
main = do 
       let strings = [("prop_create_select:",quickCheck prop_create_select),
                      ("prop_dyn_create_select:",quickCheck prop_dyn_create_select),
                      ("prop_delete_select:",quickCheck prop_delete_select),
                      ("prop_dyn_delete_select:",quickCheck prop_dyn_delete_select),
                      ("prop_join_select:",quickCheck prop_join_select),
                      ("prop_update:",quickCheck prop_update),
                      ("prop_dyn_update:",quickCheck prop_dyn_update),
                      ("quickCheck prop_like:", quickCheck prop_like),
                      ("prop_dyn_or:", quickCheck prop_dyn_or)]
       mapM_ (\(x,y) -> putStrLn x >> y >> removeFile "test.db") strings

--quickCheckWith stdArgs {maxSuccess = 500} prop_create_select
         

safeHead :: [String] -> String  
safeHead list = case take 1 list of 
        [] -> "" 
        otherwise -> head list

--safeHead' :: [[String]] -> [String]
--safeHead' list = case take 1 list of 
--        [] -> [""]
--        otherwise -> head list

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

--removeDuplicate :: [String] -> [String] 
--removeDuplicate strList = findDup lowerStrList strList
--    where lowerStrList = map (map toLower) strList
--          findDup [] [] = []
--          findDup (x:xs) (y:ys) | x `elem` xs  = findDup xs ys
--                                | otherwise = [y] ++ findDup xs ys

isNotDuplicate :: [String] -> Bool
isNotDuplicate strList = findDup lowerStrList strList
    where lowerStrList = map (map toLower) strList
          findDup [] [] = True
          findDup [x] [y] = x /= y
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
    monitor (
      whenFail'
      (putStrLn $ "strinDatabase: " ++ (show $ safeHead colVals) ++ " str: " ++ show str ))
    monitor (
      whenFail'
      (putStrLn $ "strinDatabase2: " ++ (safeHead colVals) ++ " str2: " ++ str ))

    assert(safeHead colVals == str && ((safeRead (safeLast colVals)) :: Int) == num )  

prop_dyn_create_select :: Word -> FixedStringList -> [Int] -> Property
prop_dyn_create_select (Word table) (FixedStringList strings) num = monadicIO $ do
    tables <- run getDBTables
    if map toLower table `elem` tables then run $ runDB $ dropTable table else return [("","")]
    
    let colums = head strings
    run $ runDB $ createTable table colums
    run $ sequence [ runDB $ insertInTable table colums list num | list <- strings ]
    ans <- run $ runDB $ selectTable "*" table "" 

    let colVals = map snd ans
    assert( length ans == ((length colums) * length strings))

prop_delete_select :: Word -> Word -> Int ->  Property
prop_delete_select (Word table) (Word str) num = monadicIO $ do
    tables <- run getDBTables
    if map toLower table `elem` tables then run $ runDB $ dropTable table else return [("","")]
    
    run $ runDB $ createTable table ["t","b"]
    run $ runDB $ insertInTable table ["t","b"] [str,show num] [num] 

    run $ runDB $ deleteTable table (whereClause [("b",show num, "")]) 
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
    run $ runDB $ deleteTable table (whereClause [(head colums, rowToDelete, "")])
    ansAfter <- run $ runDB $ selectTable "*" table ""
    
    ansDelCheck <- run $ runDB $ selectTable "*" table (whereClause [(head colums,rowToDelete,"")]) 

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
    ansOld <- run $ runDB $ selectTable "t" table (whereClause [("b",show num,"and"),("t",str,"")])  

    run $ runDB $ updateTable table "t" newStr (whereClause [("b",show num,"and"),("t",str,"")])
    ansNew <- run $ runDB $ selectTable "t" table (whereClause [("b",show num,"and"),("t",newStr,"")])    

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
    ansOld <- run $ runDB $ selectTable str table (whereClause [(str,str,"")])  

    run $ runDB $ updateTable table str newStr (whereClause [(str,str,"")])
    ansNew <- run $ runDB $ selectTable str table (whereClause [(str,newStr,"")])    

    assert(concat(map snd ansOld) /= concat(map snd ansNew))

prop_like :: OneChar -> OneChar ->  Property
prop_like (OneChar word1) (OneChar word2) = (word1 /= word2) ==> monadicIO $ do
  notEqual <- run $ runDB $ ("SELECT '" ++ [word1] ++ "' LIKE '" ++ [word2] ++ "' ;")
  assert(snd (head notEqual) == show 0)

prop_dyn_or :: Word -> FixedStringList -> [Int] -> Property
prop_dyn_or (Word table) (FixedStringList strings) num = forAll (choose (1, length strings - 1)) $ \chooseElem -> monadicIO $ do
    tables <- run getDBTables
    if map toLower table `elem` tables then run $ runDB $ dropTable table else return [("","")]
    
    let colums = head strings                 
    let str = head (head strings)             
    let secStr = head(drop 1 (head strings))  
    let randStr = head $ strings !! chooseElem 

    run $ runDB $ createTable table colums 
    run $ sequence [ runDB $ insertInTable table colums list num | list <- (take 5 strings) ]
    
    noneInAnsList <- run $ runDB $ selectTable str table (whereClause $ map (\x -> (str,x,"or")) (concat $ map (drop 1) strings))
    oneInAnsList <- run $ runDB $ selectTable str table (whereClause $ [(str,randStr,"or")] ++ map (\x -> (str,x,"or")) (drop 1 $ head strings))
    allInAnsList <- run $ runDB $ selectTable str table (whereClause $ map (\x -> (str,x,"or")) (map head strings))
        
    assert (null noneInAnsList)
    assert (randStr `elem` (map snd oneInAnsList) && secStr `notElem` (map snd oneInAnsList))
    assert((map head strings) `elem` [(map snd allInAnsList)]) 


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
  suchThat (vectorOf 5 $ suchThat (vectorOf s' $ stringGen) isNotDuplicate) (\x -> isNotDuplicate $ concat x)

joinClause :: String -> String -> String
joinClause table column = "cross join " ++ table ++ " using (" ++ column ++ ");" 

whereClause :: [(String,String,String)] -> String
whereClause wher = "where " ++ (removeNLastElements (length(thirdElemTuple)+2) $ foldr (++) "" (map (\(x,y,z) -> x ++ " = \'" ++ y ++ "\' " ++ z ++ " ")wher)) ++ ";" 
  where thirdElemTuple = thirdElemTuple' (last wher)
        thirdElemTuple' (a,b,c) = c

removeNLastElements :: Int -> String -> String
removeNLastElements int list = take (length list-int) list

newtype FixedStringList = FixedStringList [[String]] deriving (Eq,Show)

instance Arbitrary FixedStringList where
  arbitrary = do
                list <- stringListGen'
                return (FixedStringList list)
  shrink (FixedStringList l) = [ FixedStringList x | x <- shrink l, not $ null x, not $ any null x, not $ any (any null) x ]

newtype Word = Word String
  deriving (Show,Eq)

instance Arbitrary Word where
  arbitrary = sized $ \s -> do
    let s' = if s == 0  then 1 else s
    l <- suchThat (vectorOf s' letterGen) (\x -> map toLower x `notElem` forbidden)
    return (Word l)


newtype OneChar = OneChar Char deriving (Show,Eq)

instance Arbitrary OneChar where
  arbitrary = do 
    letter <- letterGen
    return (OneChar (toLower letter))