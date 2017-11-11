{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import Prelude hiding (Word)
import Data.Word hiding (Word)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Modifiers
import System.Process
import System.IO
import System.Directory
import Data.List (union,nub)
import qualified Data.Text as TE
import Data.Text (unpack, pack, split)
import Data.Char (isLetter, isSpace, toLower, isNumber, isDigit,intToDigit, toUpper)
import Data.Int
import Data.List (sort)
import Foreign.Storable 
import Foreign.C.String
import Foreign.C.Types
import Data.ByteString.Internal
import Foreign.Ptr
import Foreign.Marshal.Alloc

----------SQLite3-----------
import Database.SQLite.Types
import Database.SQLite.Base
import Database.SQLite
import Data.Either
-----------------------------

import qualified Data.ByteString.Char8 as T
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import Data.List hiding (insert)
import Data.Char

import Data.Time.Clock
import Data.Ratio

-------------------HDBC-----------------
import Database.HDBC (runRaw, execute, fetchAllRows, commit, disconnect, prepare, fetchAllRowsAL, fetchAllRowsAL' , fromSql, toSql,fetchAllRows', SqlValue(..), getTables, rollback, SqlValue)
import qualified Database.HDBC as HDBC
import Database.HDBC.Sqlite3 (connectSqlite3, setBusyTimeout, Connection)
import Database.HDBC.Types(IConnection)
----------------------------------------

-----------------Simple-------------------
import qualified Database.SQLite.Simple as S
import Database.SQLite.Simple (ToRow, toRow, Query(..), query_, SQLData(..), executeNamed,open, close)
import Database.SQLite.Simple.FromRow
------------------------------------------


---- Simplest ---------------
import Database.SmplstSQLite3 (withPrepared, withSQLite, bind, step, column)
import qualified Database.SmplstSQLite3 as SI
----------------------------

-------- Direct sqlite ------
import Database.SQLite3 (bindSQLData, Database, finalize, exec)
import qualified Database.SQLite3 as D
-----------------------------

------------Persistent---------
import Database.Persist.TH
import Database.Persist.Sqlite (selectList, deleteWhere, Filter, runSqlite, runMigrationSilent, entityVal, Entity, rawQuery)
import qualified Database.Persist.Sqlite as P
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import qualified Data.Conduit.List as CL


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Test
    str TE.Text
    num Int
    dou Double
    bytstr T.ByteString
    deriving (Show)
|]

main = main0

main0 :: IO ()
main0 = do 
  (seedList, inputs) <- main1 [] []
  putStrLn ("Seeds: "  ++ show seedList)
  putStrLn ("inputs: " ++ show inputs)

sqliteBindSqlite = SQLiteBind {openConn = openConnection, closeConn = closeConnection, 
                createQ = \conn query -> (execStatement conn query :: IO (Either String [[Row Value]]) ) >> return (), 
                insertQ = createQ sqliteBindSqlite, 
                selectQ = \conn query -> (execStatement conn query :: IO (Either String [[Row Value]]) ),
                deleteQ = createQ sqliteBindSqlite,
                getVal  = \val -> if isRight val then map (\(x,y) -> (getValueSqlite y)) $ concat $ concat  $ fromRight' (val :: (Either String [[Row Value]])) else [S "", I 0 ,D 0.0, B ""],

                prepCreateQ = createQ sqliteBindSqlite,
                prepInsQ = \db query [S str,I int,D dou,B bytStr] -> (execParamStatement db query [(":1",Text str),(":2",Int (fromIntegral int)),(":3",Double dou ),(":4",Blob bytStr)] :: IO (Either String [[Row Value]]) )  >> return (),
                prepSelQ = selectQ sqliteBindSqlite,
                prepDelQ = createQ sqliteBindSqlite,
                prepGetVal = getVal sqliteBindSqlite,
                prepParam = [":1",":2",":3",":4"],
                static = False}

sqliteBindHdbc = SQLiteBind {openConn = connectSqlite3 , closeConn = ((\conn -> commit conn >> disconnect conn) :: Connection -> IO ()), 
                createQ = runRaw, 
                insertQ = runRaw, 
                selectQ = \conn str -> prepare conn str >>= \select -> execute select [] >> fetchAllRowsAL'  select,
                deleteQ = createQ sqliteBindHdbc,
                getVal = \val -> getHDBCValue' $ map snd $ concat $ (val :: [[(String, SqlValue)]]),
                
                prepCreateQ = createQ sqliteBindHdbc,
                prepInsQ = \db query [S str,I int,D dou,B bytStr] -> prepare db query >>= \insert -> execute insert [toSql str, toSql int, toSql dou, toSql bytStr] >> return (),
                prepSelQ = selectQ sqliteBindHdbc,
                prepDelQ = createQ sqliteBindHdbc,
                prepGetVal = getVal sqliteBindHdbc,
                prepParam = [":1",":2",":3",":4"],
                static = False}

sqliteBindSimple = SQLiteBind {openConn = open, closeConn = close, 
                createQ = \conn query -> S.execute_ conn (Query (pack query)), 
                insertQ = \conn query -> S.execute_ conn (Query (pack query)), 
                selectQ = \conn query -> query_ conn (Query (pack query)) :: IO [PrepField],
                deleteQ = createQ sqliteBindSimple,
                getVal = \val -> (map getSimpleValue' $ toRow (head (val :: [PrepField]))),

                prepCreateQ = createQ sqliteBindSimple,
                prepInsQ = \db query [S str,I int,D dou,B bytStr] -> S.execute db (Query (pack query)) (PrepField str int dou bytStr),
                prepSelQ =  \conn query -> query_ conn (Query (pack query)) :: IO [PrepField],
                prepDelQ = createQ sqliteBindSimple,
                prepGetVal = \val -> (map getSimpleValue' $ toRow (head (val :: [PrepField]))),
                prepParam = ["?","?","?","?"],
                static = False}

sqliteBindSimpLEST = SQLiteBind {openConn = \str -> return (), closeConn = \db -> return (), 
              createQ = \db query -> withSQLite "test.db" (\conn -> withPrepared conn query (\stmt -> step stmt)) >> return (), 
              insertQ = createQ sqliteBindSimpLEST, 
              selectQ = \db query ->  withSQLite "test.db" (\conn -> do   withPrepared conn query (\stmt -> do
                                                                           step stmt
                                                                           value0 <- column stmt (0 :: Int)
                                                                           value1 <- column stmt (1 :: Int)
                                                                           value2 <- column stmt (2 :: Int)
                                                                           value3 <- column stmt (3 :: Int)
                                                                           return (value0,value1,value2, value3)) :: IO ((TE.Text, Int,Double, T.ByteString), String) ),
              deleteQ = createQ sqliteBindSimpLEST,
              getVal = getSimpLESTValue',

              prepCreateQ = createQ sqliteBindSimpLEST,
              prepInsQ = \db query [S str,I int,D dou,B bytStr] -> withSQLite "test.db" (\conn -> do  withPrepared conn query (\stmt -> do
                                                                                                                     bind stmt "?1" ((pack str) :: TE.Text)
                                                                                                                     bind stmt "?2" (int :: Int)
                                                                                                                     bind stmt "?3" (dou :: Double)
                                                                                                                     bind stmt "?4" (bytStr :: T.ByteString)
                                                                                                                     step stmt)) >> return (),

              prepSelQ =  \db query -> withSQLite "test.db" (\conn -> do   withPrepared conn query (\stmt -> do
                                                                           step stmt
                                                                           value0 <- column stmt (0 :: Int)
                                                                           value1 <- column stmt (1 :: Int)
                                                                           value2 <- column stmt (2 :: Int)
                                                                           value3 <- column stmt (3 :: Int)
                                                                           return (value0,value1,value2,value3)) :: IO ((TE.Text, Int,Double,T.ByteString), String)),
              prepDelQ = createQ sqliteBindSimpLEST,
              prepGetVal =  getSimpLESTValue',
              prepParam = ["?1","?2","?3","?4"],
              static = False}

sqliteBindDirect = SQLiteBind {openConn = \str -> D.open (pack str), closeConn = D.close, 
                createQ = \conn query -> exec conn (pack query),
                insertQ = createQ sqliteBindDirect, 
                selectQ = prepSelQ sqliteBindDirect, 
                deleteQ = createQ sqliteBindDirect,
                getVal = getDirectValue',

                prepCreateQ = createQ sqliteBindDirect,
                prepInsQ = \conn query [S str,I int,D dou,B bytStr] -> do 
                                                                     stmt <- D.prepare conn (pack query)
                                                                     bindSQLData stmt 1 (SQLText (pack str))
                                                                     bindSQLData stmt 2 (SQLInteger (fromIntegral int))
                                                                     bindSQLData stmt 3 (D.SQLFloat dou)
                                                                     bindSQLData stmt 4 (D.SQLBlob bytStr) >> return []
                                                                     D.step stmt 
                                                                     finalize stmt,

                prepSelQ =  \conn query -> do
                                           stmt <- D.prepare conn (pack query)
                                           D.step stmt  
                                           ans <- D.columns stmt
                                           finalize stmt
                                           return ans,
                prepDelQ = createQ sqliteBindDirect,
                prepGetVal = getDirectValue',
                prepParam = ["?1","?2","?3","?4"],
                static = False}


sqliteBindPersist = SQLiteBind {openConn = \str -> return () , closeConn = \conn -> return (), 

                  createQ = \conn query -> return (),
                  insertQ = \conn query -> runSqlite "test.db" $ runMigrationSilent migrateAll >>= \e -> rawQuery (pack $ query) [] $$ CL.consume >> return (), 
                  selectQ = \conn query -> runSqlite "test.db" $ runMigrationSilent migrateAll >> selectList [] [] :: IO [Entity Test], 
                  deleteQ = \db query -> runSqlite "test.db" $ runMigrationSilent migrateAll >> deleteWhere ([] :: [Filter Test]), 
                  getVal = \val -> let ans = entityVal (head val) in [S (unpack $ testStr ans), I (testNum ans), D (testDou ans), B (testBytstr ans)],

                  prepCreateQ = \db query -> return (),
                  prepInsQ = \db query [S str,I int,D dou,B bytStr] -> (runSqlite "test.db" $ runMigrationSilent migrateAll >>= \e -> P.insert $ Test (pack str) int dou bytStr) >> return (),
                  prepSelQ = \conn query -> runSqlite "test.db" $ runMigrationSilent migrateAll >> selectList [] [] :: IO [Entity Test],
                  prepDelQ = \conn query -> runSqlite "test.db" $ runMigrationSilent migrateAll >> deleteWhere ([] :: [Filter Test]),
                  prepGetVal = \val -> let ans = entityVal (head val) in [S (unpack $ testStr ans), I (testNum ans), D (testDou ans), B (testBytstr ans)],
                  prepParam = [],

                  static = True}


main1 :: [String] -> [[String]] -> IO ([String],[[String]])
main1 seedList inputs = do
  putStrLn "----------------------------------------sqlite---------------------------------------------"
  res <- quickCheckWithResult stdArgs {maxSuccess = 5000} (prop_create_select_generic_dyn sqliteBindSqlite)
  res <- quickCheckWithResult stdArgs {maxSuccess = 5000} (prop_create_select_generic_prepare_dyn sqliteBindSqlite)
  putStrLn "-------------------------------------------------------------------------------------------"

  putStrLn "----------------------------------------Hdbc---------------------------------------------"
  res <- quickCheckWithResult stdArgs {maxSuccess = 5000} (prop_create_select_generic_dyn sqliteBindHdbc)
  res <- quickCheckWithResult stdArgs {maxSuccess = 5000} (prop_create_select_generic_prepare_dyn sqliteBindHdbc)
  putStrLn "-------------------------------------------------------------------------------------------"

  putStrLn "----------------------------------------Simple---------------------------------------------"
  res <- quickCheckWithResult stdArgs {maxSuccess = 5000} (prop_create_select_generic_dyn sqliteBindSimple)
  res <- quickCheckWithResult stdArgs {maxSuccess = 5000} (prop_create_select_generic_prepare_dyn sqliteBindSimple)
  putStrLn "-------------------------------------------------------------------------------------------"
  
  putStrLn "----------------------------------------SimpLEST---------------------------------------------"
  res <- quickCheckWithResult stdArgs {maxSuccess = 5000} (prop_create_select_generic_dyn sqliteBindSimpLEST)
  res <- quickCheckWithResult stdArgs {maxSuccess = 5000} (prop_create_select_generic_prepare_dyn sqliteBindSimpLEST)
  putStrLn "-------------------------------------------------------------------------------------------"
  
  putStrLn "----------------------------------------Direct---------------------------------------------"
  res <- quickCheckWithResult stdArgs {maxSuccess = 5000} (prop_create_select_generic_dyn sqliteBindDirect)
  res <- quickCheckWithResult stdArgs {maxSuccess = 5000} (prop_create_select_generic_prepare_dyn sqliteBindDirect)
  putStrLn "-------------------------------------------------------------------------------------------"

  putStrLn "----------------------------------------Persist---------------------------------------------"
  res <- quickCheckWithResult stdArgs {maxSuccess = 5000} (prop_create_select_generic_dyn sqliteBindPersist)
  res <- quickCheckWithResult stdArgs {maxSuccess = 5000} (prop_create_select_generic_prepare_dyn sqliteBindPersist)
  putStrLn "-------------------------------------------------------------------------------------------"

  --case res of 
  --        Failure{} -> do 
  --                        putStrLn "failure"
  --                        --putStrLn (output res)
  --                        --return (["fail"],[[""]])
  --                        main1 (seedList ++ [show (usedSeed res) ++ " " ++ show (usedSize res)]) (inputs ++ [(formatOutputs $ output res)])
  --                        return (seedList,inputs)

  --        otherwise ->    return (seedList, inputs)
  return ([],[])

------------------------------------------------------------------

data Vals = I Int | D Double | S String | B T.ByteString | N Int
  deriving Show

data SQLiteBind a b c = 
  SQLiteBind    { openConn    :: String -> IO a
                , closeConn   :: a -> IO ()
                , createQ     :: a -> String -> IO ()
                , insertQ     :: a -> String -> IO ()
                , selectQ     :: a -> String -> IO b
                , deleteQ     :: a -> String -> IO ()
                , getVal      :: b -> [Vals]

                , prepCreateQ :: a -> String -> IO ()
                , prepInsQ    :: a -> String -> [Vals] -> IO ()
                , prepSelQ    :: a -> String -> IO c
                , prepDelQ    :: a -> String -> IO ()
                , prepGetVal  :: c -> [Vals]
                , prepParam   :: [String]

                , static      :: Bool
                }

getValueSqlite :: Value -> Vals
getValueSqlite (Int a) = I (fromIntegral a)
getValueSqlite (Text a) = S a
getValueSqlite (Double a) = D a
getValueSqlite (Blob a) = B a
getValueSqlite (Null) = N 0

getHDBCValue' :: [SqlValue] -> [Vals]
getHDBCValue' [str, int, dou, bytStr] = [S (fromSql str), I (fromSql int), D (fromSql dou), B (fromSql bytStr)]
getHDBCValue' [str, int, dou] = [S (fromSql str), I (fromSql int), D (fromSql dou)]


getSimpleValue' :: SQLData -> Vals
getSimpleValue' (SQLText a) = S (unpack a)
getSimpleValue' (SQLInteger a) = I (fromIntegral a)
getSimpleValue' (S.SQLFloat a) = D a
getSimpleValue' (S.SQLBlob b) =  B b
getSimpleValue' (SQLNull) = N 0

getSimpLESTValue' :: ((TE.Text, Int, Double, T.ByteString), String) -> [Vals]
getSimpLESTValue' ((txt, int, dou, bytStr ), _) = [S (unpack txt), I int, D dou, B bytStr]

getSimpLESTStringValue' :: ((TE.Text, Int, Double), String) -> [Vals]
getSimpLESTStringValue' ((txt, int, dou), _) = [S (unpack txt), I int, D dou]

getDirectStringValue' :: [SQLData] -> [Vals]
getDirectStringValue' [SQLText a,SQLInteger b,D.SQLFloat c] = [S (unpack a), I (fromIntegral b), D c]

getDirectValue' :: [SQLData] -> [Vals]
getDirectValue' [SQLText a,SQLInteger b,D.SQLFloat c, D.SQLBlob d] = [S (unpack a), I (fromIntegral b), D c, B d]

----------------------Simple-------------------------------------------

data StringField = StringField String Int Double deriving (Show)

instance FromRow StringField where
  fromRow = StringField <$> field <*> field <*> field

instance ToRow StringField where
  toRow (StringField str int f) = toRow (str,int,f)

data PrepField = PrepField String Int Double T.ByteString deriving (Show)

instance FromRow PrepField where
  fromRow = PrepField <$> field <*> field <*> field <*> field

instance ToRow PrepField where
  toRow (PrepField str int f bytStr) = toRow (str,int,f, bytStr)


------------------------Ground Hog -----------------------------------------



-------------------------------------------------------------------------------
formatOutputs :: String -> [String]
formatOutputs str = let (x:y:xs) = map unpack $ drop 1 (split (=='\n') (pack $ filter (\x -> x /= '\"') str))
                    in  [(drop 5 x)] ++ [(drop 5 y)] ++ (take (length xs -1) xs)

recRead :: String -> String
recRead [] = []
recRead list = let [(first, second)] =  readLitChar list
               in  [first] ++ recRead second

safeHead :: [String] -> String  
safeHead list = case take 1 list of 
        [] -> ""
        otherwise -> head list

safeHead' :: [SqlValue] -> SqlValue  
safeHead' list = case take 1 list of 
        [] -> SqlNull
        otherwise -> head list

safeHead2' :: [Value] -> Value  
safeHead2' list = case take 1 list of 
        [] -> Null
        otherwise -> head list

safeLast :: [String] -> String
safeLast list = case drop ((length list)-1) list of 
        [] -> ""  
        otherwise -> last list

safeRead :: String -> Int
safeRead list = case list of
        [] -> 0
        otherwise -> read list


safeReadDou :: String -> Double
safeReadDou list = case list of
        [] -> 0.0
        otherwise -> read list

safeInit :: [a] -> [a]
safeInit list = take (length list -1) list

decodeString :: String -> String
decodeString str = unpack $  decodeUtf8 (T.pack $ str)

fromRight' :: Either a b -> b
fromRight' (Right b) = b

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
getValue (Blob a) = T.unpack a
getValue (Null) = ""


getHDBCValue :: SqlValue -> String
getHDBCValue a = fromSql a

getDBTables :: IO [String]
getDBTables = do
    tables <- runDB $ "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;"
    return $ map ((map toLower). snd) tables


isSelect :: String -> Bool
isSelect query = "select" == (map toLower (take 6 query))

runDB :: String -> IO [(String,String)]
runDB str = do
  db  <- openConnection "test.db"
  val <- execStatement db str :: IO (Either String [[Row Value]])
  closeConnection db
  if isRight val then return $ map (\(x,y) -> (x, getValue y)) $ concat $ concat $ fromRight' val else return []

runDBConn :: SQLiteHandle -> String -> IO [(String,String)]
runDBConn db str = do
  val <- execStatement db str :: IO (Either String [[Row Value]])
  if isRight val then return $ map (\(x,y) -> (x, getValue y)) $ concat $ concat $ fromRight' val else return []

runDBVal :: String -> IO [(String,Value)]
runDBVal str = do
  db  <- openConnection "test.db"
  val <- execStatement db str :: IO (Either String [[Row Value]])
  closeConnection db
  if isRight val then return $ map (\(x,y) -> (x, y)) $ concat $ concat $ fromRight' val else return []


runTest sqlQuery = do
  c   <- connectSqlite3 "testHDBC.db"
  runHDBC c "create table c (t text, f text)"
  runHDBC c "insert into c values ('cc','ff')"
  res <- runHDBC c "select * from c"
  print (length res)
  commit c
  disconnect c
  cd   <- connectSqlite3 "testHDBC.db"
  runHDBC cd "drop table c"
  commit cd
  disconnect cd
  return res

runHDBC :: IConnection conn => conn ->  String -> IO [[(String, String)]]
runHDBC c sqlQuery = do
  resHDBC <- case isSelect sqlQuery of 
              True -> do
                select <- prepare c sqlQuery
                execute select []
                fetchAllRowsAL select
              otherwise -> do
                runRaw c sqlQuery 
                return []
  return (map (map (\(x,y) -> (x, getHDBCValue y))) resHDBC)


runHDBC' :: IConnection conn => conn ->  String -> IO [[(String, String)]]
runHDBC' c sqlQuery = do
  c   <- connectSqlite3 "testHDBC.db"
  resHDBC <- case isSelect sqlQuery of 
              True -> do
                select <- prepare c sqlQuery
                execute select []
                fetchAllRowsAL select
              otherwise -> do
                runRaw c sqlQuery 
                return []
  commit c
  disconnect c
  return (map (map (\(x,y) -> (x, getHDBCValue y))) resHDBC)


runDB' :: String -> IO (Maybe String)
runDB' str = do
  --putStrLn str
  db  <- openConnection "test.db"
  val <- execStatement_ db str :: IO (Maybe String)
  --putStrLn (show val)
  closeConnection db
  return val

runDBConn' :: SQLiteHandle -> String -> IO (Maybe String)
runDBConn' db str = do
  val <- execStatement_ db str :: IO (Maybe String)
  return val


foreign import ccall "printf" c_printf :: CString -> IO Int
foreign import ccall "exp" c_exp :: Double -> Double

foreign import ccall "string.h memcpy"
  memcpy2 :: Ptr a -> Ptr a -> CSize -> IO ()

instance Arbitrary B.ByteString where
    arbitrary   = fmap B.pack (listOf1 (arbitrary :: Gen Word8))


newtype ByteStringS = ByteStringS (String, B.ByteString)
 deriving (Show,Eq)

instance Arbitrary ByteStringS where
 arbitrary =  do
   bytStr <- arbitrary :: Gen T.ByteString
   let str = bsToHex bytStr
   return (ByteStringS (str,bytStr))

bsToHex :: B.ByteString -> String
bsToHex bs = concatMap (intToHex . fromIntegral) ( B.unpack bs)
  where intToHex :: Int -> String
        intToHex n = let (qu, re) = n `divMod` 16
                     in  [intToDigit qu, intToDigit re]


test = do
   bytStr <- arbitrary :: Gen T.ByteString
   let str = bsToHex bytStr
   return (ByteStringS (str,bytStr))


newtype DoubleS = DoubleS Double
  deriving (Show,Eq)

genDouble =
  sized $ \n ->
    let n' = toInteger n in
      do a <- choose ((-n') * precision, n' * precision)
         b <- choose (1, precision)
         let dou = fromRational (a % b)
         let dou' = show dou
         let dou2 = if take 1 dou' == "-" then  17 else 16
         let dou3 = if last (take dou2 dou') == 'e' || last (take dou2 dou') == '-' then dou else (read (take dou2 dou') :: Double)
         -- let dou' = if take 1 dou == "-" then (read  (take 17  dou) :: Double) else (read  (take 16  dou) :: Double)
         return (dou3)
 where
  precision = 9999999999999 :: Integer

instance Arbitrary DoubleS where
  arbitrary = do
    l <-  arbitrary
    return (DoubleS l)

newtype IntS = IntS Int
  deriving (Show,Eq)

instance Arbitrary IntS where
  arbitrary = do
    l <- arbitrary
    return (IntS l)

newtype Word = Word String
  deriving (Show,Eq)

instance Arbitrary Word where
  arbitrary = sized $ \s -> do
    let s' = if s == 0  then 1 else s
    l <- stringGen
    return (Word l)

intToHex :: Int -> String
intToHex n = let (qu,re) = n `divMod` 16
             in  [intToDigit qu , intToDigit re]
        

isNotDuplicate :: [String] -> Bool
isNotDuplicate strList = findDup lowerStrList strList
    where lowerStrList = map (map toLower) strList
          findDup [] [] = True
          findDup [x] [y] = x /= y
          findDup (x:xs) (y:ys) | x `elem` xs  = False
                                | otherwise = findDup xs ys

newtype FourWords = FourWords [String]
  deriving (Show,Eq)

instance Arbitrary FourWords where
  arbitrary = sized $ \s -> do
    let s' = if s == 0  then 1 else s
    l <- suchThat (vectorOf 4 stringGen) isNotDuplicate
    return (FourWords l)

newtype ThreeWords = ThreeWords [String]
  deriving (Show,Eq)

instance Arbitrary ThreeWords where
  arbitrary = sized $ \s -> do
    let s' = if s == 0  then 1 else s
    l <- suchThat (vectorOf 3 stringGen) isNotDuplicate
    return (ThreeWords l)

----------------------------------------------------------------------------------------------------------------------------------------------------
      --putStrLn $ insertTable (static bind) table [colStr,colNum,colDou, colBytStr] [str, show num, show dou, "x'"++ hexStr ++ "'"]

prop_create_select_generic_dyn bind (Word table) (FourWords [colStr, colNum, colDou, colBytStr]) (Word str) (IntS num) (DoubleS dou) (ByteStringS (hexStr, bytStr)) = monadicIO $  do
    ans <- run $ do 
      conn  <- (openConn bind) "test.db"
      (createQ bind) conn $ createTable' (static bind) table [colStr, colNum, colDou, colBytStr]
      (insertQ bind) conn $ insertTable (static bind) table [colStr,colNum,colDou, colBytStr] [str, show num, show dou, "x'"++ hexStr ++ "'"]
      ans <- (selectQ bind) conn $ selectTable "*" table ""
      (deleteQ bind) conn $ dropTable table
      (closeConn bind) conn
      return ans
    let [S a, I b,D c,B d] = (getVal bind) ans
    monitor (
      whenFail'
      (putStrLn $ "--------- Database: " ++ " str: " ++ a ++ " expected: " ++ str ++ " num: " ++ show b ++ " expected: " ++ show num ++ " dou: " ++ show c  ++ " expected: " ++ show dou ++" bytStr: " ++ show (B.unpack d) ++ " expected: " ++ show (B.unpack bytStr)))
    assert( a == str && b == num &&  d == bytStr)


prop_create_select_generic_prepare_dyn bind (Word table) (FourWords [colStr, colNum, colDou, colBytStr]) (Word str) (IntS num) (DoubleS dou) bytStr = monadicIO $ do
    ans  <- run $ do
      conn <- (openConn bind) "test.db"
      (prepCreateQ bind) conn $ createTable' (static bind) table [colStr, colNum, colDou, colBytStr]
      (prepInsQ bind) conn (insertTablePrep table [colStr,colNum,colDou,colBytStr] (prepParam bind)) [S str,I num, D dou, B bytStr]
      ans <- (prepSelQ bind) conn $ selectTable "*" table ""
      (prepDelQ bind) conn $ dropTable table
      (closeConn bind) conn
      return ans
    let [S a, I b,D c, B d] = (prepGetVal bind) ans
    monitor (
      whenFail'
      (putStrLn $ "--------- Database: " ++ " str: " ++ a ++ " expected: " ++ str ++ " num: " ++ show b ++ " expected: " ++ show num ++ " dou: " ++ show c  ++ " expected: " ++ show dou  ++ " bytStr: " ++ T.unpack d ++ " expected: " ++ T.unpack bytStr))
    assert( a == str && fromIntegral b ==  num  && c == dou &&  d == bytStr)


----------------------------------------------------------------------------------------------------------------------------------------------------
runDB4dou :: SQLiteHandle -> String -> Double -> IO [(ColumnName, Value)]
runDB4dou db query dou = do
  val <- case  map toLower (take 6 query) of 
            "insert"  -> execParamStatement db query [(":1",Double dou )]
            otherwise -> execStatement db query :: IO (Either String [[Row Value]])
  if isRight val then return $ concat $ concat $ fromRight' val else return []

prop_create_select23 ::  [[String]] -> DoubleS -> Property
prop_create_select23 inputs (DoubleS dou) = monadicIO $  do
    conn   <- run $ connectSqlite3 "test.db"
    createError <- run $ runHDBC conn $ createTable "a" ["dou real"]
    insError <- run $ runHDBC conn $ insertTable' "a" ["dou"] [show dou]
    run $ commit conn
    run $ disconnect conn

    conn  <- run $ openConnection "test.db"
    createError <- run $ runDBConn' conn $ createTable "b" ["dou real"]
    insError <-    run $ runDBConn' conn $ insertTable' "b" ["dou"] [show dou]

    createError <- run $ runDB4dou conn (createTable "c" ["dou real"])  0.0
    insError <- run $ runDB4dou conn (insertTablePrep "c" ["dou"] [":1"]) dou
    
    ans1 <- run $ runDBConn  conn $ selectTable "*" "a" ""
    let x1 = snd $ head ans1
    ans2 <- run $ runDBConn  conn $ selectTable "*" "b" "" 
    let x2 = snd $ head ans2
    ans3 <- run $ runDBConn  conn $ selectTable "*" "c" "" 
    let x3 = snd $ head ans3
    run $ closeConnection conn
    
    conn   <- run $ connectSqlite3 "test.db"
    ans4 <- run $ runHDBC conn $ selectTable "*" "a" ""
    let y1 = snd $ head $ head ans4
    ans5 <- run $ runHDBC conn $ selectTable "*" "b" ""
    let y2 = snd $ head $ head ans5
    ans6 <- run $ runHDBC conn $ selectTable "*" "c" ""
    let y3 = snd $ head $ head ans6

    monitor (
      whenFail'
      (putStrLn $ "--------- HDBC " ++ "   HDBC: " ++ y1 ++ " Sqlite: " ++ y2 ++ " sqlite prep: " ++ y3 ++ " \n--------- Sqlite: HDBC: " ++ x1 ++ " Sqlite: " ++ x2  ++ " Sqlite prep: " ++ x3))
    monitor (
      whenFail'
      (putStrLn $ "Create error: " ++ show createError ++ " insert error: " ++ show insError ))
    assert ( x1 == x2 && x3 == y1  && y2 == y3 && (safeReadDou x1 :: Double) == dou ) 
    
    run $ commit conn
    run $ disconnect conn
    cd   <- run $ connectSqlite3 "test.db"
    run $ runHDBC cd (dropTable "a")
    run $ runHDBC cd (dropTable "b")
    run $ runHDBC cd (dropTable "c")
    run $ commit cd
    run $ disconnect cd


createTable :: String -> [String] -> String
createTable table colNames =  do
  let newColnames = intercalate "," colNames
  "create table " ++ table ++" (" ++  newColnames ++ ");"  

createTable' :: Bool -> String -> [String] -> String
createTable' isStatic table colNames =  do
  let table' = if isStatic then "Test" else table
  let colNames' = map (\(x,y) -> x ++ y) $ colNames `zip` [" text"," integer"," real", " blob"]
  let newColnames = intercalate "," colNames'
  "create table " ++ table' ++" (" ++  newColnames ++ ");"

insertTablePrep :: String -> [String] -> [String] -> String
insertTablePrep table colNames values = do
    let newColnames = intercalate "," colNames
    let val = intercalate "," values
    "insert into " ++ table ++ " (" ++  newColnames ++ ") values (" ++ val ++ " );"

insertTable' :: String -> [String] -> [String] -> String
insertTable' table colNames (values) = do
    let values' = ["'" ++ head values ++ "'"] ++ drop 1 values
    let newColnames = intercalate "," colNames
    let val = intercalate "," values'
    "insert into " ++ table ++ " (" ++  newColnames ++ ") values (" ++ val ++ " );"

insertTable :: Bool -> String -> [String] -> [String] -> String
insertTable isStatic table colNames values = do
    let (extra, colNames', table') = case isStatic of 
                              True -> (["1"],["id", "str","num","dou", "bytStr"], "Test")
                              otherwise -> ([],colNames, table)
    let values' = extra ++ ["'" ++ head values ++ "'"] ++ drop 1 values
    let newColnames = intercalate "," colNames'
    let val = intercalate "," values'
    "insert into " ++ table' ++ " (" ++  newColnames ++ ") values (" ++ val ++ " );"

dropTable :: String -> String
dropTable table = "drop table " ++ table ++ ";"

selectTable :: String -> String -> String -> String
selectTable name table "" = "select " ++ name ++  " from " ++ table ++ ";" 
selectTable name table wher = "select " ++ name ++  " from " ++ table ++ " " ++ wher 

letterGen :: Gen Char
letterGen = suchThat arbitrary isLetter

stringGen :: Gen String
stringGen = suchThat (listOf1 letterGen) (\x -> map toLower x `notElem` forbidden)

removeNLastElements :: Int -> String -> String
removeNLastElements int list = take (length list-int) list