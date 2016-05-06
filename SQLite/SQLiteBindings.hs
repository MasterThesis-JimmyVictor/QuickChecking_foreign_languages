{-# LANGUAGE OverloadedStrings #-}

-- Database SQLite
import Database.SQLite (openConnection,execStatement_,execStatement,Row(..),Value(..))

-- Database HDBC
--import Control.Exception
--import qualified Database.HDBC as H
--import Database.HDBC.Sqlite3 (connectSqlite3)

-- Database SQLite-Simple
import           Control.Applicative
import qualified Data.Text as T
import qualified Database.SQLite.Simple as S 
import           Database.SQLite.Simple (open,query_,close,executeNamed,lastInsertRowId,NamedParam(..),ToRow(..),Only(..))
import           Database.SQLite.Simple.FromRow

data TestField = TestField Int T.Text deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

instance ToRow TestField where
  toRow (TestField id_ str) = toRow (id_, str)

main :: IO ()
main = do
  conn <- open "test.db"
  S.execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
  S.execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
  S.execute conn "INSERT INTO test (id, str) VALUES (?,?)" (TestField 13 "test string 3")
  rowId <- lastInsertRowId conn
  executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowId]
  r <- S.query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
  S.execute conn "DELETE FROM test WHERE id = ?" (Only rowId)
  close conn

 -- c <- connectSqlite3 "testHDBC.db"
 -- runRaw c "CREATE TABLE IF NOT EXISTS testtable (id INTEGER PRIMARY KEY, str TEXT)"
 -- runRaw c "INSERT INTO testtable values ('40','a');"
 -- select <- prepare c "SELECT * FROM testtable;"
 -- execute select []
 -- result <- fetchAllRows select
 -- putStrLn $ show result
 -- commit c
 -- disconnect c

  db  <- openConnection "testSQLite.db"
  execStatement_ db "create table test (str text);" :: IO(Maybe String)
  execStatement_ db ("insert into test (str) values ('z');") :: IO(Maybe String)     
  table <- execStatement db "SELECT * FROM test;"  :: IO (Either String [[Row Value]])

  let value =  snd $ head $  concat $ concat $ fromRight table
  putStrLn $ getValue value
  putStrLn $ show $ getValue value
 where fromRight (Right b) = b
       getValue (Text a) = a