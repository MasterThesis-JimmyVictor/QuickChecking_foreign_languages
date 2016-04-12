{-# LANGUAGE OverloadedStrings #-}
module Hello where

import qualified Data.Text as T
import qualified CPython as Py
import qualified CPython.Constants as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types as Py
import qualified CPython.Types.Module as Py
import qualified CPython.Types.Tuple as PyT
import System.IO (stdout)
import qualified Data.ByteString.Char8 as B
import qualified CPython.Types.Exception as Py
import qualified Control.Exception as E
import qualified CPython.Types.Dictionary as Py

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Modifiers

import System.Process
import System.IO
import System.Directory
import Data.Char

mapList :: [[[Double]]] -> IO Py.List
mapList list =  do
    floats <- mapM3 Py.toFloat list
    pyToList =<< (sequence $ [  pyToList  =<< (sequence [ pyToList lvl1 | lvl1 <- lvl2] )| lvl2 <- floats])
  where mapM3 f list = mapM (mapM (mapM f)) list
        pyToList list = Py.toList $ map Py.toObject list

douListGen :: Gen [Double]
douListGen = do
    n1 <- choose(0,255)
    n2 <- choose(0,255)
    n3 <- choose(0,255)
    return [n1,n2,n3]

letterGen :: Gen Char
letterGen = suchThat arbitrary isLetter

newtype Word = Word String
  deriving (Show,Eq)

instance Arbitrary Word where
  arbitrary = do
    s <- choose(1,20)
    l <- vectorOf s letterGen
    return (Word l)

newtype ImageArray = ImageArray [[[Double]]] deriving (Eq,Show)

--instance Arbitrary ImageArray where
--  arbitrary = sized $ \s -> do
--    let s' = if s == 0  then 1 else s
--    list <- listOf1 $ vectorOf s' douListGen
--    --list <- listOf1 $ listOf1 douListGen
--    return (ImageArray list)

instance Arbitrary ImageArray where
  arbitrary = do
    s <- choose(1,40)
    list <- listOf1 $ vectorOf s douListGen
    --list <- listOf1 $ listOf1 douListGen
    return (ImageArray list)



--main :: IO ()
--main = do
--   quickCheckWith stdArgs {maxSuccess = 1} prop_randomPic

main :: IO ()
main = runRandomPic 5

runRandomPic :: Int -> IO ()
runRandomPic 0 = return ()
runRandomPic int = do
    callProcess "rm" ["-rf", "randomPic"]
    callProcess "mkdir" ["randomPic"]
    setCurrentDirectory "./randomPic"
    --quickCheck prop_randomPic
    quickCheckWith stdArgs {maxSuccess = 1000} prop_randomPic
    convertImgsError
    setCurrentDirectory "../"
    --callProcess "rm" ["-rf", "randomPic"]
    if int `mod` 10 == 0 then putStrLn ("Tests Left: " ++ show (int-1)) else return ()
    runRandomPic (int-1)


prop_randomPic :: Word -> ImageArray -> Property
prop_randomPic (Word fileName) (ImageArray imageArray) = monadicIO $ do
--    E.handle onException $ do

    run $ Py.initialize
    list <- run $mapList imageArray
    numpy <- run $Py.importModule "numpy"
    array <- run $Py.getAttribute numpy =<< Py.toUnicode "array"
    imArray <- run $Py.callArgs array [Py.toObject list]

    astype <- run $Py.getAttribute imArray =<< Py.toUnicode "astype"
    uint8 <- run $Py.toUnicode "uint8"
    imArray <- run $Py.callArgs astype [Py.toObject uint8]


    image <- run $Py.importModule "PIL.Image"
    fromarray <- run $Py.getAttribute image =<< Py.toUnicode "fromarray"
    im <- run $Py.callArgs fromarray [Py.toObject imArray]

    convert <- run $Py.getAttribute im =<< Py.toUnicode "convert"
    rgba <- run $Py.toUnicode "RGBA"
    im' <- run $Py.callArgs convert [Py.toObject rgba]

    save <- run $Py.getAttribute im' =<< Py.toUnicode "save"
    savename <- run $Py.toUnicode (T.pack (fileName ++ "f.png"))
    im' <- run $Py.callArgs save [Py.toObject savename]
    assert(True)

convertImgs :: IO ()
convertImgs = do
    (_,Just hout,_,_) <- createProcess (shell "ls"){std_out = CreatePipe}
    cont <- hGetContents hout
    let imgList = init $ map T.unpack $ T.split (=='\n') (T.pack cont)
    let list = map (\file -> ("convert " ++ file ++ " " ++ file ++".jpg")) imgList
    mapM (\file ->  createProcess (shell ("convert " ++ file ++ " " ++ file ++".jpg"))) imgList
    return ()

convertImgsError :: IO ()
convertImgsError = do
    (_,Just hout,_,_) <- createProcess (shell "ls"){std_out = CreatePipe}
    cont <- hGetContents hout
    let imgList = init $ map T.unpack $ T.split (=='\n') (T.pack cont)
    let list = map (\file -> ("convert " ++ file ++ " " ++ file ++".jpg")) imgList
    mapM (\file ->  callCommand ("convert " ++ file ++ " " ++ file ++".jpg")) imgList
    return ()

onException :: Py.Exception -> IO ()
onException exc = do
    tb <- case Py.exceptionTraceback exc of
        Just obj -> return obj
        Nothing -> Py.none
    mod <- Py.importModule "traceback"
    proc <- Py.getAttribute mod =<< Py.toUnicode "print_exception"
    Py.callArgs proc [Py.exceptionType exc, Py.exceptionValue exc, tb]
    return ()