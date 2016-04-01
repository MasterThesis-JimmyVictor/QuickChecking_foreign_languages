import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Process
import Data.Text (unpack, pack, split)
import System.IO
import System.Directory

import qualified Data.ByteString as TE
import qualified Data.ByteString.Char8 as T

main :: IO ()
main = do
    callProcess "rm" ["-rf", "autoGen"]
    callProcess "mkdir" ["autoGen"]
    setCurrentDirectory "./autoGen"
    test_createRandomFile
    test_createPlasmaFile
    setCurrentDirectory "../"

-- | Tests to convert a random generated jpg to png
test_createPlasmaFile :: IO ()
test_createPlasmaFile = do
    callProcess "mkdir" ["plasma"]
    setCurrentDirectory "./plasma"
    sequence $ map createPlasmaFile [1..100]
    sequence $ map convertPlasmaFile [1..100]
    setCurrentDirectory "../"
    return ()

createPlasmaFile :: Int -> IO ()
createPlasmaFile num = do
    let newfile = "test" ++ show num
    createProcess (shell ("convert -size 100x100 plasma: " ++ newfile ++ ".jpg") )
    return ()

convertPlasmaFile :: Int -> IO ()
convertPlasmaFile num = do
    let newfile = "test" ++ show num
    createProcess (shell ("convert " ++ newfile ++ ".jpg " ++ newfile ++".png"))
    return ()

-- | Tests to convert a random binary file into a jpg
test_createRandomFile :: IO ()
test_createRandomFile = do
    callProcess "mkdir" ["random"]
    setCurrentDirectory "./random"
    quickCheck prop_createRandomFile
    convertImgs
    setCurrentDirectory "../"

prop_createRandomFile :: T.ByteString -> Property
prop_createRandomFile byteStr = forAll (choose (0, 100000)) $ \colElem -> monadicIO $ do
    let ints = colElem :: Int
    let filename = "img" ++ show colElem
    run $ TE.writeFile filename byteStr
    assert(True)

convertImgs :: IO ()
convertImgs = do
    (_,Just hout,_,_) <- createProcess (shell "ls"){std_out = CreatePipe}
    cont <- hGetContents hout
    let imgList = init $ map unpack $ split (=='\n') (pack cont)
    let list = map (\file -> ("convert " ++ file ++ " " ++ file ++".jpg")) imgList
    mapM (\file ->  createProcess (shell ("convert " ++ file ++ " " ++ file ++".jpg"))) imgList
    return ()


instance Arbitrary T.ByteString where
    arbitrary   = fmap T.pack arbitrary

