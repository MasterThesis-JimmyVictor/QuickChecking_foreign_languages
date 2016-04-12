import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Process
import Data.Char
import Data.Text (unpack, pack, split)
import System.IO
import System.Directory

import qualified Data.ByteString as TE
import qualified Data.ByteString.Char8 as T
import System.Exit
import Data.Word

--main = callCommand "./a.out"
--main = callCommand "convert img474 img474.jpg"

main :: IO ()
main = runRandomPic 5

runRandomPic :: Int -> IO ()
runRandomPic 0 = putStrLn "-------------No Errors was found------------------"
runRandomPic int = do
    callProcess "rm" ["-rf", "autoGen"]
    callProcess "mkdir" ["autoGen"]
    setCurrentDirectory "./autoGen"
    errCode <- test_createRandomFile
    --test_createPlasmaFile
    setCurrentDirectory "../"
    putStrLn ("Tests Left: " ++ show (int-1))
    let newErrorMsg = filter (\(ExitFailure d) -> d /= 1) errCode
    if (length newErrorMsg) == 0 then runRandomPic (int-1) else print errCode

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
test_createRandomFile :: IO [ExitCode]
test_createRandomFile = do
    callProcess "mkdir" ["random"]
    setCurrentDirectory "./random"
    --quickCheck prop_createRandomFile
    quickCheckWith stdArgs {maxSuccess = 1000} prop_createRandomFile
    errCode <- convertImgs
    setCurrentDirectory "../"
    return errCode

fromHexC :: Integral a => Char -> a
fromHexC c
  | isDigit c = fromIntegral (ord c) - fromIntegral (ord '0')
  | isLower c = fromIntegral (ord c) - fromIntegral (ord 'a') + 10

fromHex :: Integral a => String -> a
fromHex s = foldl (\a c -> fromHexC c + 16 * a) 0 s


pngHeader = [fromHex x | x <- ["89", "50", "4e", "47", "0d", "0a", "1a", "0a"]]

pngHeaderIHDR89 = [fromHex x | x <- ["89", "50", "4e", "47", "0d", "0a", "1a", "0a", "00", "00", "00", "0d", "49", "48", "44", "52", "00", "00"]]

pngEnd = [fromHex x | x <- ["49", "45", "4e", "44", "ae", "42", "60", "82"]]

pngIDATX = [fromHex x | x <- ["49", "44", "41", "54", "78", "da"]]

--pngFullHeader = TE.append pngHeaderIHDR89 pngIDATX

-- 20 -- 160

insertIDAT :: Int -> [Word8] -> T.ByteString
insertIDAT i wordList = let (l1,l2) = splitAt i wordList
               in TE.pack $ l1 ++ pngIDATX ++ l2

prop_createRandomFile :: T.ByteString -> Property
prop_createRandomFile byteStr = forAll (choose (0, 100000)) $ \colElem -> forAll (choose (20, 160)) $ \randIDAT -> monadicIO $ do
    let ints = colElem :: Int
    let filename = "img" ++ show colElem ++ ".png"
    --let byteStr' = TE.append (TE.append pngFullHeader byteStr) pngEnd
    let byteStr' = insertIDAT randIDAT (pngHeader ++ TE.unpack byteStr ++ pngEnd)
    run $ TE.writeFile filename byteStr'
    assert(True)

convertImgs :: IO [ExitCode]
convertImgs = do
    (_,Just hout,_,_) <- createProcess (shell "ls"){std_out = CreatePipe}
    cont <- hGetContents hout
    let imgList = init $ map unpack $ split (=='\n') (pack cont)
    let list = map (\file -> ("convert " ++ file ++ " " ++ file ++".jpg")) imgList
    --l <- mapM (\file ->  createProcess (shell ("./a.out " ++ file ++ " " ++ file ++".jpg"))) imgList
    l <- mapM (\file ->  createProcess (shell ("convert " ++ file ++ " " ++ file ++".jpg"))) imgList
    results <- sequence [ waitForProcess h | (_, _, _, h) <- l]
    return results


--instance Arbitrary T.ByteString where
--    arbitrary   = fmap T.pack arbitrary

instance Arbitrary T.ByteString where
    arbitrary = do 
        size <- choose (10000, 15000)
        fmap T.pack (resize size arbitrary)

