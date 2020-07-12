module FileTools where

import IntTools
import Subleq

import qualified Data.ByteString.Lazy as B
import Data.Int (Int64)
import qualified Data.Vector as V
import Data.Word (Word8)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery len as
  | length segment == len = segment : splitEvery len rest
  | otherwise = [segment]
  where
    (segment, rest) = splitAt len as

separate :: Int -> Int64 -> [Word8]
separate bytes num = zipWith getByte ixs repeated
  where
    repeated = replicate bytes $ toUnsigned' 64 num :: [Integer]
    ixs = [(bytes - 1), (bytes - 2) .. 0]
    getByte ix val = ec $ val `mod` 2 ^ (8 * (ix + 1)) `div` 2 ^ (8 * ix)

combine :: [Word8] -> Int64
combine ws =
  toSigned' (8 * bytes) $ sum $
  zipWith getPart ixs (map ec ws :: [Integer])
  where
    bytes = length ws
    ixs = [(bytes - 1), (bytes - 2) .. 0]
    getPart ix val = val * 2 ^ (8 * ix)

readAP :: FilePath -> IO AssembledProgram
readAP file = do
  (h:t) <- B.unpack <$> B.readFile file :: IO [Word8]
  if h == 8
  then return $ AP 8 $ V.fromList $ map (toSigned' 8) t
  else if h `elem` [16, 32, 64]
  then
    return $ AP (ec h) $ V.fromList $
    map combine $ splitEvery (ec h `div` 8) t
  else do
    putStrLn $ "Unsupported bitness: " ++ show h
    return $ AP 0 V.empty

writeAP :: FilePath -> AssembledProgram -> IO ()
writeAP file (AP bits ap) =
  B.writeFile file $ B.pack $ ec bits :
  concatMap (separate (ec bits `div` 8)) (V.toList ap)
