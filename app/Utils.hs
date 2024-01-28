module Utils where

import Data.Text qualified as T

import Options.Applicative
import Data.List
import GHC.IO
import Data.Text.IO

(.-) :: (a -> b) -> (b -> c) -> a -> c
f .- g = g . f

var :: Text -> Text -> Text -> Text
var name = T.replace ("${{" <> name <> "}}")

mkCommand :: String -> String -> Parser a -> Mod CommandFields a
mkCommand name desc p = command name $ info
  (helper <*> p)
  (fullDesc <> progDesc desc)

posArg :: String -> Parser Text
posArg = strArgument . metavar

formatKVs :: Text -> [(Text, Text)] -> Text
formatKVs pre kvs = case length kvs of
  0 -> ""
  _ -> (pre <>) $ T.intercalate ("\n" <> pre) $ map (formatKV pad) kvs
  where
    pad = maximum (map (T.length . fst) kvs)

formatKV :: Int -> (Text, Text) -> Text
formatKV pad = uncurry (<>) . second lpad . \p@(k, _) -> (k, p)
  where
    lpad (key, val) = T.replicate (pad - T.length key) " " <> "     " <> val

-- Poor kitten...
panik :: Text -> a
panik msg = unsafePerformIO (hPutStrLn stderr msg >> exitFailure)
{-# NOINLINE panik #-}
