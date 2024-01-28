{-# LANGUAGE QuasiQuotes #-}

module Utils where

import System.Directory
import Data.Text qualified as T
import Data.Time.Clock
import Data.Time.Calendar
import Text.RawString.QQ
import Options.Applicative
import Data.List
import GHC.IO
import Data.Text.IO

(.-) :: (a -> b) -> (b -> c) -> a -> c
f .- g = g . f

-- readTemplate :: Text -> IO Text
-- readTemplate = templatePath >=> (toString .- readFileBS <&> fmap decodeUtf8)

-- templatePath :: Text -> IO Text
-- templatePath rel = xdgConfigPath <&> (<> ("templates/" <> rel))

configPath :: Text -> IO Text
configPath rel = xdgConfigPath <&> (<> rel)

configFile :: Text -> IO Text
configFile = configPath >=> (toString .- readFileBS <&> fmap decodeUtf8)

xdgConfigPath :: IO Text
xdgConfigPath = do
  dir <- getXdgDirectory XdgConfig "./toptobes-utils/"
  
  unlessM (doesDirectoryExist dir) $
    panik "run 'ut sync --init' first..."

  pure $ toText dir

var :: Text -> Text -> Text -> Text
var name = T.replace ("${{" <> name <> "}}")

date :: IO (Integer, Int, Int)
date = getCurrentTime <&> utctDay .- toGregorian

year :: IO Integer
year = (\(y, _, _) -> y) <$> date

fromWinHome :: Text -> Text
fromWinHome path = [r|"$(wslpath "$(wslvar USERPROFILE)")/|] <> path <> "\""

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
