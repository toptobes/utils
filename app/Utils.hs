{-# LANGUAGE QuasiQuotes #-}

module Utils where

import System.Directory
import Data.Text qualified as T
import Data.Time.Clock
import Data.Time.Calendar
import Text.RawString.QQ

(.-) :: (a -> b) -> (b -> c) -> a -> c
f .- g = g . f

readTemplate :: Text -> IO Text
readTemplate = templatePath >=> (toString .- readFileBS <&> fmap decodeUtf8)

templatePath :: Text -> IO Text
templatePath rel = do
  home <- getHomeDirectory
  performSanityCheck home
  pure $ toText home <> "/projects/my-utils/templates/" <> rel

performSanityCheck :: FilePath -> IO ()
performSanityCheck home = do
  unlessM (doesDirectoryExist $ home <> "/projects/my-utils/templates/") $
    fail "You deleted '~/projects/my-utils/templates/', you idiot"

var :: Text -> Text -> Text -> Text
var name = T.replace ("${{" <> name <> "}}")

date :: IO (Integer, Int, Int)
date = getCurrentTime <&> utctDay .- toGregorian

year :: IO Integer
year = (\(y, _, _) -> y) <$> date

fromWinHome :: Text -> Text
fromWinHome path = [r|$(wslpath "$(wslvar USERPROFILE)")/|] <> path
