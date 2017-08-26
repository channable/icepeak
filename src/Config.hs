module Config (
  Config (..),
  configInfo
) where

import Data.Semigroup ((<>))
import Options.Applicative (Parser, ParserInfo, fullDesc, header, help,
  helper, info, long, metavar, strOption, value, (<**>))

-- command-line arguments
data Config = Config {
    cDataFile :: FilePath
}

-- Parsing of command-line arguments

configParser :: Parser Config
configParser =
  Config
    <$> strOption (long "data-file" <>
                   metavar "DATA_FILE" <>
                   value "icepeak.json" <>
                   help "File where data is persisted to. Default: icepeak.json")

configInfo :: ParserInfo Config
configInfo = info parser description
  where
    parser = configParser <**> helper
    description = fullDesc <>
      header "Icepeak - Fast Json document store with push notification support."
