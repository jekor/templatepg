-- Copyright 2010, 2011 Chris Forno

-- |All type conversion to and from the PostgreSQL server is handled here.

module Database.TemplatePG.Types ( PGType(..)
                                 , pgTypeFromOID
                                 , pgStringToType
                                 , pgTypeToString
                                 ) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Language.Haskell.TH
import Locale
import Text.ParserCombinators.Parsec
import Text.Regex

-- |TemplatePG currenly only supports a handful of types. It also doesn't
-- distinguish between numeric types with different ranges. More types are the
-- most likely feature of future TemplatePG releases.
data PGType = PGBoolean      -- ^ bool
            | PGInteger      -- ^ integer
            | PGReal         -- ^ float
            | PGText         -- ^ text/varchar
            | PGTimestampTZ  -- ^ timestamptz (timestamp with time zone)
            | PGDate         -- ^ date (day without time)
            | PGInterval     -- ^ interval (a time interval), send-only
  deriving (Eq, Show)

-- |Convert a type OID from PostgreSQL's catalog to a TemplatePG
-- representation. To get a list of types: @SELECT typname, oid FROM pg_type@
-- Note that I have assumed, but not tested, that type OIDs for these basic
-- types are consistent across installations. If not, I'm going to have to
-- switch to using the text descriptions
pgTypeFromOID :: Int    -- ^ PostgreSQL type OID
              -> PGType
pgTypeFromOID 16   = PGBoolean     -- bool
-- treating all ints alike for now
pgTypeFromOID 20   = PGInteger     -- int8
pgTypeFromOID 21   = PGInteger     -- int2
pgTypeFromOID 23   = PGInteger     -- int4
pgTypeFromOID 25   = PGText        -- text
-- as with ints, sacrificing precision/safety for floats
pgTypeFromOID 700  = PGReal        -- float4
pgTypeFromOID 701  = PGReal        -- float8
-- I don't currently treat varchars differently from text. It would make sense
-- to do so if I could enforce length limits at compile time.
pgTypeFromOID 1043 = PGText        -- varchar
pgTypeFromOID 1082 = PGDate        -- date
pgTypeFromOID 1184 = PGTimestampTZ -- timestamptz
pgTypeFromOID 1186 = PGInterval    -- interval
pgTypeFromOID n    = error $ "Unknown PostgreSQL type: " ++ show n

-- |This is PostgreSQL's canonical timestamp format.
-- Time conversions are complicated a bit because PostgreSQL doesn't support
-- timezones with minute parts, and Haskell only supports timezones with
-- minutes parts. We'll need to truncate and pad timestamp strings accordingly.
-- This means with minute parts will not work.
pgTimestampTZFormat :: String
pgTimestampTZFormat = "%F %T%z"

-- |Convert a Haskell value to a string of the given PostgreSQL type. Or, more
-- accurately, given a PostgreSQL type, create a function for converting
-- compatible Haskell values into a string of that type.
-- @pgTypeToString :: PGType -> (? -> String)@
pgTypeToString :: PGType -> Q Exp
pgTypeToString PGInteger     = [| show::(Integral a => a -> String) |]
pgTypeToString PGReal        = [| show::(Real a => a -> String) |]
pgTypeToString PGText        = [| escapeString |]
pgTypeToString PGBoolean     = [| (\ b -> if b then "'t'" else "'f'") |]
pgTypeToString PGTimestampTZ = [| \t -> let ts = formatTime defaultTimeLocale pgTimestampTZFormat t in
                                        "TIMESTAMP WITH TIME ZONE '" ++
                                        (take (length ts - 2) ts) ++ "'" |]
pgTypeToString PGDate        = [| \d -> "'" ++ showGregorian d ++ "'" |]
pgTypeToString PGInterval    = [| \s -> "'" ++ show (s::DiffTime) ++ "'" |]

-- |Convert a string from PostgreSQL of the given type into an appropriate
-- Haskell value. Or, more accurately, given a PostgreSQL type, create a
-- function for converting a string of that type into a compatible Haskell
-- value.
-- @pgStringToType :: PGType -> (String -> ?)@
pgStringToType :: PGType -> Q Exp
-- TODO: Is reading to any integral type too unsafe to justify the convenience?
pgStringToType PGInteger     = [| read::((Read a, Integral a) => String -> a) |]
pgStringToType PGReal        = [| read::(String -> Float) |]
pgStringToType PGText        = [| id |]
pgStringToType PGBoolean     = [| \s -> case s of
                                          "t" -> True
                                          "f" -> False
                                          _   -> error "unrecognized boolean type from PostgreSQL" |]
pgStringToType PGTimestampTZ = [| \t -> readTime defaultTimeLocale pgTimestampTZFormat (t ++ "00") |]
pgStringToType PGDate        = [| parseGregorian |]
pgStringToType PGInterval    = error "Reading PostgreSQL intervals isn't supported (yet)."

-- |Make a string safe for interpolation (escape single-quotes). This relies on
-- standard_conforming_strings = on in postgresql.conf. I'm not 100% sure that
-- this makes all strings safe for execution. I don't know if it's possible to
-- inject SQL with strange (possibly Unicode) characters.
escapeString :: String -> String
escapeString s = "'" ++ (subRegex (mkRegex "'") s "''") ++ "'"

parseGregorian :: String -> Day
parseGregorian date = case (parse gregorianDate "" date) of
                        Left err        -> error (show err)
                        Right (y, m, d) -> case fromGregorianValid y m d of
                                             Nothing  -> error $ "Invalid date " ++ date
                                             Just day -> day

gregorianDate :: Parser (Integer, Int, Int)
gregorianDate = do
  year <- count 4 digit
  _ <- char '-'
  month <- count 2 digit
  _ <- char '-'
  day <- count 2 digit
  return (read year, read month, read day)