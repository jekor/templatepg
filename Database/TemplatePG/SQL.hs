-- Copyright 2010, 2011 Chris Forno

-- |This module exposes the high-level Template Haskell interface for querying
-- and manipulating the PostgreSQL server.
-- 
-- All SQL string arguments support expression interpolation. Just enclose your
-- expression in @{}@ in the SQL string.
-- 
-- Note that transactions are messy and untested. Attempt to use them at your
-- own risk.

module Database.TemplatePG.SQL ( queryTuples
                               , queryTuple
                               , execute
                               , insertIgnore
                               , withTransaction
                               , rollback
                               ) where

import Database.TemplatePG.Protocol
import Database.TemplatePG.Types

import Control.Exception
import Control.Monad
import Data.ByteString.Lazy.UTF8 hiding (length, decode, take, foldr)
import Data.Maybe
import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (returnQ)
import Network
import System.Environment
import System.IO
import System.IO.Error (isDoesNotExistError)
import Text.ParserCombinators.Parsec

import Prelude hiding (catch, exp)

-- |Grab a PostgreSQL connection for compile time. We do so through the
-- environment variables: @TPG_DB@, @TPG_HOST@, @TPG_PORT@, @TPG_USER@, and
-- @TPG_PASS@. Only TPG_DB is required.
thConnection :: IO Handle
thConnection = do
  database <- getEnv "TPG_DB"
  hostName <- catchUndef (getEnv "TPG_HOST") (\ _ -> return "localhost")
  portNum  <- catchUndef (getEnv "TPG_PORT") (\ _ -> return "5432")
  username <- catchUndef (getEnv "TPG_USER") (\ _ -> return "postgres")
  password <- catchUndef (getEnv "TPG_PASS") (\ _ -> return "")
  let portNum' = PortNumber $ fromIntegral $ ((read portNum)::Integer)
  pgConnect hostName portNum' database username password
 where catchUndef = catchJust (\e -> if isDoesNotExistError e
                                       then Just ()
                                       else Nothing)

-- |This is where most of the magic happens.
-- This doesn't result in a PostgreSQL prepared statement, it just creates one
-- to do type inference.
-- This returns a prepared SQL string with all values (as an expression)
prepareSQL :: String -- ^ a SQL string, with
           -> Q (Exp, [(String, PGType, Bool)]) -- ^ a prepared SQL string and result descriptions
prepareSQL sql = do
  -- TODO: It's a bit silly to establish a connection for every query to be
  -- analyzed.
  h <- runIO thConnection
  let (sqlStrings, expStrings) = parseSql sql  
  (pTypes, fTypes) <- runIO $ describeStatement h $ holdPlaces sqlStrings expStrings
  s <- weaveString sqlStrings =<< zipWithM stringify pTypes expStrings
  return (s, fTypes)
 where holdPlaces ss es = concat $ weave ss (take (length es) placeholders)
       placeholders = map (('$' :) . show) ([1..]::[Integer])
       stringify typ s = [| $(pgTypeToString typ) $(returnQ $ parseExp' s) |]
       parseExp' e = (either (\ _ -> error ("Failed to parse expression: " ++ e)) id) $ parseExp e

-- |"weave" 2 lists of equal length into a single list.
weave :: [a] -> [a] -> [a]
weave x []          = x
weave [] y          = y
weave (x:xs) (y:ys) = x:y:(weave xs ys)

-- |"weave" a list of SQL fragements an Haskell expressions into a single SQL string.
weaveString :: [String] -- ^ SQL fragments
            -> [Exp]    -- ^ Haskell expressions
            -> Q Exp
weaveString [x]    []     = [| x |]
weaveString []     [y]    = returnQ y
weaveString (x:[]) (y:[]) = [| x ++ $(returnQ y) |]
weaveString (x:xs) (y:ys) = [| x ++ $(returnQ y) ++ $(weaveString xs ys) |]
weaveString _      _      = error "Weave mismatch (possible parse problem)"

-- |@queryTuples :: String -> (Handle -> IO [(column1, column2, ...)])@
-- 
-- Query a PostgreSQL server and return the results as a list of tuples.
-- 
-- Example (where @h@ is a handle from 'pgConnect'):
-- 
-- @$(queryTuples \"SELECT usesysid, usename FROM pg_user\") h@
-- 
-- @=> IO [(Maybe String, Maybe Integer)]@
queryTuples :: String -> Q Exp
queryTuples sql = do
  (sql', types) <- prepareSQL sql
  [| \h' -> do rs <- executeSimpleQuery h' $(returnQ sql')
               return $ map $(convertRow types) rs |]

-- |@queryTuple :: String -> (Handle -> IO (Maybe (column1, column2, ...)))@
-- 
-- Convenience function to query a PostgreSQL server and return the first
-- result as a tuple. If the query produces no results, return 'Nothing'.
-- 
-- Example (where @h@ is a handle from 'pgConnect'):
-- 
-- @let sysid = 10::Integer;@
-- 
-- @$(queryTuple \"SELECT usesysid, usename FROM pg_user WHERE usesysid = {sysid}\") h@
-- 
-- @=> IO (Maybe (Maybe String, Maybe Integer))@
queryTuple :: String -> Q Exp
queryTuple sql = [| \h' -> do ts <- $(queryTuples sql) h'
                              case ts of
                                []    -> return Nothing
                                (t:_) -> return $ Just t |]

-- |@execute :: String -> (Handle -> IO ())@
-- 
-- Convenience function to execute a statement on the PostgreSQL server.
-- 
-- Example (where @h@ is a handle from 'pgConnect'):
-- 
-- @let rolename = \"BOfH\"@
-- 
-- @$(execute \"CREATE ROLE {rolename}\") h@
-- 
-- @=> IO ()@
execute :: String -> Q Exp
execute sql = do
  (sql', types) <- prepareSQL sql
  case types of
    [] -> [| \h' -> executeSimpleStatement h' $(returnQ sql') |]
    _  -> error "Execute can't be used on queries, only statements."

-- |Run a sequence of IO actions (presumably SQL statements) wrapped in a
-- transaction. Unfortunately you're restricted to using this in the 'IO'
-- Monad for now due to the use of 'onException'. I'm debating adding a
-- 'MonadPeelIO' version. Untested.
withTransaction :: Handle -> IO a -> IO a
withTransaction h a =
  onException (do executeSimpleStatement h "BEGIN"
                  c <- a
                  executeSimpleStatement h "COMMIT"
                  return c)
              (executeSimpleStatement h "ROLLBACK")

-- |Roll back a transaction. Untested.
rollback :: Handle -> IO ()
rollback h = executeSimpleStatement h "ROLLBACK"

-- |Run an INSERT statement, ignoring duplicate key errors. This is also
-- limited to the 'IO' Monad. Untested.
insertIgnore :: IO () -> IO ()
insertIgnore q = catchJust uniquenessError q (\ _ -> return ())
 where uniquenessError e = case e of
                             (PGException c _) -> case c of
                                                    "23505" -> Just e
                                                    _       -> Nothing

-- |Given a result description, create a function to convert a result to a
-- tuple.
convertRow :: [(String, PGType, Bool)] -- ^ result description
           -> Q Exp
convertRow types = [| (\s -> $(tupE $ map (convertColumn 's) $ zip types [0..])) |]

-- |Given a raw PostgreSQL result and a result field type, convert the
-- appropriate field to a Haskell value.
convertColumn :: Name  -- ^ the name of the variable containing the result list (of 'Maybe' 'ByteString')
              -> ((String, PGType, Bool), Int) -- ^ the result field type and index
              -> Q Exp
convertColumn name ((_, typ, nullable), i) = [| $(pgStringToType' typ nullable) ($(varE name) !! i) |]

-- |Like 'pgStringToType', but deal with possible @NULL@s. If the boolean
-- argument is 'False', that means that we know that the value is not nullable
-- and we can use 'fromJust' to keep the code simple. If it's 'True', then we
-- don't know if the value is nullable and must return a 'Maybe' value in case
-- it is.
pgStringToType' :: PGType
                -> Bool  -- ^ nullability indicator
                -> Q Exp
pgStringToType' t False = [| ($(pgStringToType t)) . toString . fromJust |]
pgStringToType' t True  = [| liftM (($(pgStringToType t)) . toString) |]

-- SQL Parser --

-- |Given a SQL string return a list of SQL parts and expression parts.
-- For example: @\"SELECT * FROM table WHERE id = {someID} AND age > {baseAge * 1.5}\"@
-- becomes: @(["SELECT * FROM table WHERE id = ", " AND age > "],
--            ["someID", "baseAge * 1.5"])@
parseSql :: String -> ([String], [String])
parseSql sql = case (parse sqlStatement "" sql) of
                 Left err -> error (show err)
                 Right ss -> every2nd ss

every2nd :: [a] -> ([a], [a])
every2nd = foldr (\a ~(x,y) -> (a:y,x)) ([],[])

sqlStatement :: Parser [String]
sqlStatement = many1 $ choice [sqlText, sqlParameter]

sqlText :: Parser String
sqlText = many1 (noneOf "{")

-- |Parameters are enclosed in @{}@ and can be any Haskell expression supported
-- by haskell-src-meta.
sqlParameter :: Parser String
sqlParameter = between (char '{') (char '}') $ many1 (noneOf "}")