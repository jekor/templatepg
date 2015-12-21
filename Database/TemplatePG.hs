module Database.TemplatePG (-- *Introduction
                            -- $intro

                            -- *Usage
                            -- $usage

                            -- **Compile-Time Parameters
                            -- $compiletime

                            -- *Caveats
                            -- $caveats

                            -- *Limitations and Workarounds
                            -- **A Note About NULL
                            -- $nulls

                            -- **Tips
                            -- $tips

                            -- **Other Workarounds
                            -- $other
                             PGException(..)
                           , pgConnect
                           , pgDisconnect
                           , queryTuples
                           , queryTuple
                           , execute
                           , withTransaction
                           , rollback
                           , insertIgnore ) where

import Database.TemplatePG.Protocol
import Database.TemplatePG.SQL

-- $intro
-- TemplatePG is designed with 2 goals in mind: safety and performance. The
-- primary focus is on safety.
--
-- To help ensure safety, it uses the PostgreSQL server to parse every query
-- and statement in your code to infer types at compile-time. This means that
-- in theory you cannot get a syntax error at runtime. Getting proper types at
-- compile time has the nice side-effect that it eliminates run-time type
-- casting and usually results in less code. This approach was inspired by
-- MetaHDBC (<http://haskell.org/haskellwiki/MetaHDBC>) and PG'OCaml
-- (<http://pgocaml.forge.ocamlcore.org/>).
--
-- While compile-time query analysis eliminates many errors, it doesn't
-- eliminate all of them. If you modify the database without recompilation or
-- have an error in a trigger or function, for example, you can still trigger a
-- 'PGException'.
--
-- With that in mind, TemplatePG currently does a number of unsafe things: it
-- doesn't handle unexpected messages from the server very gracefully, and it's
-- not entirely safe when working with nullable result fields. I hope to fix
-- all of these at some point in the future. In the meantime, use the software
-- at your own risk. Note however that TemplatePG is currently powering
-- <http://www.vocabulink.com/> with no problems yet. (For usage examples, you
-- can see the Vocabulink source code at <https://github.com/jekor/vocabulink>).
--
-- To improve performance, TemplatePG does not use prepared statements. In
-- theory, this saves bandwidth (and a potential round-trip) and time for the
-- extra step of binding parameters. Again in theory, this is also safe because
-- we know the types of parameters at compile time. However, it still feels
-- risky (and I would appreciate any audit of the code doing this, especially
-- 'escapeString').

-- $usage
-- 'queryTuples' does all the work ('queryTuple' and 'execute' are convenience
-- functions).
--
-- It's a Template Haskell function, so you need to splice it into your program
-- with @$()@. It requires a 'Handle' to a PostgreSQL server, but can't be
-- given one at compile-time, so you need to pass it after the splice:
--
-- @h <- pgConnect ...
-- 
-- tuples <- $(queryTuples \"SELECT * FROM pg_database\") h
-- @
--
-- To pass parameters to a query, include them in the string with {}. Most
-- Haskell expressions should work. For example:
--
-- @let owner = 33
-- 
-- tuples <- $(queryTuples \"SELECT * FROM pg_database WHERE datdba = {owner} LIMIT {2 * 3}\") h
-- @
--
-- Note that parameters may only be used where PostgreSQL will allow them. This
-- will not work:
--
-- @tuples <- $(queryTuples \"SELECT * FROM {tableName}\") h@
--
-- And in general, you cannot construct queries at run-time, since they
-- wouldn't be available to be analyzed at compile time.

-- $compiletime
-- TemplatePG needs information about the database to connect to at compile
-- time (in the form of environment variables).
-- 
-- You must set at least @TPG_DB@:
-- 
-- [@TPG_DB@] the database name to use
-- 
-- [@TPG_USER@] the username to connect as (default: @postgres@)
-- 
-- [@TPG_PASS@] the password to use (default: /empty/)
-- 
-- [@TPG_HOST@] the host to connect to (default: @localhost@)
-- 
-- [@TPG_PORT@] the port number to connect on (default: @5432@)
-- 
-- You can set @TPG_DEBUG@ to get a rough protocol-level trace (pipe to
-- @hexdump@).

-- $caveats
-- TemplatePG assumes that it has a UTF-8 connection to a UTF-8 database.
--
-- TemplatePG does not bind parameters with prepared statements (at run-time),
-- instead it relies on its own type conversion and string escaping. The
-- technique might have a security vulnerability. You should also set
-- @standard_conforming_strings = on@ in your @postgresql.conf@.
-- 
-- I've included 'withTransaction', 'rollback', and 'insertIgnore', but they've
-- not been thoroughly tested, so use them at your own risk.

-- $nulls
-- Sometimes TemplatePG cannot determine whether or not a result field can
-- potentially be @NULL@. In those cases it will assume that it can. Basically,
-- any time a result field is not immediately tracable to an originating table
-- and column (such as when a function is applied to a result column), it's
-- assumed to be nullable and will be returned as a 'Maybe' value.
--
-- Additionally, you cannot directly use @NULL@ values in parameters. As a
-- workaround, you might have to use 2 or more separate queries (and @DEFAULT
-- NULL@) to @INSERT@ rows with @NULL@s.
--
-- Nullability is indicated incorrectly in the case of outer joins. TemplatePG
-- incorrectly infers that a field cannot be @NULL@ when it's able to trace the
-- result field back to a non-@NULL@ table column. As a workround, you can wrap
-- columns with @COALESCE()@ to force them to be returned as 'Maybe' values.
--
-- Because TemplatePG has to prepare statements at compile time and
-- placeholders can't be used in place of lists in PostgreSQL (such as @IN
-- (?)@), it's not currently possible to use non-static @IN ()@ clauses.

-- $other
-- There's no support for reading time intervals yet. As a workaround, you can
-- use @extract(epoch from ...)::int@ to get the interval as a number of
-- seconds.

-- $tips
-- If you find yourself pattern matching on result tuples just to pass them on
-- to functions, you can use @uncurryN@ from the tuple package. The following
-- examples are equivalent.
--
-- @(a, b, c) <- $(queryTuple \"SELECT a, b, c FROM {tableName} LIMIT 1\")
--
-- someFunction a b c
-- @
--
-- @uncurryN someFunction \`liftM\` $(queryTuple \"SELECT a, b, c FROM {tableName} LIMIT 1\")
-- @
