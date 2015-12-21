# TemplatePG - a PostgreSQL (Template) Haskell library with compile-time type inference

TemplatePG is designed with 2 goals in mind: safety and performance. The primary focus is on safety.

To help ensure safety, it uses the PostgreSQL server to parse every query and statement in your code to infer types at compile-time. This means that in theory you cannot get a syntax error at runtime. Getting proper types at compile time has the nice side-effect that it eliminates run-time type casting and usually results in less code. This approach was inspired by [MetaHDBC](http://haskell.org/haskellwiki/MetaHDBC) and [PG'OCaml](http://pgocaml.forge.ocamlcore.org/).

While compile-time query analysis eliminates many errors, it doesn't eliminate all of them. If you modify the database without recompilation or have an error in a trigger or function, for example, you can still trigger a PGException.

With that in mind, TemplatePG currently does a number of unsafe things: it doesn't handle unexpected messages from the server very gracefully, and it's not entirely safe when working with nullable result fields. I hope to fix all of these at some point in the future. In the meantime, use the software at your own risk. Note however that TemplatePG is currently powering [Vocabulink](http://www.vocabulink.com/) with no problems yet. (For usage examples, you can see the Vocabulink [source code](https://github.com/jekor/vocabulink)).

To improve performance, TemplatePG does not use prepared statements. In theory, this saves bandwidth (and a potential round-trip) and time for the extra step of binding parameters. Again in theory, this is also safe because we know the types of parameters at compile time. However, it still feels risky (and I would appreciate any audit of the code doing this, especially escapeString).

See the [Haddock documentation](http://hackage.haskell.org/package/templatepg) for how to use TemplatePG.
