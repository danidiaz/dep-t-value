# dep-t-value-sqlite

Pair external resources with types within your program, and read values of such
types from an SQLite database.

Part of the [dep-t-framework](https://github.com/danidiaz/dep-t-framework). 

## remember to add the resources to extra-source-files

Like this:

    extra-source-files:  lib/**/*.txt

Requires `cabal-version: 3` or greater.

## making direct-sqlite work on windows

[download page](https://www.sqlite.org/download.html).

On `cabal.project`:

    constraints: direct-sqlite +systemlib

On `cabal.project.local`:

    package direct-sqlite
        extra-include-dirs: C:/Users/someuser/.local/sqlite-amalgamation-3350300
        extra-lib-dirs: C:/Users/someuser/.local/sqlite-dll-win64-x64-3350300

Example `Main.hs`:

    {-# language ImportQualifiedPost #-}
    module Main where

    import Database.SQLite3
    import Data.Text qualified as T

    main :: IO ()
    main = do
        db <- open (T.pack "foo.db")
        execPrint db (T.pack "select * from foo;")
        execPrint db (T.pack "begin transaction;")
        execPrint db (T.pack "alter table foo drop column f3;")
        execPrint db (T.pack "rollback;")
        close db

## Alternatives

- First of all, the built-in [`Paths_pkgname` mechanism for accessing data files from package code](https://cabal.readthedocs.io/en/latest/cabal-package.html?highlight=getDataFileName%20#accessing-data-files-from-package-code). See also [this issue](https://github.com/haskell/cabal/issues/6096) in the Cabal repository.

- [file-embed: Use Template Haskell to embed file contents directly.](https://hackage.haskell.org/package/file-embed)

- [data-embed: Embed files and other binary blobs inside executables without Template Haskell.](https://hackage.haskell.org/package/data-embed)

