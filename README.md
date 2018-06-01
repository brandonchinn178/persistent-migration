# persistent-migration

This is a migration library for the
[persistent](http://www.stackage.org/package/persistent) package.

By default, persistent provides a way to do automatic migrations; how to
quickly and conveniently update the schema to match the definitions of the
models. Because of its automatic nature, it will also balk at any operations
that may delete data ("unsafe" migrations).

However, in a lot of production cases, you don't actually want this automatic
migration. You might want to be able to run certain unsafe migrations because
you know a column is safe to delete. You might want to be able to copy and
transform data from one column to another and then delete the old column. You
might want explicit/manual migrations for other reasons.

This package exposes an `Operation` data type that will be converted into SQL
by a persistent backend. To define a series of migrations, write a list of
these `Operations` and call `runMigration` from the appropriate persistent
backend library. `runMigration` will run all of the `Operations` that have not
been run yet.

```
import Database.Persist.Migration
import Database.Persist.Sql (PersistValue(..), rawExecute, rawSql)

createPerson :: CreateTable
createPerson = CreateTable
  { name = "person"
  , schema =
      [ Column "id" SqlInt32 []
      , Column "name" SqlString []
      , Column "age" SqlInt32 []
      , Column "alive" SqlBool [Defaults "TRUE"]
      , Column "hometown" SqlInt64 [Nullable, ForeignKey ("cities", "id")]
      ]
  , constraints =
      [ PrimaryKey ["id"]
      , Unique ["name"]
      ]
  }

migrateHeight :: RawOperation
migrateHeight = RawOperation "Separate height into height_feet, height_inches" $
  rawSql "SELECT id, height FROM person" [] >>= traverse_ migrateHeight'
  where
    migrateHeight' (Single id', Single height) = do
      let (feet, inches) = quotRem height 12
      rawExecute "UPDATE person SET height_feet = ?, height_inches = ? WHERE id = ?"
        [ PersistInt64 feet
        , PersistInt64 inches
        , PersistInt64 id'
        ]

migration :: Migration
migration =
  -- version 1
  [ Operation 0 $ createPerson)

  -- version 2
  , Operation 1 $ AddColumn "person" (Column "gender" SqlString [Nullable]) Nothing)
  -- Non-null column without default for inserted rows needs a default for existing rows.
  , Operation 2 $ AddColumn "person" (Column "height" SqlInt32 []) (Just "0"))

  -- version 3
  , Operation 3 $ AddColumn "person" (Column "height_feet" SqlInt32 []) (Just "0"))
  , Operation 4 $ AddColumn "person" (Column "height_inches" SqlInt32 []) (Just "0"))
  , Operation 5 $ migrateHeight)
  , Operation 6 $ DropColumn "person" "height")
  ]
```

```
import Database.Persist.Migration (checkMigration)
import Database.Persist.Migration.Sqlite (runMigration)

-- the migration defined above
import MyMigration (migration)

-- the migration from persistent's mkMigrate
import MyMigration.Migrate (migrationDef)

main = do
  -- run the usual migration
  runMigration migration

  -- fails if persistent detects more migrations not accounted for
  checkMigration migrationDef
```
