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
these `Operations` and call `runMigration` from the appropriate backend module.
Each `Operation` represents a movement from one version of the schema to
another. `runMigration` will check to see the current version of the schema and
run the `Operations` necessary to get from the current version to the latest
version.

```
import Database.Persist.Migration
import Database.Persist.Sql (PersistValue(..), rawExecute, rawSql)

createPerson :: CreateTable
createPerson = CreateTable
  { ctName = "person"
  , ctSchema =
      [ Column "id" SqlInt32 []
      , Column "name" SqlString [NotNull]
      , Column "age" SqlInt32 [NotNull]
      , Column "alive" SqlBool [NotNull, Default "TRUE"]
      , Column "hometown" SqlInt64 [References ("cities", "id")]
      ]
  , ctConstraints =
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
  -- first commit
  [ Operation (0 ~> 1) $ createPerson

  -- second commit
  , Operation (1 ~> 2) $ DropColumn ("person", "alive")
  , Operation (0 ~> 2) $ createPerson{ctSchema = filter ((/= "alive") . colName) $ ctSchema createPerson}
    -- Can define shorter paths for equivalent operations; version 2 should result in the same schema
    -- regardless of the path taken to get there.

  -- second commit
  , Operation (2 ~> 3) $ AddColumn "person" (Column "gender" SqlString []) Nothing
  , Operation (3 ~> 4) $ AddColumn "person" (Column "height" SqlInt32 [NotNull]) (Just "0")
    -- Non-null column without default for inserted rows needs a default for existing rows.

  -- third commit
  , Operation (5 ~> 6) $ AddColumn "person" (Column "height_feet" SqlInt32 []) (Just "0")
  , Operation (6 ~> 7) $ AddColumn "person" (Column "height_inches" SqlInt32 []) (Just "0")
  , Operation (7 ~> 8) $ migrateHeight
  , Operation (8 ~> 9) $ DropColumn ("person", "height")
  ]
```

```
import Database.Persist.Migration (checkMigration, defaultSettings)
import Database.Persist.Migration.Postgres (runMigration)

-- the migration defined above
import MyMigration (migration)

-- the migration from persistent's mkMigrate
import MyMigration.Migrate (migrationDef)

main = do
  -- run the usual migration
  runMigration defaultSettings migration

  -- fails if persistent detects more migrations not accounted for
  checkMigration migrationDef
```

For more examples, see `test/Integration/Migration.hs`.
