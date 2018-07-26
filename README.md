# persistent-migration

[![CircleCI](https://circleci.com/gh/brandonchinn178/persistent-migration/tree/master.svg?style=svg)](https://circleci.com/gh/brandonchinn178/persistent-migration/tree/master)

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
import Database.Persist.Sql (PersistValue(..), rawSql)

createPerson :: CreateTable
createPerson = CreateTable
  { name = "person"
  , schema =
      [ Column "id" SqlInt32 [NotNull, AutoIncrement]
      , Column "name" SqlString [NotNull]
      , Column "age" SqlInt32 [NotNull]
      , Column "alive" SqlBool [NotNull]
      , Column "hometown" SqlInt64 [References ("cities", "id")]
      ]
  , constraints =
      [ PrimaryKey ["id"]
      , Unique "person_identifier" ["name", "age", "hometown"]
      ]
  }

migrateHeight :: RawOperation
migrateHeight = RawOperation "Separate height into height_feet, height_inches" $
  map migrateHeight' <$> rawSql "SELECT id, height FROM person" []
  where
    migrateHeight' (Single id', Single height) =
      let (feet, inches) = quotRem height 12
      in MigrateSql "UPDATE person SET height_feet = ?, height_inches = ? WHERE id = ?"
        [ PersistInt64 feet
        , PersistInt64 inches
        , PersistInt64 id'
        ]

migration :: Migration
migration =
  -- first migration path should create all the tables
  [ 0 ~> 1 := [createPerson]

  -- can define shorter migration paths for equivalent operations; version 2, in this case, should result
  -- in the same schema, regardless of the path taken to get there.
  , 1 ~> 2 := [DropColumn ("person", "alive")]
  , 0 ~> 2 :=
    [ createPerson{ctSchema = filter ((/= "alive") . colName) $ ctSchema createPerson}
    ]

  -- example for adding columns
  , 2 ~> 3 :=
    [ AddColumn "person" (Column "gender" SqlString []) Nothing
      -- Adding a non-null column needs a default for existing rows.
    , AddColumn "person" (Column "height" SqlInt32 [NotNull]) (Just $ PersistInt64 0)
    ]

  -- example for more complex migrations; here, we split up the height field into feet and inches fields
  , 3 ~> 4 :=
    [ AddColumn "person" (Column "height_feet" SqlInt32 []) (Just $ PersistInt64 0)
    , AddColumn "person" (Column "height_inches" SqlInt32 []) (Just $ PersistInt64 0)
    , migrateHeight
    , DropColumn ("person", "height")
    ]
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

For more examples, see `test/integration/Migration.hs`.
