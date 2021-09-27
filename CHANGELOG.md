## persistent-migration 0.3.0

Fixes:
* Don't insert into `persistent_migration` table if no migrations are running ([#72](https://github.com/brandonchinn178/persistent-migration/pull/72))
* Fix `SERIAL` type for INT64 columns ([#79](https://github.com/brandonchinn178/persistent-migration/pull/79))

## persistent-migration 0.2.1

Fixes:
* Fix build for persistent-2.12.0.0

## persistent-migration 0.2.0

Breaking changes:
* Moved some types out of `Database.Persist.Migration.Operation` and into `Database.Persist.Migration.Core`

Fixes:
* Fix for GHC 8.8

Other changes:
* Re-export `rawSql` in `Database.Persist.Migration`

## persistent-migration 0.1.0

Breaking changes:
* Remove prefixes from operations (#31)
* Refactored module structure (#34)
* New migration format with batched operations (#36)
* `Operation` is now a sum type instead of a newtype wrapper around `Migrateable` (#58)
* Interpolation now done with `MigrateSql`, e.g. in `RawOperation` (#62)
* Fix bug in `CreateTable` having multiple `Unique` constraints (#63)

Other changes:
* Add new operations: RenameTable, AddConstraint, DropConstraint (#33)
* Use hpack (#42)
* Allow specifying defaults in columns (#52)
* Add new operation: RenameColumn (#55)

## persistent-migration 0.0.2

* Generalize `hasMigration` and `checkMigration` to `MonadIO`

## persistent-migration 0.0.1

* Initial implementation
