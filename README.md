# GHC Bome database

Provides database Erlang application for GHC Bome Service and is used there
as a dependency application.

## API

Put data:
```
> ghc_bome_db:put(User, Type, Value).
```

Get data:
```
> ghc_bome_db:get(User, Type).
> ghc_bome_db:get(User).
```

Delete data:
```
> ghc_bome_db:delete(User, Type).
> ghc_bome_db:delete(User).
```

### Debug

To debug or play with the API functions run an Erlang shell:
```
$ make shell
```

## Persistence

Data is being saved every second (if any changes) into the "/tmp/ghc_bome.db"
file. The parameters are specified in the "src/ghc_bome_db.app.src" file.
