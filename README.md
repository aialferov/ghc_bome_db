# GHC Bome database

Erlang application implementing data storage server for
[GHC Bome Service](http://github.com/aialferov/ghc-bome).

## API

The application provides CRUD operations with user data and exports
corresponding functions for that.

### Create

The following function creates or ovewrites existing data of the specified
(by "Id") resource:

```
-spec ghc_bome_db:put(
    Id :: term(),
    Data :: #{
        Type :: term(),
        Value :: term()
    }
) ->
    {ok, created} |
    {ok, modified}
```

### Read

Data of the resource could also be read by its "Id":

```
-spec ghc_bome_db:get(
    Id :: term(),
    Options :: [
        {filter, [DataType :: term()]}
    ]
) ->
    {ok, Data :: #{
        Type :: term(),
        Value :: term()
    }} |
    {error, not_found}
```

If option "filter" is specified the data of specified "type" list will be
returned only.

### Update

The "patch" function is used to partially update the data:

```
-spec ghc_bome_db:patch(
    Id :: term(),
    Data :: #{
        Type :: term(),
        Value :: term()
    }
) ->
    ok | {error, not_found}
```

### Delete

All the data to delete should be excplicitly specified:

```
-spec ghc_bome_db:delete(
    Id :: term(),
    DataKeys :: [
        Type :: term(),
    ]
) ->
    ok | {error, not_found}
```

## Run

Although the application should be used within the GHC Bome Service it also
could run on its own, for instance for debug purposes. The following command
runs the application in an Erlang shell:

```
$ make shell
```

The received data is being saved every 1 second into the "ghc_bome.db" file
(in the working directory). This could be configured in "app" file or overriden
in any other appropriate way (e.g. using a custom "sys.config" file or by
"application:set_env/3,4").

### Tests

Run unit tests:

```
$ make check
```
