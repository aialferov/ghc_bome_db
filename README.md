# GHC Bome database

Erlang application implementing metric storage server for
[GHC Bome Service](http://github.com/aialferov/ghc-bome).

## API

The application provides CRUD operations with user metrics and exports
corresponding functions for that.

### Create

The following function creates or ovewrites existing metrics of the specified
(by "User Id") resource:

```
-spec ghc_bome_db:put(
    UserId :: term(),
    Metrics :: #{
        Name :: term(),
        Value :: term()
    }
) ->
    {ok, created} |
    {ok, modified}
```

### Read

Metrics could also be read by its User Id:

```
-spec ghc_bome_db:get(
    UserId :: term(),
    Options :: [
        {filter, [MetricName :: term()]}
    ]
) ->
    {ok, Metrics :: #{
        Name :: term(),
        Value :: term()
    }} |
    {error, not_found}
```

Option "filter" contains list of metric names to return. If it is absent all
metrics are returned. If it is empty — an empty map is returned.

### Update

The "patch" function is used to partially update metrics:

```
-spec ghc_bome_db:patch(
    UserId :: term(),
    Metrics :: #{
        Name :: term(),
        Value :: term()
    }
) ->
    ok | {error, not_found}
```

Existing metrics will be updated. Not existing metrics will be added.

### Delete

All the metrics to delete should be excplicitly specified:

```
-spec ghc_bome_db:delete(
    UserId :: term(),
    MetricNames :: [
        Name :: term(),
    ]
) ->
    ok | {error, not_found}
```

Attempt to delete non-existing metric or empty metric names list affect nothing
and return "ok".

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
