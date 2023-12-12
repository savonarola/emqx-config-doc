# EMQX Configuration System

EMQX Configuration System is one of the most important subsystems of the EMQX broker.
It serves as the backbone of the whole EMQX cluster. It is used not only for the initial
configuration of the EMQX cluster but also for the dynamic configuration, internally or
via the HTTP API.
It is also the source of truth for the non-ephemeral state of all the subsystems of EMQX, like
* MQTT listeners;
* Authentication/authorization backends;
* MQTT bridges;
* etc.

The following high-level components may be distinguished, which we further describe in more detail:
* Configuration parsers and validators that are responsible for the parsing and validation of the configuration coming from the outside world, like the configuration files or the HTTP API.
* The EMQX configuration schema that defines the possible structure of the configuration state.
* The internal configuration state of an EMQX node, with some read/update API around it.
* The clustering subsystem tath is responsible for the distribution of the configuration state across the cluster.

Now we will describe the tools and libraries that we use to implement these components. We also will demonstrate some scenarios of the configuration state update.

## HOCON

HOCON is a configuration [language](https://github.com/lightbend/config/blob/main/HOCON.md) that is used in the EMQX configuration _files_, like `etc/emqx.conf` or `data/configs/cluster.hocon`

HOCON allows to describe JSON-like data structures:

```
field1 = value

field2 = 12.3

# Comment

mapfield {
    key1 = value1
    key2 = value2
}

arrayfield = [
    "value1",
    "value2",
    {
        key1 = "value1"
        key2 = "value2"
    }
]

nested.field = value
```

## `hocon` library

To work with HOCON EMQX uses the [`hocon`](https://github.com/emqx/hocon) library.
This library not only parses HOCON configuration files but also provides tools for the validation and conversion of the configuration data.

### Basic Parsing

First of all, `hocon` can parse HOCON files or binaries.

```erlang
1> hocon:load("example.hocon").
{ok,#{<<"arrayfield">> =>
          [<<"value1">>,<<"value2">>,
           #{<<"key1">> => <<"value1">>,<<"key2">> => <<"value2">>}],
      <<"field1">> => <<"value">>,<<"field2">> => 12.3,
      <<"mapfield">> =>
          #{<<"key1">> => <<"value1">>,<<"key2">> => <<"value2">>},
      <<"nested">> => #{<<"field">> => <<"value">>}}}


2> {ok, Bin} = file:read_file("example.hocon"), hocon:binary(Bin).
{ok,#{<<"arrayfield">> =>
          [<<"value1">>,<<"value2">>,
           #{<<"key1">> => <<"value1">>,<<"key2">> => <<"value2">>}],
      <<"field1">> => <<"value">>,<<"field2">> => 12.3,
      <<"mapfield">> =>
          #{<<"key1">> => <<"value1">>,<<"key2">> => <<"value2">>},
      <<"nested">> => #{<<"field">> => <<"value">>}}}
```

### Typerefl

There is one more lib that is closely related to `hocon` - [`typerefl`](https://github.com/ieQu1/typerefl).
This library allows runtime data validation against types declared as typespecs.

See the [example](src/emqx_cd_typerefl_example.erl) below:

```erlang
-module(emqx_cd_typerefl_example).

-export([test/0]).

%% required for handling -reflect_type() attribute
-include_lib("typerefl/include/types.hrl").

%% some custom type
-type record() :: #{id := integer(), name := string()}.

%% ask typerefl to reify the type of record/0, so we can use it in runtime
-reflect_type([record/0]).

test() ->
    %% use type in runtime checks
    ok = typerefl:typecheck(record(), #{id => 1, name => "test"}),
    {error, _} = typerefl:typecheck(record(), #{}),
    ok.
```

* We declared a custom type `record/0`.
* We asked `typerefl` to reify the type of `record/0`, i.e. make a _function_ that may be used in runtime checks.
* We used the reified type to validate some test data.

### HOCON Schema

Configuration data for a complex system like EMQX may be quite complex itself. Examples of the additional complexities are:

* Configuration may contain data structures more complex than just maps and arrays, like tuples.
* We may want produce OTP settings and settings for OTP applications from the configuration, like those that are usually placed in the `sys.config` and `vm.args` files.
* We may want to have complex validation rules.

So to tackle all these complexities `hocon` library provides the concept of _schema_. Schema is a description of data model written in provided DSL and including validation, conversion and translation rules.








#### Validations

#### Conversions

#### Translations

#### Defaults

## EMQX Configuration

### EMQX Schemas

### EMQX Config

### EMQX Config Local Update

### EMQX Config Cluster Update

### EMQX Cluster Join




