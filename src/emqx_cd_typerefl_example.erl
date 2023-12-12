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

