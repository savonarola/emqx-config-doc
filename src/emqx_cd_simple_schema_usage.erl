-module(emqx_cd_simple_schema_usage).

-export([check/1]).

check(Filename) ->
    {ok, Data} = file:read_file(Filename),
    {ok, Conf} = hocon:binary(Data),
    hocon_tconf:check_plain(emqx_cd_simple_schema, Conf).
