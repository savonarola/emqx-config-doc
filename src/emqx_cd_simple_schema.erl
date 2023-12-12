-module(emqx_cd_simple_schema).

-behaviour(hocon_schema).

-include_lib("typerefl/include/types.hrl").

-export([namespace/0, roots/0, fields/1, tags/0]).

namespace() -> ?MODULE.

tags() ->
    [<<"tag">>, <<"another tag">>].

roots() ->
    [
      user,
      {payments, hoconsc:array(hoconsc:ref(payment))}
    ].

fields(user) ->
    [
      {id, hoconsc:mk(integer(), #{requried => true})},
      {email, hoconsc:mk(string(), #{requried => true})},
      {address, hoconsc:mk(hoconsc:ref(address), #{requried => true})}
    ];
fields(payment) ->
    [
      {amount, float()},
      {currency, hoconsc:mk(string(), #{default => <<"USD">>})}
    ];
fields(address) ->
    [
      {street, string()},
      {city, string()},
      {country, hoconsc:mk(string(), #{requried => true})}
    ].



