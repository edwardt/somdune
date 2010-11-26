{application, somdune,
 [{description, "Erlang HTTP router and load balancer"},
  {vsn, "0.1.0"},
  {modules,[somdune,
            somdune_manager,
            somdune_net,
            somdune_sup]},
  {registered,[somdune_manager, somdune_sup]},
  {applications,[kernel, stdlib]},
  {env, []},
  {mod, {somdune, []}}
 ]
}.
