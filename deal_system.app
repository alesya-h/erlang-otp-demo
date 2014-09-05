{application, deal_system,
  [{description, "Deal processing system"},
   {vsn, "0.1"},
   {modules, [deal_server, deal_server_sup, deal_server_test,
              deal_system, deal_system_sup,
              deals_yaws,  deals_yaws_sup,
              time_helper, web_helper]},
   {registered, [deal_server, deals_yaws]},
   {applications, [kernel, stdlib]},
   {env, []},
   {mod, {deal_system, 8080}}]}.

