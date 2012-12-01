{application, deal_system,
  [{description, "Deal processing system"},
   {vsn, "0.1"},
   {modules, [deal_server.erl, deal_server_sup.erl, deal_server_test.erl,
              deal_system.erl, deal_system_sup.erl,
              deals_yaws.erl,  deals_yaws_sup.erl,
              time_helper.erl, web_helper.erl]},
   {registered, [deal_server, deals_yaws]},
   {applications, [kernel, stdlib]},
   {env, []},
   {mod, {deal_system, 8080}}]}.

