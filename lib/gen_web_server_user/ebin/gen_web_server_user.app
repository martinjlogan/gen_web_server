%% This is the application resource file (.app file) for the gen_web_server,
%% application.
{application, gen_web_server_user, 
  [{description, "An application that uses the gen_web_server for test and demonstration purposes"},
   {vsn, "0.1.0"},
   {modules, [gwsu_app,
              gwsu_sup,
	      gwsu_web_server]},
   {registered,[]},
   {applications, [kernel, stdlib, gen_web_server]},
   {mod, {gwsu_app, []}},
   {start_phases, []}]}.

