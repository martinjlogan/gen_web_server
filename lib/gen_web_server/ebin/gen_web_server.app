%% This is the application resource file (.app file) for the gen_web_server,
%% application.
{application, gen_web_server, 
  [{description, "An application containing the gen_web_server behaviour container and interface"},
   {vsn, "0.4.0.0"},
   {modules, [gen_web_server,
              gws_connection_sup,
              gws_web_dav_util,
	      gws_server]},
   {registered,[]},
   {applications, [kernel, stdlib, sasl, inets]},
   {start_phases, []}]}.

