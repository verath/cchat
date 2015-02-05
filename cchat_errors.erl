
-module(cchat_errors).

%% API
%% TODO: replace with export([...]).
-compile(export_all).


err_user_already_connected() ->
  {error, user_already_connected, "User already connected to the server."}.

err_user_not_connected() ->
  {error, user_not_connected, "User not connected to the server."}.

err_server_not_reached() ->
  {error, server_not_reached, "Server could not be reached."}.
