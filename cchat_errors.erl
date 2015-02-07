
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

err_user_already_joined() ->
  {error, user_already_joined, "User is already in the channel"}.

err_user_not_joined() ->
  {error, user_not_joined, "User is not connected to the channel"}.

err_leave_channels_first() ->
  {error, leave_channels_first, "Must leave all channels before disconnecting"}.

err_user_not_found() ->
  {error, user_not_found, "The user was not found"}.