-module(cchat_errors).

%% API
-export([
    err_server_not_reached/0,
    err_user_already_connected/0,
    err_user_not_connected/0,
    err_user_already_joined/0,
    err_user_not_joined/0,
    err_leave_channels_first/0,
    err_nick_taken/0
]).


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

err_nick_taken() ->
    {error, nick_taken, "That nick is already connected to this server."}.
