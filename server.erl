-module(server).
-export([main/1, initial_state/1, connect/2, disconnect/1, join_channel/2, leave_channel/2]).
-include_lib("./defs.hrl").


handle_connect(S = #server_st{clients = Clients}, From, MsgRef, Nick) ->
    case lists:keyfind(Nick, #server_client.nick, Clients) of
        false ->
            NewClient = #server_client{nick = Nick, pid = From},
            From ! {result, MsgRef, ok},
            S#server_st{clients = [NewClient | Clients]};
        _ ->
            From ! {result, MsgRef, user_already_connected},
            S
    end.

handle_disconnect(S = #server_st{clients = Clients}, From, MsgRef) ->
    case lists:keyfind(From, #server_client.pid, Clients) of
        false ->
            From ! {result, MsgRef, user_not_connected},
            S;
        Client ->
            From ! {result, MsgRef, ok},
            S#server_st{clients = lists:delete(Client, Clients)}
    end.

handle_join_channel(S = #server_st{channels = Channels}, From, MsgRef, ChannelName) ->
    case dict:find(ChannelName, Channels) of
        {ok, {ChannelPid, ClientPids}} ->
            case lists:member(From, ClientPids) of
                true ->
                    From ! {result, MsgRef, user_already_joined},
                    S;
                false ->
                    channel:add_client(ChannelPid, From),
                    NewChannels = dict:store(ChannelName, {ChannelPid, [From | ClientPids]}, Channels),
                    From ! {result, MsgRef, {ok, ChannelPid}},
                    S#server_st{channels = NewChannels}
            end;
        error ->
            ChannelPid = channel:start(ChannelName, [From]),
            NewChannels = dict:store(ChannelName, {ChannelPid, [From]}, Channels),
            From ! {result, MsgRef, {ok, ChannelPid}},
            S#server_st{channels = NewChannels}
    end.


handle_leave_channel(S = #server_st{channels = Channels}, From, MsgRef, ChannelName) ->
    case dict:find(ChannelName, Channels) of
        {ok, {ChannelPid, ClientPids}} ->
            case lists:member(From, ClientPids) of
                false ->
                    From ! {result, MsgRef, user_not_joined},
                    S;
                true ->
                    channel:remove_client(ChannelPid, From),
                    NewClientPids = lists:delete(From, ClientPids),
                    NewChannels = dict:store(ChannelName, {ChannelPid, NewClientPids}, Channels),
                    From ! {result, MsgRef, ok},
                    S#server_st{channels = NewChannels}
            end;
        error ->
            From ! {result, MsgRef, user_not_joined},
            S
    end.


main(S = #server_st{}) ->
    receive
        {request, From, MsgRef, {connect, Nick}} ->
            NewState = handle_connect(S, From, MsgRef, Nick),
            main(NewState);
        {request, From, MsgRef, {disconnect}} ->
            NewState = handle_disconnect(S, From, MsgRef),
            main(NewState);
        {request, From, MsgRef, {join_channel, Channel}} ->
            NewState = handle_join_channel(S, From, MsgRef, Channel),
            main(NewState);
        {request, From, MsgRef, {leave_channel, Channel}} ->
            NewState = handle_leave_channel(S, From, MsgRef, Channel),
            main(NewState);
        Msg ->
            % To make sure nothing is sending accidental messages to
            % our server; catch all clause.
            io:format("Server got unexpected message: ~p~n", [Msg]),
            main(S)
    end.

initial_state(ServerName) ->
    #server_st{server = ServerName, clients = [], channels = dict:new()}.

% Tries to send a message to ProcessName. Does not use
% helper:request/3 because we need to do some additional
% checks to make sure server can be reached
try_request(ProcessName, Request) ->
    case whereis(ProcessName) of
        undefined -> server_not_reached;
        Pid ->
            Ref = make_ref(),
            Pid ! {request, self(), Ref, Request},
            receive
                {result, Ref, Result} ->
                    Result;
                {exit, Ref, Reason} ->
                    exit(Reason)
            after 3000 ->
                server_not_reached
            end
    end.


%
% -- API functions for talking to the server --
%

connect(ProcessName, Nick) ->
    case try_request(ProcessName, {connect, Nick}) of
        ok -> ok;
        user_already_connected -> cchat_errors:err_user_already_connected();
        server_not_reached -> cchat_errors:err_server_not_reached()
    end.

disconnect(ProcessName) ->
    case try_request(ProcessName, {disconnect}) of
        ok -> ok;
        user_not_connected -> cchat_errors:err_user_not_connected();
        server_not_reached -> cchat_errors:err_server_not_reached()
    end.

join_channel(Server, Channel) ->
    case helper:request(Server, {join_channel, Channel}) of
        {ok, ChannelPid} -> {ok, ChannelPid};
        user_already_joined -> cchat_errors:err_user_already_joined()
    end.

leave_channel(Server, Channel) ->
    case helper:request(Server, {leave_channel, Channel}) of
        ok -> ok;
        user_not_joined -> cchat_errors:err_user_not_joined()
    end.
