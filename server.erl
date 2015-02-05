-module(server).
-export([main/1, initial_state/1, change_nick/2, connect/2, disconnect/2]).
-include_lib("./defs.hrl").



handle_connect(S = #server_st{}, From, MsgRef, Nick) ->
    case orddict:is_key(Nick, S#server_st.clients) of
        true ->
            From ! {result, MsgRef, user_already_connected},
            S;
        false ->
            NewClients = orddict:store(Nick, From, S#server_st.clients),
            From ! {result, MsgRef, ok},
            S#server_st{clients = NewClients}
    end.

handle_disconnect(S = #server_st{}, From, MsgRef, Nick) ->
    case orddict:is_key(Nick, S#server_st.clients) of
        true ->
            NewClients = orddict:erase(Nick, S#server_st.clients),
            From ! {result, MsgRef, ok},
            S#server_st{clients = NewClients};
        false ->
            From ! {result, MsgRef, user_not_connected},
            S
    end.


main(S = #server_st{}) ->
    receive
        {request, From, MsgRef, {connect, Nick}} ->
            NewState = handle_connect(S, From, MsgRef, Nick),
            main(NewState);
        {request, From, MsgRef, {disconnect, Nick}} ->
            NewState = handle_disconnect(S, From, MsgRef, Nick),
            main(NewState);
        Msg ->
            % To make sure nothing is sending accidental messages to
            % our server; catch all clause.
            io:format("Server got unexpected message: ~p~n", [Msg]),
            main(S)
    after 60000 ->
        % Todo: Should probably not timeout, used for testing.
        io:format("~s ~n", ["Server: Timeout!"])
    end.

initial_state(ServerName) ->
    #server_st{server = ServerName, clients = orddict:new()}.

% Tries to send a message to ProcessName. Does not use
% helper:request/3 because it uses exit to throw
% exceptions, and trapping exit is not something
% that should be neccessary?
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


disconnect(ProcessName, Nick) ->
    io:format("Disconnect from ~p~n", [ProcessName]),
    case try_request(ProcessName, {disconnect, Nick}) of
        ok -> ok;
        user_not_connected -> cchat_errors:err_user_not_connected();
        server_not_reached -> cchat_errors:err_server_not_reached()
    end.


change_nick(Pid, Nick) ->
    helper:request(Pid, {change_nick, Nick}).


