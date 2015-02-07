-module(channel).
-include_lib("./defs.hrl").

%% API
-export([init/2, start/2, add_client/2, remove_client/2, send_message/3]).

handle_add_client(St = #channel_st{clients = Clients}, From, Ref, ClientPid) ->
    NewClients = [ClientPid | Clients],
    From ! {result, Ref, ok},
    St#channel_st{clients = NewClients}.

handle_remove_client(St = #channel_st{clients = Clients}, From, Ref, ClientPid) ->
    NewClients = lists:delete(ClientPid, Clients),
    From ! {result, Ref, ok},
    St#channel_st{clients = NewClients}.

handle_message(St = #channel_st{clients = Clients, name = ChannelName}, From, Ref, Nick, Message) ->
    % TODO: benchmark this further, for perf tests.
    spawn(fun() ->
        lists:foreach(fun(ClientPid) ->
            ClientPid ! {async_request, {incoming_msg, ChannelName, Nick, Message}}
        end, lists:delete(From, Clients))
    end),
    % NOTE: this reply only means channel has received the mesage and nothing about
    % other clients getting the message!
    From ! {result, Ref, ok},
    St.

loop(St = #channel_st{}) ->
    receive
        {request, From, Ref, {add_client, ClientPid}} ->
            NewState = handle_add_client(St, From, Ref, ClientPid),
            loop(NewState);
        {request, From, Ref, {remove_client, ClientPid}} ->
            NewState = handle_remove_client(St, From, Ref, ClientPid),
            loop(NewState);
        {request, From, Ref, {message, Nick, Message}} ->
            NewState = handle_message(St, From, Ref, Nick, Message),
            loop(NewState);
        Msg ->
            % To make sure nothing is sending accidental messages to
            % our channel; catch all clause.
            io:format("Channel got unexpected message: ~p~n", [Msg]),
            loop(St)
    end.

init(ChannelName, Clients) ->
    loop(#channel_st{name = ChannelName, clients = Clients}).

start(ChannelName, Clients) ->
    spawn(?MODULE, init, [ChannelName, Clients]).

add_client(ChannelPid, ClientPid) ->
    helper:request(ChannelPid, {add_client, ClientPid}).

remove_client(ChannelPid, ClientPid) ->
    helper:request(ChannelPid, {remove_client, ClientPid}).

send_message(ChannelPid, Nick, Message) ->
    % Not using async here to minimize risk of messages arriving in
    % the wrong order. TODO: This too bad for performance?
    helper:request(ChannelPid, {message, Nick, Message}).
