-module(channel).
-include_lib("./defs.hrl").

%% API
-export([init/2, start/2, add_client/2, remove_client/2]).

loop(St = #channel_st{clients = Clients, name = ChannelName}) ->
  receive
    {request, From, Ref, {add_client, ClientPid}} ->
      NewClients = [ClientPid | Clients],
      From ! {result, Ref, ok},
      loop(St#channel_st{clients = NewClients});
    {request, From, Ref, {remove_client, ClientPid}} ->
      NewClients = lists:delete(ClientPid, Clients),
      From ! {result, Ref, ok},
      loop(St#channel_st{clients = NewClients});
    {request, From, _Ref, {message, Nick, Message}} ->
      % TODO: benchmark this further, for perf tests.
      spawn(fun() ->
        lists:foreach(fun(ClientPid) ->
          ClientPid ! {message, ChannelName, Nick, Message}
        end, lists:delete(From, Clients))
      end),
      loop(St);
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