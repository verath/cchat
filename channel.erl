-module(channel).
-include_lib("./defs.hrl").

%% API
-export([init/1, start/1]).

loop(St) ->
  receive
    Msg ->
      % To make sure nothing is sending accidental messages to
      % our channel; catch all clause.
      io:format("Channel got unexpected message: ~p~n", [Msg]),
      loop(St)
  end.

init(ChannelName) ->
  loop(ChannelName).

start(ChannelName) ->
  spawn(?MODULE, init, [ChannelName]).
