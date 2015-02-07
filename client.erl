-module(client).
-export([main/1, initial_state/2, send_message/4]).
-include_lib("./defs.hrl").

%% Receive messages from other processes and handle them accordingly
main(State = #cl_st{}) ->
    receive
        {async_request, Request} ->
            {_, NextState} = loop(State, Request),
            main(NextState);
        {request, From, Ref, Request} ->
            {Response, NextState} = loop(State, Request),
            From ! {result, Ref, Response},
            main(NextState)
    end.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #cl_st{
        nick = Nick,
        gui = GUIName,
        server = undefined,
        channels = orddict:new()
    }.


% Forwards a message that was sent to a channel to the client.
send_message(ClientPid, ChannelName, Nick, Message) ->
    ClientPid ! {async_request, {incoming_msg, ChannelName, Nick, Message}}.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from other processes.

%% Connect to server
loop(St = #cl_st{nick = Nick}, {connect, ServerName}) ->
    Server = list_to_atom(ServerName),
    case server:connect(Server, Nick) of
        ok ->
            {ok, St#cl_st{server = Server}};
        Error ->
            {Error, St}
    end;

%% Disconnect from server
loop(St, disconnect) when St#cl_st.server == undefined ->
    % Can not be connected if we dont have a server
    {cchat_errors:err_user_not_connected(), St};

loop(St = #cl_st{server = Server}, disconnect) ->
    case orddict:is_empty(St#cl_st.channels) of
        true ->
            case server:disconnect(Server) of
                ok ->
                    {ok, St#cl_st{server = undefined}};
                Error ->
                    {Error, St}
            end;
        false ->
            {cchat_errors:err_leave_channels_first(), St}
    end;

% Join channel
loop(St = #cl_st{server = Server, channels = Channels}, {join, ChannelName}) ->
    case server:join_channel(Server, ChannelName) of
        {ok, ChannelPid} ->
            NewChannels = orddict:store(ChannelName, ChannelPid, Channels),
            {ok, St#cl_st{channels = NewChannels}};
        Error ->
            {Error, St}
    end;

%% Leave channel
loop(St = #cl_st{server = Server, channels = Channels}, {leave, ChannelName}) ->
    case server:leave_channel(Server, ChannelName) of
        ok ->
            NewChannels = orddict:erase(ChannelName, Channels),
            {ok, St#cl_st{channels = NewChannels}};
        Error ->
            {Error, St}
    end;

% Sending messages
loop(St = #cl_st{channels = Channels, nick = Nick}, {msg_from_GUI, ChannelName, Msg}) ->
    case orddict:find(ChannelName, Channels) of
        {ok, ChannelPid} ->
            case channel:send_message(ChannelPid, Nick, Msg) of
                ok -> {ok, St}
            end;
        error ->
            {cchat_errors:err_user_not_joined(), St}
    end;

%% Get current nick
loop(St, whoami) ->
    {St#cl_st.nick, St};

%% Change nick
loop(St = #cl_st{}, {nick, Nick}) ->
    if
        St#cl_st.server == undefined ->
            {ok, St#cl_st{nick = Nick}};
        St#cl_st.server /= undefined ->
            {cchat_errors:err_user_already_connected(), St}
    end;

%% Incoming message
loop(St = #cl_st{gui = GUIName}, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name ++ "> " ++ Msg}),
    {ok, St}.
