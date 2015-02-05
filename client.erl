-module(client).
-export([main/1, initial_state/2]).
-include_lib("./defs.hrl").

%% Receive messages from GUI and handle them accordingly
main(State) ->
    receive
        {request, From, Ref, Request} ->
            {Response, NextState} = loop(State, Request),
            From ! {result, Ref, Response},
            main(NextState)
    end.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #cl_st { nick = Nick, gui = GUIName, server = undefined }.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server
loop(St = #cl_st{nick = Nick}, {connect, ServerName}) ->
    Server = list_to_atom(ServerName),
    case server:connect(Server, Nick) of
        ok ->
            {ok, St#cl_st{server = Server}};
        Response ->
            {Response, St}
    end;

%% Disconnect from server
loop(St, disconnect) when St#cl_st.server == undefined ->
    % Can not be connected if we dont have a server
    {cchat_errors:err_user_not_connected(), St};
loop(St = #cl_st{nick = Nick, server = Server}, disconnect) ->
    case server:disconnect(Server, Nick) of
        ok ->
            {ok, St#cl_st{server = undefined}};
        Response ->
            {Response, St}
    end;

% Join channel
loop(St, {join, _Channel}) ->
    {ok, St} ;

%% Leave channel
loop(St, {leave, _Channel}) ->
     {ok, St} ;

% Sending messages
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
     {ok, St} ;

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
loop(St = #cl_st { gui = GUIName }, _MsgFromClient) ->
    {incoming_msg, Channel, Name, Msg} = _MsgFromClient,
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
