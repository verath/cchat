% This record defines the structure of the client process.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   server: the name (or Pid) of the server process.
%   nick: The nick of the client.
%   channels: A dict {channelName -> channelPid} that the client has currently joined.
-record(cl_st, {
    gui,
    server,
    nick,
    channels
}).

% This record defines the structure of the server process.
% It contains the following fields:
%   server: the name of the server process.
%   clients: A list of server_client records
%   channels: A dict {channelName -> {channelPid, [clientPid1, clientPid2,...]}}
-record(server_st, {
    server,
    clients,
    channels
}).

% This record defines the structure of a client, as used by the server.
% Fields:
%   nick: The name of the client,
%   pid: The name (or Pid) of the client process.
-record(server_client, {
    nick,
    pid
}).

% This record defines the structure of the channel process.
% It contains the following fields:
%   name: the name of the channel.
%   clients: A list of clientPids.
-record(channel_st, {
    name,
    clients
}).