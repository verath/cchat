% This record defines the structure of the client process.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   server: the name (or Pid) of the server process.
%   nick: The nick of the client.
%   joined_count: Counter for number of channels joined. Must be 0 for disconnect to be allowed.
-record(cl_st, {
  gui,
  server,
  nick,
  joined_count
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

-record(server_client, {
  nick,
  pid
}).