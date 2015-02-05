% This record defines the structure of the client process.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   server: the name (or Pid) of the server process.
%   nick: The nick of the client.
-record(cl_st, {
  gui,
  server,
  nick
}).

% This record defines the structure of the server process.
% It contains the following fields:
%   server: the name of the server process.
%
%
-record(server_st, {
  server,
  clients
}).
