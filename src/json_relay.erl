-module(json_relay).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([start/0, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% Hooks
-export ([packet/5]).


-include_lib("logger/include/log.hrl").


%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(?MODULE).

start(_StartType, _StartArgs) ->
    ?MODULE:start_link().

stop(_State) ->
    '_info'("JSON Relay stopped."),
    ok.


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Opts) ->
    URI = misc:get_env(?MODULE, url, Opts),
    HooksWeight = misc:get_env(?MODULE, weight, Opts),
    hooks:install({terminal, packet}, HooksWeight, {?MODULE, packet}, [terminal]),
    ok = inets:start(),
    ok = ssl:start(),
    '_notice'("json_relay is going to send something to ~s, opts is ~p", [URI, Opts]),
    {ok, { {one_for_one, 5, 10}, []} }.


%% ===================================================================
%% Hooks callbacks
%% ===================================================================

%% Вызывается, когда прибывает пакет с координатами
packet(terminal, Pid, {_Protocol, Uin} = Terminal, Packet, Timeout) ->
  '_debug'("Called packet with pid, terminal, packet, timeout: ~p, ~p, ~p, ~p", [Pid, Terminal, Packet, Timeout]),
  Eventtime = case maps:get(eventtime, Packet, undefined) of
                undefined -> erlang:universaltime();
                ET -> ET
              end,
  FullPacket = Packet#{eventtime => Eventtime, imei => Uin},
  send_json(FullPacket).

% формирует JSON и POST'ит его
send_json(Packet) ->
  PacketJSON = misc:to_json(maps:without([raw], Packet)),
  URI = misc:get_env(?MODULE, url, []),
  '_debug'("Sending packet to <~s>, content: ~s", [URI, PacketJSON]),
  case httpc:request(post, {URI, [{"Accept", "*/*"}], "application/json", PacketJSON}, [], []) of
    {ok, Result} ->
      {{_, StatusCode, _StatusText}, _Headers, Body} = Result,
      Success = StatusCode div 100 =:= 2, % Only 2xx HTTP codes are good
      case {Success, StatusCode} of
        {true, _}    -> '_debug'("Successfully sent data with result: ~p", [Result]);
        {false, 422} -> % 422 means "Your data is semantically invalid!"
          '_warning'("Server didn't accept data saying: ~s", [Body]);
        {false, 503} -> % Service Unavailable, usually means "We are deploying now (or in another maintenance), try later"
          % TODO: Postpone data sending for some time
          '_warning'("Server ~s currently not available, try later!", [URI]);
        {false, _}   -> % We didn't expect that, ignore
          '_warning'("Server responded inexpectedly: ~p", [Result])
      end;
    {error, Reason} ->
      case Reason of
        % econnrefused and etimedout are both there
        {failed_connect, Details} -> '_warning'("Can't connect to server: ~p", [Details])
      end
  end.
