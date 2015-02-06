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
    '_notice'("json_relay is going to send something to ~s, opts is ~p", [URI, Opts]),
    {ok, { {one_for_one, 5, 10}, []} }.


%% ===================================================================
%% Hooks callbacks
%% ===================================================================

%% Вызывается, когда прибывает пакет с координатами, формирует JSON и POST'ит его.
packet(terminal, _Pid, _Terminal, Packet, _Timeout) ->
  '_debug'("Called packet with pid, terminal, packet, timeout: ~p, ~p, ~p", [_Pid, _Terminal, Packet, _Timeout]),
  Eventtime = case maps:get(eventtime, Packet, undefined) of
                undefined -> erlang:universaltime();
                ET -> ET
              end,
  FullPacket = Packet#{eventtime => Eventtime},
  PacketJSON = json_enc(maps:without([raw], FullPacket)),
  URI = misc:get_env(?MODULE, url, []),
  case hooks:get(raw_id) of
    undefined -> stop;
    _RawId ->
      '_trace'("About to send packet to <~s>, content: ~ts", [URI, PacketJSON]),
      case httpc:request(post, {URI, [{"Accept", "*/*"}], "application/json", PacketJSON}, [], []) of
        {ok, Result} -> '_trace'("Successfully sent data with result: ~p", [Result]);
        {error, Reason} -> '_trace'("Something went wrong: ~p", [Reason])
      end
  end.


%% Different copy-pasted things. TODO: Move it somewhere

json_enc(L) ->
  L1 = pre_json(L),
  case catch jsxn:encode(L1) of
    {'EXIT', {badarg, _}} -> '_warning'("can't transform to json: ~w", [L1]), <<"{}">>;
    {'EXIT', Reason} -> '_warning'("jsx failed transform ~w: ~w", [L1, Reason]), <<"{}">>;
    E -> E
  end.

pre_json(Map) ->
  maps:map(
    fun(_, V) when is_map(V) -> pre_json(V);
       (LL, {G, M}) when LL =:= latitude; LL =:= longitude ->
        #{d => G, m => M};
       (_, V) -> V
    end,
    Map).
