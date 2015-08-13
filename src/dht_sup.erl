%%% @doc Main supervisor for the DHT code
%%% @end
%%% @private
-module(dht_sup).
-behaviour(supervisor).
-export([start_link/0]).

% supervisor callbacks
-export([init/1]).

-dialyzer({nowarn_function, [init/1]}).
%% ------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, dht_sup}, ?MODULE, []).

%% ------------------------------------------------------------------
init([]) ->
    {ok, Port} = application:get_env(dht, port),
    {ok, StateFile} = application:get_env(dht, state_file),
    {ok, BootstrapNodes} = application:get_env(dht, bootstrap_nodes),

    Store = { store,
               {dht_store, start_link, []},
               permanent, 5000, worker, [store] },
    State = { state,
               {dht_state, start_link, [StateFile, BootstrapNodes]},
               permanent, 5000, worker, [state] },
    Network = { network,
                {dht_net, start_link, [Port]},
               permanent, 5000, worker, [network] },
    Tracker = { tracker,
                 {dht_track, start_link, []},
               permanent, 5000, worker, [tracker] },
    {ok,
        {{ rest_for_one,
           5,
           900 },
        [Store, State, Network, Tracker]}}.

%% ------------------------------------------------------------------
