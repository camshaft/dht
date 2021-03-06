%% @doc EQC Model for the system state
%% The high-level entry-point for the DHT state. This model implements the public
%% interface for the dht_state gen_server which contains the routing table of the DHT
%%
%% The high-level view is relatively simple to define, since most of the advanced parts
%% pertaining to routing has already been handled in dht_routing_meta and its corresponding
%% EQC model.
%%
%% This model defines the large-scale policy rules of the DHT routing table. It uses dht_routing_meta
%% and dht_routing_table for the hard work at the low level and just delegates necessary work to
%% those parts of the code (and their respective models).
%%
%% The dht_state code is a gen_server which occasionally spawns functions to handle background
%% work. Some states are gen_server internal and are marked as such. They refer to callout
%% specifications which are done inside the gen_server. The code path is usually linear in this
%% case as well, however.
%%
%% @end
%%
%% TODO LIST:
%% • When a range is full, you can maybe insert the node anyway. You have to ask if
%%    the range can split, and if affirmative, you can still insert the node as the range
%%    will correctly split in this case. This is not currently handled by the code, but it
%%    is necessary for correct operation.

-module(dht_state_eqc).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-include("dht_eqc.hrl").

-record(state,{
	init = false, %% true when the model has been initialized
	id %% The NodeID the node is currently running under
}).

-define(K, 8).

%% API SPEC
%% -----------------
%%
%% We call out to the networking layer and also the routing meta-data layer.
api_spec() ->
	#api_spec {
		language = erlang,
		modules =
		  [
		  	#api_module {
		  		name = dht_routing_table,
		  		functions = [
		  			#api_fun { name = new, arity = 3, classify = dht_routing_table_eqc }
		  		]
		  	},
		  	#api_module {
		  		name = dht_routing_meta,
		  		functions = [
		  			#api_fun { name = new, arity = 1, classify = dht_routing_meta_eqc },
		  			#api_fun { name = export, arity = 1, classify = dht_routing_meta_eqc },
		  			
		  			#api_fun { name = insert, arity = 2, classify = dht_routing_meta_eqc },
		  			#api_fun { name = replace, arity = 3, classify = dht_routing_meta_eqc },
		  			#api_fun { name = remove, arity = 2, classify = dht_routing_meta_eqc },
		  			#api_fun { name = node_touch, arity = 3, classify = dht_routing_meta_eqc },
		  			#api_fun { name = node_timeout, arity = 2, classify = dht_routing_meta_eqc },
		  			#api_fun { name = reset_range_timer, arity = 3, classify = dht_routing_meta_eqc },
		  		
		  			#api_fun { name = member_state, arity = 2, classify = dht_routing_meta_eqc },
		  			#api_fun { name = neighbors, arity = 3, classify = dht_routing_meta_eqc },
		  			#api_fun { name = node_list, arity = 1, classify = dht_routing_meta_eqc },
		  			#api_fun { name = node_state, arity = 2, classify = dht_routing_meta_eqc },
		  			#api_fun { name = range_members, arity = 2, classify = dht_routing_meta_eqc },
		  			#api_fun { name = range_state, arity = 2, classify = dht_routing_meta_eqc }
		  		]
		  	},
		  	#api_module {
		  		name = dht_net,
		  		functions = [
		  			#api_fun { name = find_node, arity = 2 },
		  			#api_fun { name = ping, arity = 1 }
		  		]
		  	}
		  ]
	}.

%% Commands we are skipping:
%% 
%% We skip the state load/store functions. Mostly due to jlouis@ not thinking this is where the bugs are
%% nor is it the place where interesting interactions happen:
%%
%% * load_state/2
%% * dump_state/0, dump_state/1, dump_state/2
%%

%% INITIAL STATE
%% -----------------------

gen_state(ID) -> #state { id = ID, init = false }.
initial_state() -> #state{}.

%% START_LINK
%% -----------------------

%% Start up a new routing state tracker:
start_link(NodeID, Nodes) ->
    {ok, Pid} = dht_state:start_link(NodeID, {no_state_file, ?ID_MIN, ?ID_MAX}, Nodes),
    unlink(Pid),
    erlang:is_process_alive(Pid).
    
start_link_pre(S) -> not initialized(S).

start_link_args(#state { id = ID }) ->
    BootStrapNodes = [],
    [ID, BootStrapNodes].

%% Starting the routing state tracker amounts to initializing the routing meta-data layer
start_link_callouts(#state { id = ID }, [ID, Nodes]) ->
    ?MATCH(Tbl, ?CALLOUT(dht_routing_table, new, [ID, ?ID_MIN, ?ID_MAX], 'ROUTING_TABLE')),
    ?CALLOUT(dht_routing_meta, new, [Tbl], {ok, ID, 'META'}),
    ?SEQ([?APPLY(request_sucess, [N, #{ reachable => false}]) || N <- Nodes]),
    ?RET(true).

%% Once started, we can't start the State system again.
start_link_next(State, _, _) ->
    State#state { init = true }.

start_link_features(_S, _A, _R) -> [{state, start_link}].

%% CLOSEST TO
%% ------------------------

%% Return the `Num` nodes closest to `ID` known to the routing table system.
closest_to(ID, Num) ->
    dht_state:closest_to(ID, Num).
	
closest_to_pre(S) -> initialized(S).

closest_to_args(_S) ->
    [dht_eqc:id(), nat()].
	
%% This call is likewise just served by the underlying system
closest_to_callouts(_S, [ID, Num]) ->
    ?MATCH(Ns, ?CALLOUT(dht_routing_meta, neighbors, [ID, Num, 'META'],
       list(dht_eqc:peer()))),
    ?RET(Ns).

closest_to_features(_S, [_, Num], _) when Num >= 8 -> [{state, {closest_to, '>=8'}}];
closest_to_features(_S, [_, Num], _) -> [{state, {closest_to, Num}}].

%% NODE ID
%% ---------------------

%% Request the node ID of the system
node_id() -> dht_state:node_id().

node_id_pre(S) -> initialized(S).
	
node_id_args(_S) -> [].
	
node_id_callouts(#state { id = ID }, []) -> ?RET(ID).

node_id_features(_S, _A, _R) -> [{state, node_id}].

%% INSERT
%% ---------------------

%% The rules split into two variants

%% REQUEST_SUCCESS
%% ----------------

%% Tell the routing system a node responded succesfully.
%%
%% Once we learn there is a new node, we can request success on the
%% node. Doing so will attempt an insert of the node into the routing table,
%% or will update an already existing node in the routing table.
%%
%% • an IP/Port pair is first pinged to learn the ID of the pair, and
%%   then it is inserted.
%% • A node is assumed to be valid already and is
%%   just inserted. If you doubt the validity of a node, then supply
%%   it's IP/Port pair in which case the node will be inserted with a
%%   ping.
%%
%% Note that if the gen_server returns `{verify, QuestionableNode}`
%% then that node is being pinged by the call in order to possible
%% refresh this node. And then we recurse. This means there is a
%% behavior where we end up pinging all the nodes in a bucket/range
%% before insertion succeeds/fails.
%%
request_success(Node, Opts) ->
    Res = dht_state:request_success(Node, Opts),
    dht_state:sync(),
    Res.
    
request_success_pre(S) -> initialized(S).

request_success_args(_S) ->
    [dht_eqc:peer(), #{ reachable => bool() }].

request_success_callouts(_S, [Node, Opts]) ->
    ?MATCH(NodeState, ?APPLY(insert_node_gs, [Node])),
    case NodeState of
        ok -> ?RET(ok);
        already_member ->
            ?APPLY(request_success_gs, [Node, Opts]),
            ?RET(already_member);
        not_inserted -> ?RET(not_inserted);
        {error, Reason} -> ?RET({error, Reason});
        {verify, QN} ->
            ?APPLY(refresh_node, [QN]),
            ?APPLY(request_success, [Node, Opts])
    end.
    
request_success_gs_callouts(_S, [Node, Opts]) ->
    ?MATCH(MState,
     ?CALLOUT(dht_routing_meta, member_state,
              [Node, 'META'],
              oneof([unknown, member]))),
    case MState of
        unknown ->
            ?APPLY(insert_node_gs, [Node]),
            ?RET(ok);
        roaming_member -> ?RET(ok);
        member ->
            ?CALLOUT(dht_routing_meta, node_touch, [Node, Opts, 'META'], 'META'),
            ?RET(ok)
    end.

request_success_features(_S, [_, #{reachable := true }], _R) ->
	[{state, {request_success, reachable}}];
request_success_features(_S, [_, #{reachable := false }], _R) ->
	[{state, {request_success, non_reachable}}].

%% REQUEST_TIMEOUT
%% ----------------

%% Tell the routing system a node did not respond in a timely fashion
request_timeout(Node) ->
    Res = dht_state:request_timeout(Node),
    dht_state:sync(),
    Res.
    
request_timeout_pre(S) -> initialized(S).

request_timeout_args(_S) ->
    [dht_eqc:peer()].

request_timeout_callouts(_S, [Node]) ->
    ?MATCH(MState,
      ?CALLOUT(dht_routing_meta, member_state, [Node, 'META'], oneof([unknown, member]))),
    case MState of
        unknown -> ?RET(ok);
        member ->
          ?CALLOUT(dht_routing_meta, node_timeout, [Node, 'META'], 'META'),
          ?RET(ok);
        roaming_member -> ?RET(ok)
    end.

request_timeout_features(_S, [_], _) -> [{state, request_timeout}].

%% REFRESH_NODE
%% ------------------------------

%% Try refreshing the knowledge about the node `Node`. Used when inserting new nodes
%% to refresh already known nodes so they move from being questionable to being good.
refresh_node(Node) ->
    dht_state:refresh_node(Node).
    
refresh_node_pre(S) -> initialized(S).

refresh_node_args(_S) -> [dht_eqc:peer()].

%% Note that if the node has the wrong ID, we define the node as being timeouting.
%% This in turn makes the node bad and it will be replaced in the routing table, eventually.
refresh_node_callouts(_S, [{ID, IP, Port} = Node]) ->
    ?MATCH(PingRes, ?APPLY(ping, [IP, Port])),
    case PingRes of
        pang -> ?APPLY(request_timeout, [Node]);
        {ok, ID} -> ?APPLY(request_success, [Node, #{ reachable => true }]);
        {ok, _WrongID} -> ?APPLY(request_timeout, [Node])
    end.

refresh_node_features(_S, _A, _R) -> [{state, refresh_node}].

%% PING (Internal call to the network stack)
%% ---------------------

%% Ping a node, updating the response correctly on a succesful pong message
ping_callouts(_S, [IP, Port]) ->
    ?MATCH(R, ?CALLOUT(dht_net, ping, [{IP, Port}], oneof([pang, {ok, dht_eqc:id()}]))),
    ?RET(R).

ping_features(_S, _A, pang) -> [{state, {ping, pang}}];
ping_features(_S, _A, {ok, _}) -> [{state, {ping, ok}}].

%% INACTIVE_RANGE (GenServer Message)
%% --------------------------------

%% Refreshing a range proceeds based on the state of that range.
%% • The range is not a member: do nothing
%% • The range is ok: set a new timer for the range.
%%		This sets a timer based on the last activity of the range. In turn, it sets a timer
%%		somewhere between 0 and ?RANGE_TIMEOUT. Ex: The last activity was 5 minutes
%%		ago. So the timer should trigger in 10 minutes rather than 15 minutes.
%% • The range needs refreshing:
%%		refreshing the range amounts to executing a FIND_NODE call on a random ID in the range
%%		which is supplied by the underlying meta-data code. The timer is alway set to 15 minutes
%%		in this case (by a forced set), since we use the timer for progress.

inactive_range(Msg) ->
    dht_state:sync(Msg, 500).

inactive_range_pre(S) -> initialized(S).

inactive_range_args(_S) ->
    [{inactive_range, dht_eqc:range()}].
    
%% Analyze the state of the range and let the result guide what happens.
inactive_range_callouts(_S, [{inactive_range, Range}]) ->
    ?MATCH(RS, ?CALLOUT(dht_routing_meta, range_state, [Range, 'META'],
        oneof([{error, not_member}, ok, empty, {needs_refresh, dht_eqc:id()}]))),
    case RS of
        {error, not_member} -> ?EMPTY;
        ok ->
            ?CALLOUT(dht_routing_meta, reset_range_timer, [Range, #{ force => false }, 'META'], 'META');
        empty ->
            ?CALLOUT(dht_routing_meta, reset_range_timer, [Range, #{ force => true }, 'META'], 'META');
        {needs_refresh, ID} ->
            ?CALLOUT(dht_routing_meta, reset_range_timer, [Range, #{ force => true }, 'META'], 'META'),
            ?APPLY(refresh_range, [ID])
    end,
    ?RET(ok).

inactive_range_features(_S, _A, _R) -> [{state, inactive_range}].

%% REFRESH_RANGE (Internal private call)

%% This encodes the invariant that once we have found nodes close to the refreshing, they are
%% used as a basis for insertion.
refresh_range_callouts(_S, [ID]) ->
    ?MATCH(FNRes, ?CALLOUT(dht_net, find_node, [ID],
        oneof([{error, timeout}, {dummy, list(dht_eqc:peer())}]))),
    case FNRes of
        {error, timeout} -> ?EMPTY;
        {_, Near} ->
            ?SEQ([?APPLY(request_success, [N, #{ reachable => false}])
                  || N <- Near])
    end.
    
refresh_range_features(_S, _A, _R) -> [{state, refresh_range}].

%% INSERT_NODE (GenServer Internal Call)
%% --------------------------------

%% Insertion of a node on the gen_server side amounts to analyzing the state of the
%% range/bucket in which the node would fall. We generate random bucket data by
%% means of the following calls:
g_node_state(L) ->
    ?LET(S, vector(length(L), oneof([good, bad, {questionable, nat()}])),
        lists:zip(L, S)).

bucket_members() ->
    ?SUCHTHAT(L, list(dht_eqc:peer()),
       length(L) =< 8).

%% Given a set of pairs {Node, NodeState} we can analyze them and sort them into
%% the good, bad, and questionable nodes. The sort order matter.
analyze_node_state(UnsortedNodes) ->
    Nodes = lists:sort(UnsortedNodes), %% Force stable order
    GoodNodes = [N || {N, good} <- Nodes],
    BadNodes = lists:sort([N || {N, bad} <- Nodes]),
    QNodes = [{N,T} || {N, {questionable, T}} <- Nodes],
    QSorted = [N || {N, _} <- lists:keysort(2, QNodes)],
    analyze_node_state(BadNodes, GoodNodes, QSorted).
    
%% Provide a view-type for the analyzed node state
analyze_node_state(_Bs, Gs, _Qs) when length(Gs) == ?K -> range_full;
analyze_node_state(Bs ,Gs, Qs) when length(Bs) + length(Gs) + length(Qs) < ?K -> room;
analyze_node_state([B|_], _Gs, _Qs) -> {bad, B};
analyze_node_state([], _, [Q | _Qs]) -> {questionable, Q}.
        
%% Insertion requests the current bucket members, then analyzes the state of the bucket.
%% There are 4 possible cases:
%% • The bucket is full of good nodes—ignore the new node
%% • The bucket has room for another node—insert the new node
%% • The bucket has at least one bad node—swap the new node for the bad node
%% • The bucket has no bad nodes, but a questionable node—verify responsiveness
%%		of the questionable node (by means of interaction with the caller of
%%		insert_node/1)
%%
insert_node_gs_callouts(_S, [Node]) ->
    ?MATCH(MState, ?CALLOUT(dht_routing_meta, member_state, [Node, 'META'],
        oneof([unknown, member, roaming_member]))),
    case MState of
        member -> ?RET(already_member);
        roaming_member -> ?RET(already_member); %% TODO: For now. I'm not sure this is right.
        unknown -> ?APPLY(adjoin_node, [Node])
    end.
    
insert_node_gs_features(_S, _A, _R) -> [{state, insert_node_gs}].

%% Internal helper call for adjoining a new node
adjoin_node_callouts(_S, [Node]) ->
    ?MATCH(Near, ?CALLOUT(dht_routing_meta, range_members, [Node, 'META'],
       bucket_members())),
    ?MATCH(NodeState, ?CALLOUT(dht_routing_meta, node_state, [Near, 'META'],
        g_node_state(Near))),
    R = analyze_node_state(NodeState),
    case R of
        range_full ->
            ?MATCH(IR, ?CALLOUT(dht_routing_meta, insert, [Node, 'META'],
                    oneof([ok, not_inserted]))),
            case IR of
                ok -> ?RET(ok);
                not_inserted -> ?RET(not_inserted)
            end;
        room ->
            ?CALLOUT(dht_routing_meta, insert, [Node, 'META'], {ok, 'META'}),
            ?RET(ok);
        {bad, Bad} ->
            ?CALLOUT(dht_routing_meta, replace, [Bad, Node, 'META'], {ok, 'META'}),
            ?RET(ok);
        {questionable, Q} -> ?RET({verify, Q})
    end.

%% MODEL CLEANUP
%% ------------------------------
reset() ->
	case whereis(dht_state) of
	    undefined -> ok;
	    Pid when is_pid(Pid) ->
	        exit(Pid, kill),
	        timer:sleep(1)
	end,
	ok.

%% PROPERTY
%% -----------------------
postcondition_common(S, Call, Res) ->
    eq(Res, return_value(S, Call)).

weight(_S, request_success) -> 15;
weight(_S, _) -> 1.

prop_component_correct() ->
    ?SETUP(fun() ->
        eqc_mocking:start_mocking(api_spec()),
        fun() -> ok end
    end,
    ?FORALL(ID, dht_eqc:id(),
    ?FORALL(StartState, gen_state(ID),
    ?FORALL(Cmds, commands(?MODULE, StartState),
        begin
            ok = reset(),
            {H,S,R} = run_commands(?MODULE, Cmds),
        pretty_commands(?MODULE, Cmds, {H,S,R},
            aggregate(with_title('Commands'), command_names(Cmds),
            collect(eqc_lib:summary('Length'), length(Cmds),
            aggregate(with_title('Features'), eqc_statem:call_features(H),
            features(eqc_statem:call_features(H),
                R == ok)))))
        end)))).

%% Helper for showing states of the output:
t() -> t(5).

t(Secs) ->
    eqc:quickcheck(eqc:testing_time(Secs, eqc_statem:show_states(prop_component_correct()))).

%% INTERNAL MODEL HELPERS
%% -----------------------

initialized(#state { init = I }) -> I.
