-module(gen_smtpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_mail_client/5]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	io:format("gen_smtpc_sup started ... ~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_mail_client({From, FromPassword}, To, Subject, Body, Options) ->
	% start mail client
    supervisor:start_child(?MODULE, [{From, FromPassword}, To, Subject, Body, Options]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Process = {gen_smtpc, 
              {gen_smtpc, start_link, []},
               temporary, 2000, worker, []
              },
    
    {ok,{{simple_one_for_one, 10, 60}, [Process]}}.