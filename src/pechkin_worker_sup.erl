%%%-------------------------------------------------------------------
%%% @author Lev Tonkikh <leonst998@gmail.com>
%% @doc
%%
%% @end
%%%-------------------------------------------------------------------
-module(pechkin_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("log.hrl").
-include("pechkin.hrl").

-define(SERVER, ?MODULE).

-define(CHILD(ID, Module, Opts, Type), #{id       => ID,
                                         start    => {Module, start_link, Opts},
                                         restart  => permanent,
                                         shutdown => 5000,
                                         type     => Type,
                                         modules  => [Module]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags   = #{strategy  => one_for_one,
                   intensity => 1,
                   period    => 5},

    WorkersCount = case application:get_env(pechkin, max_workers, none) of
                       N when N == ""; N == none ->
                           erlang:system_info(schedulers);
                       N when is_integer(N) ->
                           N
                   end,

    ok = gproc_pool:new(?TELEGRAM_WORKERS, round_robin, [{size, WorkersCount}]),

    ?INFO("Create pool for ~p workers", [WorkersCount]),

    ChildSpecs = lists:map(
                   fun(I) ->
                           Worker = ?TELEGAM_WORKER(I),

                           gproc_pool:add_worker(?TELEGRAM_WORKERS, Worker, I),

                           ?CHILD(Worker, pechkin_worker, [I], worker)

                   end, lists:seq(1, WorkersCount)),

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
