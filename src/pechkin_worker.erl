%%%-------------------------------------------------------------------
%% @author Lev Tonkikh <leonst998@gmail.com>
%% @doc
%%
%% @end
%%%-------------------------------------------------------------------
-module(pechkin_worker).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("pechkin.hrl").

-record(state, {
          id :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(I) when is_integer(I) ->
    gen_server:start_link(?MODULE, [I], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([I]) ->
    process_flag(trap_exit, true),

    Name = ?TELEGAM_WORKER(I),
    gproc_pool:connect_worker(?TELEGRAM_WORKERS, Name),

    {ok, #state{id = I}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Message, State) when is_record(Message, ?TELEGRAM_MESSAGE) ->

    RootUrl = <<"https://api.telegram.org/bot">>,
    Method  = <<"/sendMessage?chat_id=">>,
    Params  = <<"&text=">>,
    ApiKey  = Message#?TELEGRAM_MESSAGE.apikey,
    Channel = Message#?TELEGRAM_MESSAGE.channel,
    Text    = Message#?TELEGRAM_MESSAGE.text,

    Url     = <<RootUrl/bitstring,
                ApiKey/bitstring,
                Method/bitstring,
                Channel/bitstring,
                Params/bitstring,
                Text/bitstring>>,

    Request = {binary_to_list(Url), []},
    HTTPOpt = [],
    Options = [{sync, false}],  %% here we not use receiver

    case httpc:request(get, Request, HTTPOpt, Options) of
        {ok, _RequestId} ->
            ok;
        {error, Reason} ->
            io:format("error: ~p", [Reason])
    end,

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
