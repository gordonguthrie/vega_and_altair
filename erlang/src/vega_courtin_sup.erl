%%%-------------------------------------------------------------------
%% @doc vega and altair supervisor for courtin
%%
%% @end
%%%-------------------------------------------------------------------

-module(vega_courtin_sup).

-behaviour(supervisor).

-export([
            start_link/0,
            add_lover/1,
            delete_lover/1
        ]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    io:format("Starting vega courtin sup~n"),
    SupFlags = #{strategy => one_for_one,
                 intensity => 100,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

add_lover(Id) ->
    ChildSpec = gen_child_spec(Id),
    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok,_}                        -> ok;
        {error, {already_started, _}} -> ok;
        Else                          -> Else
    end.

delete_lover(#{key := K}) ->
    ok = supervisor:terminate_child(?MODULE, K).

%% internal functions
gen_child_spec(#{key := K} = Id) ->
    {K, {vega_lover, start_link, [Id]},
     temporary, infinity, worker, [vega_lover]}.

