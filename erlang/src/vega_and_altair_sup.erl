%%%-------------------------------------------------------------------
%% @doc vega and altair top level supervisor.
%%
%% @end
%%%-------------------------------------------------------------------

-module(vega_and_altair_sup).

-behaviour(supervisor).

-export([start_link/0]).

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
    SupFlags = #{strategy => one_for_one,
                 intensity => 100,
                 period => 1},
    Router     = {belka_router, {belka_router, start_link, []},
                    permanent, 5000, worker, [belka_router]},
    Admin      = {altair_admin, {altair_admin, start_link, []},
                    permanent, 5000, worker, [altair_admin]},
    Lovers     = {altair_lovers, {altair_lovers, start_link, []},
                    permanent, 5000, worker, [altair_lovers]},
    Templates  = {belka_templates, {belka_templates, start_link, []},
                    permanent, 5000, worker, [belka_templates]},
    CourtinSup = {vega_courtin_sup, {vega_courtin_sup, start_link, []},
                    permanent, 5000, supervisor, [vega_courtin_sup]},
    ChildSpecs = [
                    Router,
                    Admin,
                    Lovers,
                    Templates,
                    CourtinSup
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions


