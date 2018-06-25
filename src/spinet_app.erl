%%%-------------------------------------------------------------------
%% @doc spinet public API
%% @end
%%%-------------------------------------------------------------------

-module(spinet_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, StartArgs) ->
    ok = logger:add_handler(my_standard_h, logger_std_h,
                #{level => info, filter_default => log,
                  logger_std_h =>
                  #{type => {file, "./system_info.log"},
                    filesync_repeat_interval => 1000}}),
    logger:info("Starting spinet, Args=~p", [StartArgs]),
    spinet_sup:start_link(StartArgs).

%%--------------------------------------------------------------------
stop(_State) -> ok.

%%====================================================================
%% Internal functions
%%====================================================================
