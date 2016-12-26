%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%     Si4703 FM Radio Receiver
%%% @end
%%% Created : 26 Dec 2016 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(i2c_si4703).

-behaviour(gen_server).

-include_lib("i2c/include/i2c.hrl").

%% API
-export([start/0]).
-export([start_link/0]).
-export([power_on/0]).
-export([set_channel/1]).
-export([set_volume/1]).
-export([seek_up/0]).
-export([seek_down/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	  regs :: tuple()    %% current value of the 16 registers
	 }).

%% using pins 
%% 1  - 3.3V
%% 3  - I2C data  GPIO 8
%% 5  - I2C clock GPIO 9
%% 9  - GND
%% 11 - GPIO1     GPIO 0
%% 13 - GPIO2     GPIO 2
%% 15 - RESET     GPIO 3
%% 17 - SEN=3.3V (I2C enable)
%%
-define(RESET_PIN, 3).
-define(SDIO_PIN,  8).
-define(GPIO1_PIN, 0).
-define(GPIO2_PIN, 2).

-define(I2C_BUSID, 1).

%%
-define( FAIL, 0).
-define( SUCCESS, 1).

%% 0b._001.0000 = I2C address of Si4703 - 
%% note that the Wire function assumes non-left-shifted I2C address, 
%% not 0b.0010.000W
-define(SI4703, 16#10).

%% This is the number of attempts we will try to 
%% contact the device before erroring out
-define(I2C_FAIL_MAX, 10). 

%% Direction used for seeking. Default is down
-define(SEEK_DOWN, 0).
-define(SEEK_UP, 1).

%% Define the register names
-define(DEVICEID,   0).
-define(CHIPID,     1).
-define(POWERCFG,   2).
-define(CHANNEL,    3).
-define(SYSCONFIG1, 4).
-define(SYSCONFIG2, 5).
-define(STATUSRSSI, 10).
-define(READCHAN,   11).
-define(RDSA,       12).
-define(RDSB,       13).
-define(RDSC,       14).
-define(RDSD,       15).

%% Register 2 - POWERCFG
-define(SMUTE,  15).
-define(DMUTE,  14).
-define(SKMODE, 10).
-define(SEEKUP, 9).
-define(SEEK,   8).

%% Register 3 - CHANNEL
-define(TUNE, 15).

%% Register 4 - SYSCONFIG1
-define(RDS, 12).
-define(DE,  11).

%% Register 5 - SYSCONFIG2
-define(SPACE1, 5).
-define(SPACE0, 4).

%% Register 10 - STATUSRSSI
-define(RDSR,   15).
-define(STC,    14).
-define(SFBL,   13).
-define(AFCRL,  12).
-define(RDSS,   11).
-define(STEREO, 8).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(i2c),
    application:start(gpio),
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).    

start_link() ->
    application:start(i2c),
    application:start(gpio),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

power_on() ->
    gen_server:call(?SERVER, power_on).

set_channel(Channel) ->
    gen_server:call(?SERVER, {set_channel, Channel}).

seek_up() ->
    gen_server:call(?SERVER, seek_up).

seek_down() ->
    gen_server:call(?SERVER, seek_down).

set_volume(Volume) ->
    gen_server:call(?SERVER, {set_volume, Volume}).    

    
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


%% To get the Si4703 inito 2-wire mode, SEN needs to be high and 
%% SDIO needs to be low after a reset.
%% The breakout board has SEN pulled high, but also has SDIO pulled high. 
%% Therefore, after a normal power up. The Si4703 will be in an unknown state.
%% RST must be controlled
init([]) ->
    gpio:init(?GPIO1_PIN),
    gpio:init(?GPIO2_PIN),
    gpio:init(?RESET_PIN),
    gpio:init(?SDIO_PIN),
    gpio:output(?RESET_PIN),
    gpio:output(?SDIO_PIN),
    gpio:input(?GPIO1_PIN),
    gpio:input(?GPIO2_PIN),

    %% perform reset and set  i2c interface
    gpio:clr(?SDIO_PIN),
    gpio:clr(?RESET_PIN),
    timer:sleep(1),
    gpio:set(?RESET_PIN),
    timer:sleep(1),

    ok = i2c:open(?I2C_BUSID),

    Regs = read_registers(?I2C_BUSID),
    Regs1 = set_reg(7, Regs, 16#8100),  %% Enable the oscillator
    write_registers(?I2C_BUSID, Regs1),

    timer:sleep(500),

    Regs2 = read_registers(?I2C_BUSID),
    Regs3 = set_reg(?POWERCFG,  Regs2, 16#4001),
    Regs4 = or_reg(?SYSCONFIG1, Regs3,  (1 bsl ?RDS)),  %% Enable RDS
    Regs5 = or_reg(?SYSCONFIG1, Regs4,  (1 bsl ?DE)),   %% 50kHz Europe setup
    Regs6 = and_reg(?SYSCONFIG2, Regs5, 16#fff0),      %% 50kHz Europe setup

    write_registers(?I2C_BUSID, Regs6),

    timer:sleep(110),

    {ok, #state{ regs = Regs6 }}.

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

%% Freq(MHz) = 0.200(in USA) * Channel + 87.5MHz
%% 97.3 = 0.2 * Chan + 87.5
%% 9.8 / 0.2 = 49

handle_call(power_on, _From, State) ->
    {reply, ok, State};
handle_call({set_channel, Channel}, _From, State) ->
    NewChannel = Channel * 10,        %% 973 * 10 = 9730
    NewChannel1 = NewChannel - 8750,  %% 9730 - 8750 = 980
    NewChannel2 = NewChannel1 div 10, %% 980 / 10 = 98

    Regs1 = and_reg(?CHANNEL, State#state.regs, 16#FE00),
    Regs2 = or_reg(?CHANNEL, Regs1, NewChannel2),
    Regs3 = or_reg(?CHANNEL, Regs2, (1 bsl ?TUNE)),
    write_registers(?I2C_BUSID, Regs3),

    %% 1. wait for STC to be set in  STATUSRSSI

    %% 2. clear TUNE in CHANNEL
    
    %% 3. wait for STC to be cleared in STATUSRSSI

    {reply, ok, State#state { regs = Regs3 }};
handle_call(seek_up, _From, State) ->
    {reply, ok, State};
handle_call(seek_down, _From, State) ->
    {reply, ok, State};
handle_call({set_volume,Volume}, _From, State) when is_integer(Volume) ->
    Vol = if Volume < 0 -> 0;
	     Volume > 15 -> 15;
	     Volume -> Volume
	  end,
    Regs1 = and_reg(?SYSCONFIG2, State#state.regs, 16#fff0),
    Regs2 = or_reg(?SYSCONFIG2, Regs1, Vol),
    write_registers(?I2C_BUSID, Regs2),
    {reply, ok, State#state { regs = Regs2 }};
handle_call(_Request, _From, State) ->
    {reply, {error,bad_request}, State}.

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

read_registers(Bus) ->
    Cmd = [#i2c_msg { addr=?SI4703,flags=[rd],len=32,data=(<<>>) } ],
    case i2c:rdwr(Bus,Cmd) of
        {ok,Bytes} ->
	    io:format("read_registers Bytes = ~p\n", [Bytes]),
	    case erlang:iolist_to_binary(Bytes) of
		<<R10:16,R11:16,R12:16,R13:16,R14:16,R15:16,
		  R0:16,R1:16,R2:16,R3:16,R4:16,R5:16,R6:16,R7:16,R8:16,R9:16>> ->
		    Regs = {R0,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15},
		    io:format(" Regs = ~p\n", [Regs]),
		    {ok,Regs}
	    end;
	Error -> Error
    end.

write_registers(Bus,Regs={_R0,_R1,R2,R3,R4,R5,R6,R7,_R8,_R9,_R10,_R11,_R12,_R13,_R14,_R15}) ->
    Data = <<R2:16,R3:16,R4:16,R5:16,R6:16,R7:16>>,
    io:format("write Regs = ~p\n", [Regs]),
    io:format("write Data = ~p\n", [Data]),
    Cmd = [#i2c_msg { addr=?SI4703,flags=[],len=12,data=Data } ],
    case i2c:rdwr(Bus,Cmd) of
        {ok,_} ->
	    io:format("write Data ok\n", []),
	    ok;
	Error -> 
	    io:format("write error ~p\n", [Error]),
	    Error
    end.    
    
set_reg(Reg, Regs, Elem) ->
    setelement(Reg+1, Regs, Elem).

or_reg(Reg, Regs, Elem) ->
    setelement(Reg+1, Regs, Elem bor element(Reg+1, Regs)).

and_reg(Reg, Regs, Elem) ->
    setelement(Reg+1, Regs, Elem band element(Reg+1, Regs)).
