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

%% debug
-export([read_registers/2]).
-export([write_registers/3]).
-export([update_registers/3]).
-export([set_volume_/3]).
-export([set_channel_/3]).
-export([clear_tune_/2]).
-export([status_rssi/2]).
-export([status_tune_complete/2]).
-export([status_seek_complete/2]).
-export([mute_/3]).
-export([read_channel_/2]).

-export([set_reg/3]).
-export([and_reg/3]).
-export([or_reg/3]).
-export([set_bits/3]).
-export([clr_bits/3]).
-export([init_pins/0]).
-export([reset/0]).
-export([setup/0]).

-define(SERVER, ?MODULE).

-record(state, {
	  regs :: tuple()    %% current value of the 16 registers
	 }).

%% using pins 
%% 1  - 3.3V
%% 3  - I2C data  GPIO 2
%% 5  - I2C clock GPIO 3
%% 9  - GND
%% 11 - GPIO1     GPIO 17
%% 13 - GPIO2     GPIO 27
%% 15 - RESET     GPIO 22
%% 17 - SEN=3.3V (I2C enable)
%%
-define(RESET_PIN, 22).
-define(GPIO1_PIN, 17).
-define(GPIO2_PIN, 27).
-define(SDIO_PIN,  2).

-define(I2C_BUSID, 1).

%%
-define( FAIL, 0).
-define( SUCCESS, 1).

%% 0b._001.0000 = I2C address of Si4703 - 
%% note that the Wire function assumes non-left-shifted I2C address, 
%% not 0b.0010.000W
-define(SI4703, 16#10).
%% -define(SI4703, 16#20).

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
-define(SYSCONFIG3, 6).
-define(TEST1,      7).
-define(TEST2,      8).
-define(BOOTCONFIG, 9).
-define(STATUSRSSI, 10).
-define(READCHAN,   11).
-define(RDSA,       12).
-define(RDSB,       13).
-define(RDSC,       14).
-define(RDSD,       15).

%% DEVICEID(0)
-define(DEVICEID_PN, 12).    %% [12-15]
-define(DEVICEID_MFGID, 0).  %% [0-11]
-define(DEVICEID_PN_SI4702, 16#01).
-define(DEVICEID_PN_SI4703, 16#01).
-define(DEVICEID_MFGID_SILICON_LABS, 16#242).

%% CHIPID(1)
-define(CHIPID_REV, 10).     %% [10-15]
-define(CHIPID_DEV, 6).      %% [6-9]
-define(CHIPID_FIRMWARE, 0). %% [0-5]

-define(CHIPID_SI4702_EN_1, 16#1053).
-define(CHIPID_SI4702_EN_0, 16#1000).
-define(CHIPID_SI4703_EN_1, 16#1253).
-define(CHIPID_SI4703_EN_0, 16#1200).

%% POWERCFG(2)
-define(POWERCFG_DSMUTE, 15).
-define(POWERCFG_DMUTE,  14).
-define(POWERCFG_MONO,   13).
-define(POWERCFG_RDSM,   11).
-define(POWERCFG_SKMODE, 10).
-define(POWERCFG_SEEKUP,  9).
-define(POWERCFG_SEEK,    8).
-define(POWERCFG_DISABLE, 6).
-define(POWERCFG_ENABLE,  0).


%% CHANNEL(3)
-define(TUNE, 15).

%% SYSCONFIG1(4)
-define(SYSCONFIG1_RDSIEN, 15).
-define(SYSCONFIG1_STCIEN, 14).
-define(SYSCONFIG1_RDS,    12).
-define(SYSCONFIG1_DE,     11).
-define(SYSCONFIG1_AGCD,   10).
-define(SYSCONFIG1_BLNDADJ,  6). %% [6-7]
-define(SYSCONFIG1_GPIO3,    4). %% [4-5]
-define(SYSCONFIG1_GPIO2,    2). %% [2-3]
-define(SYSCONFIG1_GPIO1,    0). %% [0-1]

%% SYSCONFIG2(5)
-define(SEEKTH, 8).  %% [8-15]
-define(BAND,   6).  %% [6-7]
-define(SPACE0, 4).
-define(SPACE1, 5).
-define(VOLUME, 0).  %% [0-3]

%% STATUSRSSI(10)
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
    ok = i2c:open(?I2C_BUSID),
    timer:sleep(100),
    {ok,Regs} = read_registers(?I2C_BUSID,?SI4703),
    Regs1 = set_reg(?TEST1, Regs, 16#8100),  %% Enable the oscillator???
    write_registers(?I2C_BUSID,?SI4703,Regs1),
    
    timer:sleep(500),

    {ok,Regs2} = read_registers(?I2C_BUSID,?SI4703),
    Regs3 = set_reg(?POWERCFG,  Regs2,
		    (1 bsl ?POWERCFG_DMUTE) bor (1 bsl ?POWERCFG_ENABLE)),
    %% Enable RDS
    Regs4 = or_reg(?SYSCONFIG1, Regs3,
		   (1 bsl ?SYSCONFIG1_RDS)),
    Regs5 = or_reg(?SYSCONFIG1, Regs4,
		   (1 bsl ?SYSCONFIG1_DE)),  %% 50kHz Europe setup
    Regs6 = and_reg(?SYSCONFIG2, Regs5,
		    16#fff0),      %% Set Volume = 0

    write_registers(?I2C_BUSID,?SI4703,Regs6),

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
handle_call({set_channel, Channel}, _From, State) when is_number(Channel) ->
    %% 
    clear_tune_(?I2C_BUSID,?SI4703),
    timer:sleep(100),

    Regs = set_channel_(?I2C_BUSID,?SI4703,Channel),
    %% 1. wait for STC to be set in  STATUSRSSI
    %% 2. clear TUNE in CHANNEL
    %% 3. wait for STC to be cleared in STATUSRSSI
    {reply, ok, State#state { regs = Regs }};
handle_call(seek_up, _From, State) ->
    {reply, ok, State};
handle_call(seek_down, _From, State) ->
    {reply, ok, State};
handle_call({set_volume,Volume}, _From, State) when is_integer(Volume) ->
    Regs = set_volume_(?I2C_BUSID,?SI4703,Volume),
    {reply, ok, State#state { regs = Regs }};
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

setup() ->
    init_pins(),
    reset().

init_pins() ->
    gpio:init(?GPIO1_PIN),
    gpio:init(?GPIO2_PIN),
    gpio:init(?RESET_PIN),
    gpio:init(?SDIO_PIN),
    gpio:output(?RESET_PIN),
    gpio:output(?SDIO_PIN),
    gpio:input(?GPIO1_PIN),
    gpio:input(?GPIO2_PIN).

%% perform reset and set  i2c interface
reset() ->
    gpio:clr(?SDIO_PIN),
    timer:sleep(1),
    gpio:clr(?RESET_PIN),
    timer:sleep(1),
    gpio:set(?RESET_PIN),
    timer:sleep(1).

mute_(Bus, Addr, On) when is_boolean(On) ->
    update_registers(Bus, Addr, 
	    fun(Regs) ->
		    if On =:= true ->
			    and_reg(?POWERCFG, Regs, 
				    (bnot (1 bsl ?POWERCFG_DMUTE)));
		       On =:= false ->
			    or_reg(?POWERCFG, Regs, 
				   (1 bsl ?POWERCFG_DMUTE))
		    end
	    end).

set_volume_(Bus,Addr,Volume) when is_integer(Volume) ->
    update_registers(Bus, Addr, 
	    fun(Regs) ->
		    Vol = if Volume < 0 -> 0;
			     Volume > 15 -> 15;
			     true -> Volume
			  end,
		    Regs1 = and_reg(?SYSCONFIG2, Regs, 16#fff0),
		    Regs2 = or_reg(?SYSCONFIG2, Regs1, Vol),
		    Regs2
	    end).

clear_tune_(Bus,Addr) ->
    update_registers(Bus, Addr, 
	    fun(Regs) ->
		    clr_bits(?CHANNEL, Regs, (1 bsl ?TUNE))
	    end).

set_channel_(Bus,Addr,Channel) when is_number(Channel) ->
    update_registers(Bus,Addr,
	    fun(Regs) ->
		    NewChannel = trunc(Channel * 100),    %% 97.3 * 100 = 9730
		    NewChannel1 = NewChannel - 8750,  %% 9730 - 8750 = 980
		    NewChannel2 = NewChannel1 div 10, %% 980 / 10 = 98
		    Regs1 = and_reg(?CHANNEL, Regs, 16#FE00),
		    Regs2 = or_reg(?CHANNEL, Regs1, NewChannel2),
		    Regs3 = or_reg(?CHANNEL, Regs2, (1 bsl ?TUNE)),
		    %% set space 01 europe
		    Regs4 = clr_bits(?SYSCONFIG2, Regs3, (1 bsl ?SPACE1)),
		    Regs5 = set_bits(?SYSCONFIG2, Regs4, (1 bsl ?SPACE0)),
		    Regs5
	    end).

update_registers(Bus,Addr,Fun) ->
    {ok,Regs} = read_registers(Bus,Addr),
    Regs1 = Fun(Regs),
    write_registers(Bus,Addr,Regs1),
    Regs1.

status_rssi(Bus, Addr) ->
    {ok,Regs} = read_registers(Bus,Addr),
    get_reg(?STATUSRSSI, Regs) band 16#ff.

status_tune_complete(Bus, Addr) ->
    {ok,Regs} = read_registers(Bus,Addr),
    (get_reg(?STATUSRSSI, Regs) band (1 bsl ?STC)) /= 0.

status_seek_complete(Bus, Addr) ->
    {ok,Regs} = read_registers(Bus,Addr),
    (get_reg(?STATUSRSSI, Regs) band (1 bsl ?STC)) /= 0.

read_channel_(Bus,Addr) ->
    {ok,Regs} = read_registers(Bus,Addr),
    Chan = get_reg(?READCHAN, Regs) band 16#3ff,
    (Chan*10 + 8750) / 100.

read_registers(Bus,Addr) ->
    Cmd = [#i2c_msg { addr=Addr,flags=[rd],len=32,data=(<<>>) } ],
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

write_registers(Bus,Addr,Regs={_R0,_R1,R2,R3,R4,R5,R6,R7,_R8,_R9,_R10,_R11,_R12,_R13,_R14,_R15}) ->
    Data = <<R2:16,R3:16,R4:16,R5:16,R6:16,R7:16>>,
    io:format("write Regs = ~p\n", [Regs]),
    io:format("write Data = ~p\n", [Data]),
    Cmd = [#i2c_msg { addr=Addr,flags=[],len=12,data=Data } ],
    case i2c:rdwr(Bus,Cmd) of
        {ok,_} ->
	    io:format("write Data ok\n", []),
	    ok;
	Error -> 
	    io:format("write error ~p\n", [Error]),
	    Error
    end.    

or_reg(Reg, Regs, Elem) ->
    set_reg(Reg, Regs, Elem bor get_reg(Reg, Regs)).

and_reg(Reg, Regs, Elem) ->
    set_reg(Reg, Regs, Elem band get_reg(Reg, Regs)).

set_bits(Reg, Regs, Bits) ->
    or_reg(Reg, Regs, Bits).

clr_bits(Reg, Regs, Bits) ->
    and_reg(Reg, Regs, bnot Bits).


set_reg(Reg, Regs, Elem) ->
    setelement(Reg+1, Regs, Elem).

get_reg(Reg, Regs) ->
    element(Reg+1, Regs).
