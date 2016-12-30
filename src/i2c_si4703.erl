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
-export([dump_registers/2]).
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
-export([get_reg/2]).
-export([init_pins/0]).
-export([reset/0]).
-export([setup/0]).

-define(SERVER, ?MODULE).

-record(state, {
	  regs :: tuple()    %% current value of the 16 registers (bitstrings)
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
    Regs1 = set_field(?TEST1,Regs,xoscen,1),  %% Enable the oscillator
    write_registers(?I2C_BUSID,?SI4703,Regs1),
    
    timer:sleep(500),

    {ok,Regs2} = read_registers(?I2C_BUSID,?SI4703),
    Regs3 = set_fields(?POWERCFG,Regs2,[{dmute,1},{enable,1}]),
    %% Enable RDS, DE=1 -> 50kHz Europe setup
    Regs4 = set_fields(?SYSCONFIG1,Regs3,[{rdsien,1},{stcien},{rds,1},{de,1}]),
    Regs5 = set_fields(?SYSCONFIG2,Regs4,[{volume,0}]),
    write_registers(?I2C_BUSID,?SI4703,Regs5),

    timer:sleep(110),

    gpio:set_interrupt(?GPIO2_PIN, falling),

    {ok, #state{ regs = Regs5 }}.

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

handle_info({gpio_interrupt,0,?GPIO2_PIN,_Value}, State) ->
    %% either STC or RDSR are set
    {ok,Regs} = read_registers(?I2C_BUSID,?SI4703),
    Ps = decode_register(?STATUSRSSI, get_reg(?STATUSRSSI, Regs)),
    io:format("interrupt: ~p\n", [Ps]),
    %% SHOULD do some stuff here and then set SEEK in POWERCFG or
    %% TUNE in CHANNEL this will clear STC
    {noreply, State#state { regs = Regs }};
handle_info(_Info, State) ->
    io:format("handle_info: got ~p\n", [_Info]),
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
			    set_field(?POWERCFG,Regs,dmute,0);
		       On =:= false ->
			    set_field(?POWERCFG,Regs,dmute,1)
		    end
	    end).

set_volume_(Bus,Addr,Volume) when is_integer(Volume) ->
    update_registers(Bus, Addr, 
	    fun(Regs) ->
		    Vol = if Volume < 0 -> 0;
			     Volume > 15 -> 15;
			     true -> Volume
			  end,
		    set_field(?SYSCONFIG2,Regs,volume,Vol)
	    end).

clear_tune_(Bus,Addr) ->
    update_registers(Bus, Addr, 
	    fun(Regs) ->
		    set_field(?CHANNEL,Regs,tune,0)
	    end).

set_channel_(Bus,Addr,Channel) when is_number(Channel) ->
    update_registers(Bus,Addr,
	    fun(Regs) ->
		    Chan = (trunc(Channel * 100) - 8750) div 10,
		    Regs1 = set_fields(?CHANNEL,Regs,[{channel,Chan},{tune,1}]),
		    set_fields(?SYSCONFIG2,Regs1,[{space,2#01}])
	    end).

update_registers(Bus,Addr,Fun) ->
    {ok,Regs} = read_registers(Bus,Addr),
    Regs1 = Fun(Regs),
    write_registers(Bus,Addr,Regs1),
    Regs1.

status_rssi(Bus, Addr) ->
    {ok,Regs} = read_registers(Bus,Addr),
    Ps = decode_register(?STATUSRSSI, get_reg(?STATUSRSSI, Regs)),
    [RSSI] = get_fields([rssi], Ps),
    RSSI.

status_tune_complete(Bus, Addr) ->
    {ok,Regs} = read_registers(Bus,Addr),
    {_,Ps} = decode_register(?STATUSRSSI, get_reg(?STATUSRSSI, Regs)),
    [STC] = get_fields([stc], Ps),
    STC /= 0.

status_seek_complete(Bus, Addr) ->
    {ok,Regs} = read_registers(Bus,Addr),
    {_,Ps} = decode_register(?STATUSRSSI, get_reg(?STATUSRSSI, Regs)),
    [STC] = get_fields([stc], Ps),
    STC /= 0.

read_channel_(Bus,Addr) ->
    {ok,Regs} = read_registers(Bus,Addr),
    {_,Ps} = decode_register(?CHANNEL, get_reg(?CHANNEL, Regs)),
    [CHANNEL] = get_fields([channel], Ps),
    (CHANNEL*10 + 8750) / 100.  %% FIXME: formula! read space and band

dump_registers(Bus,Addr) ->
    case read_registers(Bus,Addr) of
	{ok,Regs} ->
	    {ok, [decode_register(Reg, RegContent) ||
		     {Reg,RegContent} <- lists:zip(lists:seq(0,15),
						   tuple_to_list(Regs))]};
	Error -> Error
    end.

read_registers(Bus,Addr) ->
    Cmd = [#i2c_msg { addr=Addr,flags=[rd],len=32,data=(<<>>) } ],
    case i2c:rdwr(Bus,Cmd) of
        {ok,Bytes} ->
	    io:format("read_registers Bytes = ~p\n", [Bytes]),
	    case erlang:iolist_to_binary(Bytes) of
		<<R10:16/bitstring,R11:16/bitstring,R12:16/bitstring,
		  R13:16/bitstring,R14:16/bitstring,R15:16/bitstring,
		  R0:16/bitstring,R1:16/bitstring,R2:16/bitstring,
		  R3:16/bitstring,R4:16/bitstring,R5:16/bitstring,
		  R6:16/bitstring,R7:16/bitstring,R8:16/bitstring,
		  R9:16/bitstring>> ->
		    Regs = {R0,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15},
		    io:format(" Regs = ~p\n", [Regs]),
		    {ok,Regs}
	    end;
	Error -> Error
    end.

write_registers(Bus,Addr,Regs={_R0,_R1,R2,R3,R4,R5,R6,R7,_R8,_R9,_R10,_R11,_R12,_R13,_R14,_R15}) ->
    Data = <<R2:16/bitstring,R3:16/bitstring,R4:16/bitstring,
	     R5:16/bitstring,R6:16/bitstring,R7:16/bitstring>>,
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

%% decode registers into proplists
decode_register(?DEVICEID, <<PN:4, MFGID:12>>) ->
    {deviceid,[{pn,PN}, {mfgid, MFGID}]};
decode_register(?CHIPID,   <<REV:6, DEV:4, FIRMWARE:6>>) ->
    {chipid,[{rev, REV}, {dev,DEV}, {firmware, FIRMWARE}]};
decode_register(?POWERCFG, <<DSMUTE:1,DMUTE:1,MONO:1,_:1,RDSM:1,SKMODE:1,
			     SEEKUP:1, SEEK:1, _:1,DISABLE:1, _:5,ENABLE:1>>)->
    {powercfg,[{dsmute,DSMUTE}, {dmute,DMUTE},{mono,MONO},{rdsm,RDSM},
	       {skmode,SKMODE}, {seekup,SEEKUP}, {seek, SEEK}, 
	       {disable,DISABLE}, {enable, ENABLE}]};
decode_register(?CHANNEL,<<TUNE:1, _:5, CHANNEL:10>>) ->
    {channel,[{tune,TUNE}, {channel,CHANNEL}]};
decode_register(?SYSCONFIG1, <<RDSIEN:1,STCIEN:1,_:1,RDS:1,DE:1,AGCD:1,_:2,
			       BLNDADJ:2,GPIO3:2,GPIO2:2,GPIO1:2>>) ->
    {sysconfig1,
     [{rdsien,RDSIEN},{stcien,STCIEN},{rds,RDS},{de,DE},{agcd,AGCD},
      {blndadj,BLNDADJ},{gpio3,GPIO3},{gpio2,GPIO2},{gpio1,GPIO1}]};
decode_register(?SYSCONFIG2, <<SEEKTH:8, BAND:2, SPACE:2, VOLUME:4>>) ->
    {sysconfig1,
     [{seekth,SEEKTH},{'band',BAND},{space,SPACE},{volume,VOLUME}]};
decode_register(?SYSCONFIG3, <<SMUTER:2,SMUTEA:2,_:3,VOLEXT:1,
			       SKSNR:4,SKCNT:4>>) ->
    {sysconfig3,
     [{skmuter,SMUTER},{smutea,SMUTEA},{volext,VOLEXT},
      {sksnr,SKSNR},{skcnt,SKCNT}]};
decode_register(?TEST1, <<XOSCEN:1,AHIZEN:1,RESERVED:14>>) ->
    {test1,
     [{xoscen,XOSCEN},{ahizen,AHIZEN},{reserved,RESERVED}]};
decode_register(?TEST2, <<RESERVED:16>>) ->
    {test2,[{reserved,RESERVED}]};
decode_register(?BOOTCONFIG, <<RESERVED:16>>) ->
    {bootconfig,[{reserved,RESERVED}]};
decode_register(?STATUSRSSI, <<RDSR:1,STC:1,SF:1,AFCRL:1,RDSS:1,BLERA:2,
			       ST:1,RSSI:8>>) ->
    {statusrssi,
     [{rdsr,RDSR},{stc,STC},{sf,SF},{afcrl,AFCRL},{rdss,RDSS},{blera,BLERA},
      {st,ST},{rssi,RSSI}]};
decode_register(?READCHAN,<<BLERB:2,BLERC:2,BLERD:2,READCHAN:10>>) ->
    {readchan,
     [{blerb,BLERB},{blerc,BLERC},{blerd,BLERD},{readchan,READCHAN}]};
decode_register(?RDSA, <<RDSA:16>>) ->
    {rdsa,[{rdsa,RDSA}]};
decode_register(?RDSB, <<RDSB:16>>) ->
    {rdsb,[{rdsb,RDSB}]};
decode_register(?RDSC, <<RDSC:16>>) ->
    {rdsc,[{rdsc,RDSC}]};
decode_register(?RDSD, <<RDSD:16>>) ->
    {rdsd,[{rdsd,RDSD}]}.

encode_register(?DEVICEID, New, Old) ->
    [PN,MFGID] = get_fields([pn,mfgid],update_fields(New, Old)),
    <<PN:4, MFGID:12>>;
encode_register(?CHIPID, New, Old) ->
    [REV, DEV, FIRMWARE] = get_fields([rev,dev,firmware],
				      update_fields(New, Old)),
    <<REV:6, DEV:4, FIRMWARE:6>>;
encode_register(?POWERCFG, New, Old) ->
    [DSMUTE,DMUTE,MONO,RDSM,SKMODE,
     SEEKUP, SEEK, DISABLE, ENABLE] = get_fields(
					[dsmute,dmute,mono,rdsm,skmode,
					 seekup,seek,disable,enable],
					update_fields(New, Old)),
    <<DSMUTE:1,DMUTE:1,MONO:1,0:1,RDSM:1,SKMODE:1,
      SEEKUP:1, SEEK:1, 0:1,DISABLE:1, 0:5,ENABLE:1>>;
encode_register(?CHANNEL, New, Old) ->
    [TUNE,CHANNEL] = get_fields([tune,channel],
				update_fields(New, Old)),
    <<TUNE:1, 0:5, CHANNEL:10>>;
encode_register(?SYSCONFIG1, New, Old) ->
    [RDSIEN,STCIEN,RDS,DE,AGCD,BLNDADJ,GPIO3,GPIO2,GPIO1] =
	get_fields([rdsien,stcien,rds,de,agcd,blndadj,gpio3,gpio2,gpio1],
		   update_fields(New, Old)),
    <<RDSIEN:1,STCIEN:1,0:1,RDS:1,DE:1,AGCD:1,0:2,
      BLNDADJ:2,GPIO3:2,GPIO2:2,GPIO1:2>>;
encode_register(?SYSCONFIG2, New, Old) ->
    [SEEKTH,BAND,SPACE,VOLUME] = 
	get_fields([seekth,'band',space,volume],
		   update_fields(New, Old)),
    <<SEEKTH:8, BAND:2, SPACE:2, VOLUME:4>>;
encode_register(?SYSCONFIG3, New, Old) ->
    [SMUTER,SMUTEA,VOLEXT,SKSNR,SKCNT] =
	get_fields([skmuter,smutea,volext,sksnr,skcnt],
		   update_fields(New, Old)),
    <<SMUTER:2,SMUTEA:2,0:3,VOLEXT:1,SKSNR:4,SKCNT:4>>;
encode_register(?TEST1, New, Old) ->
    [XOSCEN,AHIZEN,RESERVED] = get_fields([xoscen,ahizen,reserved], 
					  update_fields(New, Old)),
    <<XOSCEN:1,AHIZEN:1,RESERVED:14>>;
encode_register(?TEST2, New, Old) ->
    [RESERVED] = get_fields([reserved], update_fields(New, Old)),
    <<RESERVED:16>>;
encode_register(?BOOTCONFIG, New, Old) ->
    [RESERVED] = get_fields([reserved], update_fields(New, Old)),
    <<RESERVED:16>>;
encode_register(?STATUSRSSI, New, Old) ->
    [RDSR,STC,SF,AFCRL,RDSS,BLERA,ST,RSSI] =
	get_fields([rdsr,stc,sf,afcrl,rdss,blera,st,rssi],
		   update_fields(New, Old)),
    <<RDSR:1,STC:1,SF:1,AFCRL:1,RDSS:1,BLERA:2,ST:1,RSSI:8>>;
encode_register(?READCHAN, New, Old) ->
    [BLERB,BLERC,BLERD,READCHAN] = 
	get_fields([blerb,blerc,blerd,readchan],
		   update_fields(New, Old)),
    <<BLERB:2,BLERC:2,BLERD:2,READCHAN:10>>;
encode_register(?RDSA, New, Old) ->
    [RDSA] = get_fields([rdsa], update_fields(New, Old)),
    <<RDSA:16>>;
encode_register(?RDSB, New, Old) ->
    [RDSB] = get_fields([rdsb], update_fields(New, Old)),
    <<RDSB:16>>;
encode_register(?RDSC, New, Old) ->
    [RDSC] = get_fields([rdsc], update_fields(New, Old)),
    <<RDSC:16>>;
encode_register(?RDSD, New, Old) ->
    [RDSD] = get_fields([rdsd], update_fields(New, Old)),
    <<RDSD:16>>.

get_fields([F|Fs], Ps) ->
    [proplists:get_value(F, Ps) |
     get_fields(Fs, Ps)];
get_fields([], _Ps) ->
    [].

update_fields([{Field,Value}|Fs], Ps) ->
    update_fields(Fs, update_field(Field, Value, Ps));
update_fields([], Ps) ->
    Ps.

update_field(Field, Value, [{Field,_Old}|Ps]) ->
    [{Field,Value}|Ps];
update_field(Field, Value, [P|Ps]) ->
    [P|update_field(Field,Value,Ps)].

set_field(Reg,Regs,Field,Value) ->
    set_fields(Reg,Regs,[{Field,Value}]).

set_fields(Reg,Regs,Fs) ->
    RegContentOld = get_reg(Reg, Regs),
    {_RegName,Fields} = decode_register(Reg,RegContentOld),
    RegContentNew = encode_register(Reg, Fs, Fields),
    set_reg(Reg, Regs, RegContentNew).

set_reg(Reg, Regs, Elem) ->
    setelement(Reg+1, Regs, Elem).

get_reg(Reg, Regs) ->
    element(Reg+1, Regs).

