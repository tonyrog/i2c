%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    i2c interface module for TCA8414 keyboard scanner
%%% @end
%%% Created : 24 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(i2c_tca8418).

-export([open/1, open/2]).
-export([open1/1, open1/2]).

-export([read_keys/1]).
-export([read_key_events/1, read_key_events/2]).
-export([read_byte/2, write_byte/3]).
-export([configure3x3/1]).


%% default slave address
-define(I2C_ADDR_TCA, 16#34).

%% register definition
-define(CFG, 16#01).                %% interrupt processor interrupt
-define(INT_STAT, 16#02).           %% Interrupt status register
-define(KEY_LCK_EC, 16#03).         %% Key lock and event counter register).
-define(KEY_EVENT_A, 16#04).        %% Key event register A
-define(KEY_EVENT_B, 16#05).        %% Key event register B
-define(KEY_EVENT_C, 16#06).        %% Key event register C
-define(KEY_EVENT_D, 16#07).        %% Key event register D
-define(KEY_EVENT_E, 16#08).        %% Key event register E
-define(KEY_EVENT_F, 16#09).        %% Key event register F
-define(KEY_EVENT_G, 16#0A).        %% Key event register G
-define(KEY_EVENT_H, 16#0B).        %% Key event register H
-define(KEY_EVENT_I, 16#0C).        %% Key event register I
-define(KEY_EVENT_J, 16#0D).        %% Key event register J
-define(KP_LCK_TIMER, 16#0E).       %%  Keypad lock 1 to lock 2 timer
-define(UNLOCK1, 16#0F).            %% Unlock key 1
-define(UNLOCK2, 16#10).            %% Unlock key2
-define(GPIO_INT_STAT1, 16#11).     %% GPIO interrupt status
-define(GPIO_INT_STAT2, 16#12).     %% GPIO interrupt status
-define(GPIO_INT_STAT3, 16#13).     %% GPIO interrupt status
-define(GPIO_DAT_STAT1, 16#14).     %% GPIO data status   
-define(GPIO_DAT_STAT2, 16#15).     %% GPIO data status
-define(GPIO_DAT_STAT3, 16#16).     %% GPIO data status
-define(GPIO_DAT_OUT1, 16#17).      %% GPIO data out
-define(GPIO_DAT_OUT2, 16#18).      %% GPIO data out
-define(GPIO_DAT_OUT3, 16#19).      %% GPIO data out
-define(GPIO_INT_EN1, 16#1A).       %% GPIO interrupt enable
-define(GPIO_INT_EN2, 16#1B).       %% GPIO interrupt enable
-define(GPIO_INT_EN3, 16#1C).       %% GPIO interrupt enable
-define(KP_GPIO1, 16#1D).
-define(KP_GPIO2, 16#1E).
-define(KP_GPIO3, 16#1F).
-define(GPI_EM1, 16#20).            %% GPI event mode 1
-define(GPI_EM2, 16#21).            %% GPI event mode 2
-define(GPI_EM3, 16#22).            %% GPI event mode 3
-define(GPIO_DIR1, 16#23).          %% GPIO data direction 1
-define(GPIO_DIR2, 16#24).          %% GPIO data direction 2
-define(GPIO_DIR3,16#25).           %% GPIO data direction 3
-define(GPIO_INT_LVL1, 16#26).      %% GPIO edge/level detect 1
-define(GPIO_INT_LVL2, 16#27).      %% GPIO edge/level detect 2
-define(GPIO_INT_LVL3, 16#28).      %% GPIO edge/level detect 3
-define(DEBOUNCE_DIS1, 16#29).
-define(DEBOUNCE_DIS2, 16#2A).
-define(DEBOUNCE_DIS3, 16#2B).
-define(GPIO_PULL1, 16#2C).
-define(GPIO_PULL2, 16#2D).
-define(GPIO_PULL3, 16#2E).

%% CFG flags
-define(AI,           16#80).
-define(GPI_E_CGF,    16#40).
-define(OVR_FLOW_M,   16#20).
-define(INT_CFG,      16#10).
-define(OVR_FLOW_IEN, 16#08).
-define(K_LCK_IEN,    16#04).
-define(GPI_IEN,      16#02).
-define(KE_IEN,       16#01).

%% INT_STAT - interrupt status flags
%%   CTRL-ALT-DEL
-define(CAD_INT,      16#10).
%%   Overflow interrupt status. Requires writing a 1 to clear interrupts.
-define(OVR_FLOW_INT, 16#08). 
%%   Keypad lock interrupt status. This is the interrupt to the processor
%%   when the keypad lock sequence is started. Requires writing a 1 to 
%%   clear interrupts.
-define(K_LCK_INT,    16#04). 
%%   GPI interrupt status. Requires writing a 1 to clear interrupts.
-define(GPI_INT,      16#02).
%%   Key events interrupt status. Requires writing a 1 to clear interrupts.
-define(K_INT,        16#01).

open(Bus) ->
    open(Bus, ?I2C_ADDR_TCA).
open(Bus, Addr) ->
    i2c:open(Bus),
    i2c:set_slave(Bus, Addr),
    init(Bus),
    ok.

open1(Bus) ->
    open1(Bus, ?I2C_ADDR_TCA).
open1(Bus, Addr) ->
    {ok,Port} = i2c:open1(Bus),
    i2c:set_slave(Port, Addr),
    init(Bus),
    {ok,Port}.

configure3x3(Bus) ->
    write_byte(Bus, ?KP_GPIO1, 16#07),
    write_byte(Bus, ?KP_GPIO2, 16#07),
    write_byte(Bus, ?KP_GPIO3, 16#00),
    write_byte(Bus, ?CFG, 16#95),
%%    write_byte(Bus, ?UNLOCK1, 16#21),  %% key=33
%%    write_byte(Bus, ?UNLOCK2, 16#01),  %% key=1
%%    write_byte(Bus, ?KP_LCK_TIMER, 16#52),
    ok.


init(_Bus) ->
    ok.

read_keys(Bus) ->
    IntStat = read_byte(Bus, ?INT_STAT),
    if IntStat band (?GPI_INT bor ?K_INT) =/= 0 ->
	    Keys = read_key_events(Bus),
	    write_byte(Bus, ?INT_STAT, IntStat band (?GPI_INT bor ?K_INT)),
	    Keys;
       true ->
	    []
    end.

read_key_events(Bus) ->
    read_key_events(Bus,[]).

read_key_events(Bus, Acc) ->
    case read_byte(Bus, ?KEY_LCK_EC) band 16#0f of
	0 -> 
	    lists:reverse(Acc);
	_N ->
	    Key = read_byte(Bus, ?KEY_EVENT_A),
	    KeyStat = if Key band 16#80 =/= 0 -> press; true -> release end,
	    read_key_events(Bus, [{KeyStat,Key band 16#7f}|Acc])
    end.

read_byte(Bus, Command) ->
    {ok,Byte} = i2c:smbus_read_byte_data(Bus, Command),
    Byte.

write_byte(Bus, Command, Byte) when Byte band (bnot 16#ff) =:= 0 ->
    {ok,_} = i2c:smbus_write_byte_data(Bus, Command, Byte),
    ok.
