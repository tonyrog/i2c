%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    i2c interface module for TCA8414 keyboard scanner
%%% @end
%%% Created : 24 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(i2c_tca8418).

-export([open/1, open/2]).
-export([open1/1, open1/2]).

%% generate key api
-export([read_events/1]).
-export([deq_events/1, deq_events/2]).
-export([read_byte/2, write_byte/3]).
-export([configure_3x3/1]).
-export([configure_4x3/1]).
-export([configure_lock/1]).
-export([keycode_to_sym/1]).
-export([sym_to_keycode/1]).
%% GPIO api
-export([gpio_init/2, gpio_get/2, gpio_set/2, gpio_clr/2]).
-export([gpio_input/2, gpio_output/2]).
-export([gpio_set_direction/3, gpio_get_direction/2]).
-export([gpio_set_interrupt/3, gpio_get_interrupt/2]). 
-export([gpio_read_port/2, gpio_write_port/3]).

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
-define(KP_GPIO1, 16#1D).           %% Keypad or GPIO selection
-define(KP_GPIO2, 16#1E).           %% Keypad or GPIO selection
-define(KP_GPIO3, 16#1F).           %% Keypad or GPIO selection
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

%% gpio port address index by pin group I
-define(GPIO_INT_STAT(I), (?GPIO_INT_STAT1+(I))).
-define(GPIO_DAT_STAT(I), (?GPIO_DAT_STAT1+(I))).
-define(GPIO_DAT_OUT(I),  (?GPIO_DAT_OUT1+(I))).
-define(GPIO_INT_EN(I),   (?GPIO_INT_EN1+(I))).
-define(KP_GPIO(I),       (?KP_GPIO1+(I))).
-define(GPI_EM(I),        (?GPIO_EM1+(I))).
-define(GPIO_DIR(I),      (?GPIO_DIR1+(I))).
-define(GPIO_INT_LVL(I),  (?GPIO_INT_LVL1+(I))).
-define(DEBOUNCE_DIS(I),  (?DEBOUNCE_DIS1+(I))).
-define(GPIO_PULL(I),     (?GPIO_PULL1+(I))).

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

%% KEY_LCK_EC - Key lock and event counter register
-define(KEY_EC(X), ((X) band 16#0f)).
-define(LCK1,      16#10).
-define(LCK2,      16#20).
-define(K_LCK_EN,  16#40).

%% KP_GPIO1/GPI_EM1..
-define(ROW0, 16#01).
-define(ROW1, 16#02).
-define(ROW2, 16#04).
-define(ROW3, 16#08).
-define(ROW4, 16#10).
-define(ROW5, 16#20).
-define(ROW6, 16#40).
-define(ROW7, 16#80).

%% KP_GPIO2/GPI_EM2...
-define(COL0, 16#01).
-define(COL1, 16#02).
-define(COL2, 16#04).
-define(COL3, 16#08).
-define(COL4, 16#10).
-define(COL5, 16#20).
-define(COL6, 16#40).
-define(COL7, 16#80).

%% KP_GPIO3/GPI_EM3...
-define(COL8, 16#01).
-define(COL9, 16#02).

-define(ROW(I), (1 bsl (I))).
-define(COL(I), (1 bsl ((I) band 7))).

-type posix() :: atom().
-type pin() :: 0..17 | {row,0..7} | {col,0..9}.
-type style() :: none | rising | falling.
-type event() :: {press,Key::integer()} | {release,Key::integer()} |
		 {gpio_interrupt,Port::0..2,Pin::0..17,Value::0..1}.

-spec open(Bus::i2c:i2c_bus()) ->
	  ok | {error, Reason::posix()}.

open(Bus) ->
    open(Bus, ?I2C_ADDR_TCA).
open(Bus, Addr) ->
    i2c:open(Bus),
    i2c:set_slave(Bus, Addr),
    init(Bus),
    ok.

-spec open1(Bus::i2c:i2c_bus()) ->
	  {ok,port()} | {error, Reason::posix()}.

open1(Bus) ->
    open1(Bus, ?I2C_ADDR_TCA).
open1(Bus, Addr) ->
    {ok,Port} = i2c:open1(Bus),
    i2c:set_slave(Port, Addr),
    init(Bus),
    {ok,Port}.

-define(KEYMAP_4x3,
	#{  1  => $1, 2  => $2, 3 => $3,
	    11 => $4, 12 => $5, 13 => $6,
	    21 => $7, 22 => $8, 23 => $9,
	    31 => $*, 32 => $0, 33 => $# }).

-define(KEYREVMAP_4x3,
	#{ $1 => 1,  $2 => 2,  $3 => 3,
	   $4 => 11, $5 => 12, $6 => 13,
	   $7 => 21, $8 => 22, $9 => 23,
	   $* => 31, $0 => 32, $# => 33 }).

-spec keycode_to_sym(Key::integer()) -> char().
keycode_to_sym(Key) ->
    maps:get(Key, ?KEYMAP_4x3).

-spec sym_to_keycode(char()) -> integer().
sym_to_keycode(Sym) ->
    maps:get(Sym, ?KEYREVMAP_4x3).

-spec configure_3x3(Bus::i2c:i2c_bus()) -> ok.
configure_3x3(Bus) ->
    write_byte(Bus, ?KP_GPIO1, ?ROW0 bor ?ROW1 bor ?ROW2),
    write_byte(Bus, ?KP_GPIO2, ?COL0 bor ?COL1 bor ?COL2),
    write_byte(Bus, ?KP_GPIO3, 16#00),
    write_byte(Bus, ?CFG, (?AI bor ?INT_CFG bor ?KE_IEN)),
    ok.

-spec configure_4x3(Bus::i2c:i2c_bus()) -> ok.
configure_4x3(Bus) ->
    write_byte(Bus, ?KP_GPIO1, ?ROW0 bor ?ROW1 bor ?ROW2 bor ?ROW3),
    write_byte(Bus, ?KP_GPIO2, ?COL0 bor ?COL1 bor ?COL2),
    write_byte(Bus, ?KP_GPIO3, 16#00),
    write_byte(Bus, ?CFG, (?AI bor ?INT_CFG bor ?KE_IEN)),
    ok.

-spec configure_lock(Bus::i2c:i2c_bus()) -> ok.	  
configure_lock(Bus) ->
    configure_lock(Bus, 33, 1, 2, 10).

-spec configure_lock(Bus::i2c:i2c_bus(),
		     Key1::integer(),
		     Key2::integer(),
		     LckTimer::0..7,
		     IntTimer::0..31) -> ok.
%% LckTimer and IntTimer are in seconds
configure_lock(Bus, Key1, Key2, LckTimer, IntTimer) when
      is_integer(LckTimer), (LckTimer >= 0),
      is_integer(IntTimer), (IntTimer >= 0) ->
    CFG = read_byte(Bus, ?CFG),
    write_byte(Bus, ?CFG, CFG bor ?K_LCK_IEN),
    write_byte(Bus, ?UNLOCK1, Key1),
    write_byte(Bus, ?UNLOCK2, Key2),
    write_byte(Bus, ?KP_LCK_TIMER,
	       min(LckTimer,7) bor (min(IntTimer,31) bsl 3)).

init(_Bus) ->
    %% nothing yet
    ok.

-spec read_events(Bus::i2c:i2c_bus()) ->
	  [event()].

read_events(Bus) ->
    IntStat = read_byte(Bus, ?INT_STAT),
    if IntStat band (?GPI_INT bor ?K_INT) =/= 0 ->
	    EventList = deq_events(Bus),
	    write_byte(Bus, ?INT_STAT, IntStat band (?GPI_INT bor ?K_INT)),
	    EventList;
       true ->
	    []
    end.

%% Note! Does not clear the interrupt! use read_keys for that
deq_events(Bus) ->
    deq_events(Bus,[]).

deq_events(Bus, Acc) ->
    case read_byte(Bus, ?KEY_LCK_EC) band 16#0f of
	0 -> 
	    lists:reverse(Acc);
	_N ->
	    EvA = read_byte(Bus, ?KEY_EVENT_A),
	    Key = EvA band 16#f7,
	    if Key >= 1, Key =< 80 ->
		    KeyState = if EvA band 16#80 =/= 0 -> press;
				  true -> release 
			       end,
		    deq_events(Bus, [{KeyState,Key}|Acc]);
	       Key >= 97, Key =< 104 ->  %% row R0..R7 interrupt
		    Value = EvA bsr 7,
		    Port = 0,
		    Pin  = Key-97,   %% may be feed into pin(I)
		    deq_events(Bus, [{gpio_interrupt,Port,Pin,Value}|Acc]);
	       Key >= 105, Key =< 114 -> %% row C0..C9 interrupt
		    Value = EvA bsr 7,
		    Pin0  = (Key-105),      %% 0..9
		    Port  = 1+(Pin0 bsr 3), %% 1..2
		    Pin   = 8+Pin0,         %% may be feed into pin(I)
		    deq_events(Bus, [{gpio_interrupt,Port,Pin,Value}|Acc]);
	       true ->
		    io:format("tca8418: unexpeced key: ~w\n", [Key]),
		    deq_events(Bus, Acc)
	    end
    end.



%% ensure that the pin is not in keypad mode
gpio_init(Bus, Pin) ->
    {PinBit,RegOffs} = pin(Pin),
    Reg = read_byte(Bus, ?KP_GPIO(RegOffs)),
    write_byte(Bus, ?KP_GPIO(RegOffs), Reg band (bnot PinBit)).

%% fix new api, return boolean
gpio_get(Bus, Pin) ->
    {PinBit,RegOffs} = pin(Pin),
    Reg = read_byte(Bus, ?GPIO_DAT_STAT(RegOffs)),
    case Reg band PinBit of
	0 -> {ok,0};
	_ -> {ok,1}
    end.

gpio_set(Bus, Pin) ->
    {PinBit,RegOffs} = pin(Pin),
    Reg = read_byte(Bus, ?GPIO_DAT_OUT(RegOffs)), %% or _STAT? unclear..
    write_byte(Bus, ?GPIO_DAT_OUT(RegOffs), Reg bor PinBit).

gpio_clr(Bus, Pin) ->
    {PinBit,RegOffs} = pin(Pin),
    Reg = read_byte(Bus, ?GPIO_DAT_OUT(RegOffs)),  %% or _STAT? unclear..
    write_byte(Bus, ?GPIO_DAT_OUT(RegOffs), Reg band (bnot PinBit)).

gpio_input(Bus, Pin) ->
    gpio_set_direction(Bus, Pin, in).

gpio_output(Bus, Pin) ->
    gpio_set_direction(Bus, Pin, out).

gpio_set_direction(Bus, Pin, Dir) ->
    {PinBit,RegOffs} = pin(Pin),
    Reg = read_byte(Bus, ?GPIO_DIR(RegOffs)),
    case Dir of
	in ->
	    write_byte(Bus, ?GPIO_DIR(RegOffs), Reg band (bnot PinBit));
	out ->
	    write_byte(Bus, ?GPIO_DIR(RegOffs), Reg bor PinBit);
	high ->
	    Reg1 = read_byte(Bus, ?GPIO_DAT_OUT(RegOffs)),
	    write_byte(Bus, ?GPIO_DAT_OUT(RegOffs), Reg1 bor PinBit),
	    write_byte(Bus, ?GPIO_DIR(RegOffs), Reg bor PinBit);
	low ->
	    Reg1 = read_byte(Bus, ?GPIO_DAT_OUT(RegOffs)),
	    write_byte(Bus, ?GPIO_DAT_OUT(RegOffs), Reg1 band (bnot PinBit)),
	    write_byte(Bus, ?GPIO_DIR(RegOffs), Reg bor PinBit)
    end.

gpio_get_direction(Bus, Pin) ->
    {PinBit,RegOffs} = pin(Pin),
    Reg = read_byte(Bus, ?GPIO_DIR(RegOffs)),
    case Reg band PinBit of
	0 -> {ok,in};
	_ -> {ok,out}
    end.

-spec gpio_set_interrupt(Bus::i2c:i2c_bus(), Pin::pin(), Style::style()) ->
	  ok | {error,posix()}.

gpio_set_interrupt(Bus, Pin, Style) ->
    {PinBit,RegOffs} = pin(Pin),
    En = read_byte(Bus, ?GPIO_INT_EN(RegOffs)),
    case Style of
	none -> %% disable
	    write_byte(Bus, ?GPIO_INT_EN(RegOffs), En band (bnot PinBit));
	falling ->
	    Lvl = read_byte(Bus, ?GPIO_INT_LVL(RegOffs)),
	    write_byte(Bus, ?GPIO_INT_EN(RegOffs), En bor PinBit),
	    write_byte(Bus, ?GPIO_INT_LVL(RegOffs), Lvl band (bnot PinBit));
	rising ->
	    Lvl = read_byte(Bus, ?GPIO_INT_LVL(RegOffs)),
	    write_byte(Bus, ?GPIO_INT_EN(RegOffs), En bor PinBit),
	    write_byte(Bus, ?GPIO_INT_LVL(RegOffs), Lvl bor PinBit)
    end.


gpio_get_interrupt(Bus, Pin) ->
    {PinBit,RegOffs} = pin(Pin),
    case read_byte(Bus, ?GPIO_INT_EN(RegOffs)) band PinBit of
	0 -> none;
	_ ->
	    case read_byte(Bus, ?GPIO_INT_LVL(RegOffs)) band PinBit of
		0 -> falling;
		_ -> rising
	    end
    end.

gpio_read_port(Bus, Pin) ->
    {_PinBit,RegOffs} = pin(Pin),
    read_byte(Bus, ?GPIO_DAT_STAT(RegOffs)).

gpio_write_port(Bus, Pin, Data) ->
    {_PinBit,RegOffs} = pin(Pin),
    write_byte(Bus, ?GPIO_DAT_OUT(RegOffs), Data).

%% pin to bit and port number
-spec pin(pin()) -> {Bit::16#00..16#ff,0..3}.

pin({row,I}) when is_integer(I),I >= 0, I =< 7 -> {?ROW(I),0};
pin({col,I}) when is_integer(I),I >= 0, I =< 7 -> {?COL(I),1};
pin({col,I}) when is_integer(I),I >= 8, I =< 9 -> {?COL(I),2};
pin(I) when is_integer(I), I >= 0, I =< 7  -> {?ROW(I),0};
pin(I) when is_integer(I), I >= 8, I =< 15 -> {?COL(I-8),1};
pin(I) when is_integer(I), I >= 16, I =< 17 -> {?COL(I-16),2}.
    

read_byte(Bus, Command) ->
    {ok,Byte} = i2c:smbus_read_byte_data(Bus, Command),
    Byte.

write_byte(Bus, Command, Byte) when Byte band (bnot 16#ff) =:= 0 ->
    {ok,_} = i2c:smbus_write_byte_data(Bus, Command, Byte),
    ok.
