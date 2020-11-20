%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    RTC sd3078 for piSugar2
%%% @end
%%% Created : 15 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(i2c_sd3078).

-export([open/1, open/2, open1/1, open1/2]).
-export([read_battery_low_flag/1]).
-export([read_battery_high_flag/1]).
-export([read_battery_charging_flag/1]).
-export([toggle_charging/2]).
-export([read_time/1]).
-export([write_time/2]).
-expotr([write_time/2]).
-export([read_alarm_time/1]).
-export([read_alarm_enabled/1]).
-export([read_alarm_flag/1]).
-export([clear_alarm_flag/1]).
-export([disable_alarm/1]).
-export([set_alarm/3]).

%% debug/test
-export([set_test_wake/1]).

-define(I2C_ADDR_RTC, 16#32).

%% data element positions in bcd data
-define(SD3078_REG_RTC,    16#00).
-define(SD3078_REG_RTC_SC, 16#00).
-define(SD3078_REG_RTC_MN, 16#01).
-define(SD3078_REG_RTC_HR, 16#02).
-define(SD3078_REG_RTC_DW, 16#03).
-define(SD3078_REG_RTC_DM, 16#04).
-define(SD3078_REG_RTC_MO, 16#05).
-define(SD3078_REG_RTC_YR, 16#06).
%% alarm time
-define(SD3078_REG_ALRM, 16#07).
-define(SD3078_REG_ALRM_SC,  16#07).
-define(SD3078_REG_ALRM_MN,  16#08).
-define(SD3078_REG_ALRM_HR,  16#09).
-define(SD3078_REG_ALRM_DW,  16#0a).
-define(SD3078_REG_ALRM_DM,  16#0b).
-define(SD3078_REG_ALRM_MO,  16#0c).
-define(SD3078_REG_ALRM_YR,  16#0d).
-define(SD3078_REG_ALRM_ENA, 16#0e).
-define(SD3078_REG_CTRL1,    16#0f).
-define(SD3078_REG_CTRL2,    16#10).
-define(SD3078_REG_CTRL3,    16#11).

-define(SD3078_REG_CHARGE,   16#18).
-define(SD3078_REG_BAT,      16#1a).


%% ALARM bits
-define(ALRM_ENA_SC, 2#0000_0001)
-define(ALRM_ENA_MN, 2#0000_0010).
-define(ALRM_ENA_HR, 2#0000_0100).
-define(ALRM_ENA_DW, 2#0000_1000).

-define(KEY_WRITE1, 16#80).
-define(KEY_WRITE2, 16#04).
-define(KEY_WRITE3, 16#80).

%% offsets for RTC and ALRM
-define(OFFS_SC, 16#00).
-define(OFFS_MN, 16#01).
-define(OFFS_HR, 16#02).
-define(OFFS_DW, 16#03).
-define(OFFS_DM, 16#04).
-define(OFFS_MO, 16#05).
-define(OFFS_YR, 16#06).

open(Bus) ->
    open(Bus, ?I2C_ADDR_RTC).
open(Bus, Addr) ->
    i2c:open(Bus),
    i2c:set_slave(Bus, Addr),
    init(Bus),
    ok.

open1(Bus) ->
    open1(Bus, ?I2C_ADDR_RTC).
open1(Bus, Addr) ->
    Port = i2c:open1(Bus),
    i2c:set_slave(Port, Addr),
    init(Bus),
    {ok,Port}.
    

init(Bus) ->
    clear_alarm_flag(Bus).

enable_write(Bus) ->
    set_bits(Bus, ?SD3078_REG_CTRL2, ?KEY_WRITE1),
    set_bits(Bus, ?SD3078_REG_CTRL1, ?KEY_WRITE2),
    set_bits(Bus, ?SD3078_REG_CTRL1, ?KEY_WRITE3),
    ok.

disable_write(Bus) ->
    clr_bits(Bus, ?SD3078_REG_CTRL1, ?KEY_WRITE2),
    clr_bits(Bus, ?SD3078_REG_CTRL1, ?KEY_WRITE3),
    clr_bits(Bus, ?SD3078_REG_CTRL2, ?KEY_WRITE1),
    ok.

read_battery_low_flag(Bus) ->
    V = read_byte(Bus, ?SD3078_REG_BAT),
    (V band 2#0000_0001) =/= 0.

read_battery_high_flag(Bus) ->
    V = read_byte(Bus, ?SD3078_REG_BAT),
    (V band 2#0000_0010) =/= 0.

read_battery_charging_flag(Bus) ->
    V = read_byte(Bus, ?SD3078_REG_CHARGE),
    (V band 2#1000_0000) =/= 0.

%% toggle?
toggle_charging(Bus, Enable) ->
    enable_write(Bus),
    V = if Enable -> 2#1000_0010; true -> 2#0000_0010 end,
    write_byte(Bus, ?SD3078_REG_CHARGE, V),
    disable_write(Bus).

%% read time
read_time(Bus) ->
    case read_block(Bus, ?SD3078_REG_RTC, 7) of
	{ok,BcdTime} ->
	    H = binary_get(BcdTime, ?OFFS_HR),
	    if H band 16#80 =:= 16#80 ->
		    BcdTime1 = binary_upd(BcdTime,?OFFS_HR,0,16#80),
		    to_date_time(BcdTime1);
	       H band 16#20 =:= 16#20 ->
		    H1 = bcd_to_dec(H band 16#1f),
		    BcdTime1 = binary_set(BcdTime,?OFFS_HR,
					  dec_to_bcd(H1+12)),
		    to_date_time(BcdTime1);
	       true ->
		    to_date_time(BcdTime)
	    end;
	Error ->
	    Error
    end.

%% write time
write_time(Bus, DateTime) ->
    <<S,M,H,Rest/binary>> = from_date_time(DateTime),
    enable_write(Bus),
    %% set 24-bit flag and write
    write_block(Bus, ?SD3078_REG_RTC, <<S,M,(H bor 16#80),Rest/binary>>),
    disable_write(Bus).

%% Read alarm time
read_alarm_time(Bus) ->
    case read_block(Bus, ?SD3078_REG_ALRM, 7) of
	{ok,BcdTime} ->
	    %% always 24hr - clear 24 bit flag and 16#40?
	    BcdTime1 = binary_upd(BcdTime,?OFFS_HR,0,16#C0),
	    to_date_time(BcdTime1);
	Error -> Error
    end.
	
%% Check alarm enabled
read_alarm_enabled(Bus) ->
    V = read_byte(Bus, ?SD3078_REG_ALRM_ENA),
    if V band 2#0000_0111 =:= 0 ->
	    false;
       true ->
	    CTR2 = read_byte(Bus, ?SD3078_REG_CTRL2),
	    if CTR2 band 2#0000_0010 =:= 0 ->
		    false;
	       true ->
		    true
	    end
    end.

%% Read alarm flag
read_alarm_flag(Bus) ->
    %% CTR1 - INTDF and INTAF
    Data = read_byte(Bus, ?SD3078_REG_CTRL1),
    if Data band 2#0010_0000 =/= 0;
       Data band 2#0001_0000 =/= 0 ->
            true;
       true ->
	    false
    end.
	
%% Clear alarm flag
clear_alarm_flag(Bus) ->
    case read_alarm_flag(Bus) of
	true ->
            enable_write(Bus),
	    clr_bits(Bus, ?SD3078_REG_CTRL1, 2#0011_0000),
            disable_write(Bus),
	    ok;
	false ->
	    ok;
	Error ->
	    Error
    end.

%% Disable alarm
disable_alarm(Bus) ->
    enable_write(Bus),
    %% CTR2 - INTS1, clear
    upd_bits(Bus, ?SD3078_REG_CTRL2, 2#0101_0010, 2#0010_0000),
    write_byte(Bus, ?SD3078_REG_ALRM_ENA, 2#0000_0000), %% disable alarm
    disable_write(Bus),
    ok.

%% Set alarm, weekday_repeat from sunday 1-7
set_alarm(Bus, DateTime, WeekdayRepeat) when WeekdayRepeat > 0,
					     WeekdayRepeat < 7 ->
    BcdTime = from_date_time(DateTime),
    BcdTime1 = binary_set(BcdTime, ?OFFS_DW, (WeekdayRepeat-1)),
    
    enable_write(Bus),
    write_block(Bus, ?SD3078_REG_ALRM, BcdTime1),  %% set alarm time

    %% CTR2 - alarm interrupt and frequency
    upd_bits(Bus, ?SD3078_REG_CTRL2, 2#0101_0010, 2#0010_0000),

    %% alarm allows weekday, hour/minus/second
    write_byte(Bus, ?SD3078_REG_ALRM_ENA, 2#0000_1111),

    disable_write(Bus).


%% set (byte) register bit
set_bits(Bus, Reg, Bits) ->
    Byte = read_byte(Bus, Reg),
    write_byte(Bus, Reg, Byte bor Bits).

%% clear (byte) register bit
clr_bits(Bus, Reg, Bits) ->
    Byte = read_byte(Bus, Reg),
    write_byte(Bus, Reg, Byte band (bnot Bits)).

upd_bits(Bus, Reg, Set, Clr) ->
    Byte = read_byte(Bus, Reg),
    write_byte(Bus, Reg, (Byte bor Set) band (bnot Clr)).


read_byte(Bus, Command) ->
    {ok,Byte} = i2c:smbus_read_byte_data(Bus, Command),
    Byte.

write_byte(Bus, Command, Byte) when Byte band (bnot 16#ff) =:= 0 ->
    {ok,_} = i2c:smbus_write_byte_data(Bus, Command, Byte),
    ok.

write_block(Bus, Command, Data) when is_binary(Data) ->
    {ok,_} = i2c:smbus_write_i2c_block_data(Bus, Command, Data),
    ok.

read_block(Bus, Command, Len) ->
    i2c:smbus_read_i2c_block_data(Bus, Command, Len).


%% Set a test wake up after 1 minutes
set_test_wake(Bus) ->
    Now = {date(),time()},
    ok = write_time(Bus, Now),
    Then = calendar:gregorian_seconds_to_datetime(
	     calendar:datetime_to_gregorian_seconds(Now)+90),
    set_alarm(Bus, Then, 2#0111_1111),

    io:format("Will wake up after 1min 30sec, please power-off\n"),
    ok.

%% convert data_time => RTC bcd format
from_date_time({{Year,Mon,Day},{Hour,Min,Sec}}) ->
    W = calendar:day_of_the_week(date()) rem 7,
    << (dec_to_bcd(Sec)),(dec_to_bcd(Min)),(dec_to_bcd(Hour)),
       (dec_to_bcd(W)),
       (dec_to_bcd(Day)),(dec_to_bcd(Mon)),(dec_to_bcd(Year rem 100))>>.

%% convert RTC bcd format to date,time 
to_date_time(<<Sec,Min,Hour,_Wday,Day,Mon,Year,_/binary>>) ->
    {{bcd_to_dec(Year)+2000,bcd_to_dec(Mon),bcd_to_dec(Day)},
     {bcd_to_dec(Hour),bcd_to_dec(Min),bcd_to_dec(Sec)}}.

bcd_to_dec(BCD) ->
    (BCD band 16#0F) + (((BCD band 16#F0) bsr 4) * 10).

dec_to_bcd(DEC) ->
    (DEC rem 10) + ((DEC div 10) bsl 4).

binary_upd(Binary, Pos, Set, Clr) ->
    <<Before:Pos/binary, Byte, After/binary>> = Binary,
    <<Before/binary, ((Byte bor Set) band (bnot Clr)), After/binary>>.

binary_set(Binary, BytePos, Byte) ->
    <<Before:BytePos/binary, _, After/binary>> = Binary,
    <<Before/binary, Byte, After/binary>>.

binary_get(Binary, BytePos) ->
    binary:at(Binary, BytePos).
