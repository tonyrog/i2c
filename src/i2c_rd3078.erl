%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    RTC sd3078 for piSugar2
%%% @end
%%% Created : 15 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(i2c_rd3078).
-compile(export_all).

-define(I2C_ADDR_RTC, 16#32).

open(Bus) ->
    i2c:open(Bus),
    i2c:set_slave(Bus, ?I2C_ADDR_RTC).

init(Bus) ->
    clear_alarm_flag(Bus).


enable_write(Bus) ->
    %% ctr2 - wrtc1
    {ok,CTR2} = i2c:smbus_read_byte_data(Bus, 16#10),
    i2c:smbus_write_byte_data(Bus, 16#10, CTR2 bor 2#1000_0000),
    %% ctr1 - wrtc2 and wrtc3
    {ok,CTR1} = i2c:smbus_read_byte_data(Bus, 16#0f),
    i2c:smbus_write_byte_data(Bus, 16#0f, CTR1  bor 2#1000_0100),
    ok.

disable_write(Bus) ->
    %% ctr1 - wrtc2 and wrtc3
    {ok,CTR1} = i2c:smbus_read_byte_data(Bus, 16#0f),
    i2c:smbus_write_byte_data(Bus, 16#0f, CTR1 band 2#0111_1011),
    %% ctr2 - wrtc1
    {ok,CTR2} = i2c:smbus_read_byte_data(Bus, 16#10),
    i2c:smbus_write_byte_data(Bus, 16#10, CTR2 band 2#0111_1111),
    ok.

read_battery_low_flag(Bus) ->
    {ok,V} = i2c:smbus_read_byte_data(Bus, 16#1a),
    {ok, (V band 2#0000_0001) =/= 0}.

read_battery_high_flag(Bus) ->
    {ok,V} = i2c:smbus_read_byte_data(Bus,16#1a),
    {ok, (V band 2#0000_0010) =/= 0}.

read_battery_charging_flag(Bus) ->
    {ok,V} = i2c:smbus_read_byte_data(Bus,16#18),
    {ok, (V band 2#1000_0000) =/= 0}.

toggle_charging(Bus, Enable) ->
    enable_write(Bus),
    V = if Enable -> 16#82; true -> 16#82 band 2#0111_1111 end,
    i2c:smbus_write_byte_data(Bus, 16#18, V),
    disable_write(Bus).

%% read time
read_time(Bus) ->
    case i2c:smbus_read_i2c_block_data(Bus, 16#00, 32) of
	{ok,BcdTime = <<_,_,H,_/binary>>} ->
	    if H band 16#80 =:= 16#80 ->
		    BcdTime1 = set_pos(BcdTime, 2, (H band 16#7F)),
		    to_date_time(BcdTime1);
	       H band 16#20 =:= 16#20 ->
		    H1 = bcd_to_dec(H band 16#1F),
		    H2 = dec_to_bcd(H1+12),
		    BcdTime1 = set_pos(BcdTime, 2, H2),
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
    H24 = H bor 16#80, %% 24h mode
    enable_write(Bus),
    i2c:smbus_write_i2c_block_data(Bus, 16#00, <<S,M,H24,Rest/binary>>),
    disable_write(Bus).

%% Read alarm time
read_alarm_time(Bus) ->
    case i2c:smbus_read_i2c_block_data(Bus, 16#07, 32) of
	{ok,<<S,M,H,Rest/binary>>} ->
	    %% always 24hr
	    {ok,to_date_time(<<S,M,(H band 16#3F),Rest/binary>>)};
	Error -> Error
    end.
	
%% Check alarm enabled
read_alarm_enabled(Bus) ->
    case i2c:smbus_read_byte_data(Bus, 16#0e) of
	{ok,V} when V band 2#0000_0111 =:= 0 ->
	    {ok,false};
	{ok,_} ->
	    case i2c:smbus_read_byte_data(Bus, 16#10) of
		{ok,CTR2} when CTR2 band 2#0000_0010 =:= 0 ->
		    {ok,false};
	       true ->
		    {ok,true}
	    end;
	Error ->
	    Error
    end.

%% Read alarm flag
read_alarm_flag(Bus) ->
    %% CTR1 - INTDF and INTAF
    case i2c:smbus_read_byte_data(Bus, 16#0f) of
	{ok,Data} when Data band 2#0010_0000 =/= 0;
		       Data band 2#0001_0000 =/= 0 ->
            {ok,true};
	{ok,_} ->
	    {ok,false};
	Error ->
	    Error
    end.
	
%% Clear alarm flag
clear_alarm_flag(Bus) ->
    case read_alarm_flag(Bus) of
	{ok,true} ->
            enable_write(Bus),
            {ok,CTR1} = i2c:smbus_read_byte_data(Bus, 16#0f),
            i2c:smbus_write_byte_data(Bus, 16#0f, CTR1 band 2#1100_1111),
            disable_write(Bus),
	    ok;
	{ok,false} ->
	    ok;
	Error ->
	    Error
    end.

%% Disable alarm
disable_alarm(Bus) ->
    enable_write(Bus),
    %% CTR2 - INTS1, clear
    {ok,CTR2} = i2c:smbus_read_byte_data(Bus, 16#10),
    i2c:smbus_write_byte_data(Bus, 16#10, 
			      (CTR2 bor 2#0101_0010) band 2#1101_1111),
    %% disable alarm
    i2c:smbus_write_byte_data(Bus, 16#0e, 16#0000_0000),

    disable_write(Bus),
    ok.

%% Set alarm, weekday_repeat from sunday 0-6
set_alarm(Bus, DateTime, WeekdayRepeat) ->
    BcdTime = from_date_time(DateTime),
    BcdTime1 = set_pos(BcdTime, 3, WeekdayRepeat),

    enable_write(Bus),

    %% alarm time
    i2c:smbus_write_i2c_block_data(Bus, 16#07, BcdTime1),

    %% CTR2 - alarm interrupt and frequency
    {ok,CTR2} = i2c:smbus_read_byte_data(Bus, 16#10),
    
    i2c:smbus_write_byte_data(Bus, 16#10,
			      (CTR2 bor 2#0101_0010) band 2#1101_1111),

    %% alarm allows weekday, hour/minus/second
    i2c:smbus_write_byte_data(Bus, 16#0e, 2#0000_1111),

    disable_write(Bus).


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

set_pos(Binary, BytePos, Byte) ->
    case Binary of
	<<Before:BytePos/binary, _, After/binary>> ->
	    <<Before/binary, Byte, After/binary>>
    end.
