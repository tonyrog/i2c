%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    PiSugar2 battery controler (ip5209)
%%% @end
%%% Created : 15 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(i2c_ip5209).

-define(I2C_ADDR_BAT, 16#75).

-define(BAT_FULL_CHARGE_DURATION, (5 * 60)).

-define(BATTERY_CURVE,
	[
	 {4.16, 100.0},
	 {4.05, 95.0},
	 {4.00, 80.0},
	 {3.92, 65.0},
	 {3.86, 40.0},
	 {3.79, 25.5},
	 {3.66, 10.0},
	 {3.52, 6.5},
	 {3.49, 3.2},
	 {3.1, 0.0}
	]).

-compile(export_all).

open(Bus) ->
    i2c:open(Bus),
    i2c:set_slave(Bus, ?I2C_ADDR_BAT),
    ok.

read_voltage(Bus) ->
    L = read_byte(Bus, 16#a2),
    H = read_byte(Bus, 16#a3),
    <<_:2, V:14/signed>> = <<H,L>>,  %% read a2,a3 as 2block?
    (2600.0 + abs(V)*0.26855) / 1000.0.

read_intensity(Bus) ->
    L = read_byte(Bus, 16#a4),
    H = read_byte(Bus, 16#a5),
    <<_:2, I:14/signed>> = <<H,L>>,  %% read a4,a5 as 2block?
    (I*0.745985) / 1000.0.

is_power_plugged_2led(Bus) ->
    V = read_byte(Bus, 16#55),
    V band 2#0001_0000 =/= 0.

read_gpio_tap(Bus) ->
    read_byte(Bus, 16#55).

force_shutdown(Bus) ->
    T = read_byte(Bus, 16#01),
    T1 = T band 2#1111_1011,
    write_byte(Bus, 16#01, T1).

parse_voltage_level(Voltage) ->
    if Voltage > 0.0 ->
	    convert_battery_voltage_to_level(Voltage, ?BATTERY_CURVE);
       true ->
            100.0
    end.

convert_battery_voltage_to_level(Voltage,[{V,L}|_Curve]) when Voltage >= V ->
    L;
convert_battery_voltage_to_level(Voltage,[{V,L}|Curve]) ->
    convert_battery_voltage_to_level(Voltage,V,L,Curve).

convert_battery_voltage_to_level(Voltage,V0,L0,[{V1,L1}|_Curve]) 
  when Voltage >= V1 ->
    P = (Voltage - V1) / (V0 - V1),
    L1 + P*(L0 - L1);
convert_battery_voltage_to_level(Voltage,_V0,_L0,[{V1,L1}|Curve]) ->
    convert_battery_voltage_to_level(Voltage,V1,L1,Curve);
convert_battery_voltage_to_level(_Voltage,_V0,_L0,[]) ->
    0.0.

%% short cuts
    
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
