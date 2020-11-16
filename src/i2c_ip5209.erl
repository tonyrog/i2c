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
    {ok,L} = i2c:smbus_read_byte_data(Bus, 16#a2),
    {ok,H} = i2c:smbus_read_byte_data(Bus, 16#a3),
    %%  check negative values
    Voltage =
	if H band 16#20 =:= 16#20 ->
		V = ((H bor 2#1100_0000) bsl 8) + L,
		2600.0 - V*0.26855;
	   true ->
		V = ((H band 16#1f) bsl 8) + L,
		2600.0 + V*0.26855
	end,
    {ok, Voltage / 1000.0}.

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



    

    
    
