%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Interface to the PCA9685 circuit
%%%    16-channel, 12-bit PWM Fm+ I2C-bus LED controller
%%% @end
%%% Created : 24 Feb 2015 by Tony Rogvall <tony@rogvall.se>

-module(i2c_pca9685).
-compile(export_all).

-include("../include/i2c.hrl").

-ifndef(PCA9685_CHIP_ADDR).
-define(PCA9685_CHIP_ADDR, 16#41). %% default chip address
-endif.

%% software reset address
-define(SWRST, 16#03).

%% Control register
-define(MODE1,            16#00).
-define(MODE2,            16#01).
-define(SUBADR1,          16#02).
-define(SUBADR2,          16#03).
-define(SUBADR3,          16#04).
-define(ALLCALLADR,       16#05).
-define(LEDX_ON_L(N),     ((N)*4 + 6)).
-define(LEDX_ON_H(N),     ((N)*4 + 7)).
-define(LEDX_OFF_L(N),    ((N)*4 + 8)).
-define(LEDX_OFF_H(N),    ((N)*4 + 9)).
-define(ALL_LED_ON_L,  250).
-define(ALL_LED_ON_H,  251).
-define(ALL_LED_OFF_L, 252).
-define(ALL_LED_OFF_H, 253).
-define(PRE_SCALE,        254).
-define(TEST_MODE,        255).

%% MODE1 register bits
-define(RESTART, 16#80).
-define(EXTCLK,  16#40).
-define(AI,      16#20).  %% Auto Increament control reg
-define(SLEEP,   16#10).
-define(SUB1,    16#08).
-define(SUB2,    16#04).
-define(SUB3,    16#02).
-define(ALLCALL, 16#01).

%% MODE2 register bits
-define(INVERT, 16#10). 
-define(OCH,    16#08).  %% output change on ack, (stop otherwise)
-define(OUTDRV, 16#04).  %% open-drain=0, totem pole structure=1
-define(OUTNE_0,  16#00).
-define(OUTNE_1,  16#01).
-define(OUTNE_2,  16#02).
-define(OUTNE_3,  16#03).

-define(OSC_FREQ, 25000000).
-define(MICROS,    1000000).

-define(DECODE(Flags,Flag,Name),
	if (Flags) band (Flag) =:= (Flag) -> [Name];
	   true -> []
	end).
		

start() ->
    start(1).

start(Bus) ->
    application:start(i2c),
    ok = i2c:open(Bus),
    init(Bus).

init(Bus) ->
    write_pwm(Bus, all, 0, 0),
    write_mode2(Bus, ?OUTDRV),
    write_mode1(Bus, ?ALLCALL),
    timer:sleep(5),
    {ok,Mode1} = read_mode1(Bus),
    write_mode1(Bus, Mode1 band (bnot ?SLEEP)),
    timer:sleep(5),
    ok.

reset(Bus) ->
    Data = <<?SWRST>>,
    Cmd =  [#i2c_msg { addr=0,flags=[],len=byte_size(Data),data=Data }],
    case i2c:rdwr(Bus,Cmd) of
	{ok,_} -> ok;
	Error -> Error
    end.

set_mode1(Bus, Bits) ->
    {ok,Mode1} = read_mode1(Bus),
    B = encode_mode1(Bits),
    write_mode1(Bus, Mode1 bor B).

clr_mode1(Bus, Bits) ->
    {ok,Mode1} = read_mode1(Bus),
    B = encode_mode1(Bits),
    write_mode1(Bus, Mode1 band (bnot B)).

get_mode1(Bus) ->
    {ok,Mode1} = read_mode1(Bus),
    decode_mode1(Mode1).

encode_mode1(Bits) ->
    encode_mode1(Bits, 0).

encode_mode1([B|Bs], Bits) ->
    case B of
	restart -> encode_mode1(Bs, Bits bor ?RESTART);
	extclk ->  encode_mode1(Bs, Bits bor ?EXTCLK);
	ai ->  encode_mode1(Bs, Bits bor ?AI);
	sleep ->  encode_mode1(Bs, Bits bor ?SLEEP);
	sub1 ->  encode_mode1(Bs, Bits bor ?SUB1);
	sub2 ->  encode_mode1(Bs, Bits bor ?SUB2);
	sub3 ->  encode_mode1(Bs, Bits bor ?SUB3);
	allcall ->  encode_mode1(Bs, Bits bor ?ALLCALL)
    end;
encode_mode1([], Bits) ->
    Bits.

decode_mode1(Mode) ->
    lists:append([?DECODE(Mode,?RESTART,restart),
		  ?DECODE(Mode,?EXTCLK,extclk),
		  ?DECODE(Mode,?AI,ai),
		  ?DECODE(Mode,?SLEEP,sleep),
		  ?DECODE(Mode,?SUB1,sub1),
		  ?DECODE(Mode,?SUB2,sub2),
		  ?DECODE(Mode,?SUB3,sub3),
		  ?DECODE(Mode,?ALLCALL,allcall)]).
   
write_mode1(Bus, Val) when is_integer(Val), Val >= 0, Val =< 255 ->
    write_byte(Bus, ?MODE1, Val).

read_mode1(Bus) ->
    case read_byte(Bus, ?MODE1) of
	{ok,<<Mode>>} -> {ok,Mode};
	Error -> Error
    end.

set_mode2(Bus, Bits) ->
    {ok,Mode1} = read_mode2(Bus),
    B = encode_mode2(Bits),
    write_mode2(Bus, Mode1 bor B).

clr_mode2(Bus, Bits) ->
    {ok,Mode1} = read_mode2(Bus),
    B = encode_mode2(Bits),
    write_mode2(Bus, Mode1 band (bnot B)).

get_mode2(Bus) ->
    {ok,Mode1} = read_mode2(Bus),
    decode_mode2(Mode1).

encode_mode2(Bits) ->
    encode_mode2(Bits, 0).

encode_mode2([B|Bs], Bits) ->
    case B of
	invert -> encode_mode2(Bs, Bits bor ?INVERT);
	och ->  encode_mode2(Bs, Bits bor ?OCH);
	outdrv ->  encode_mode2(Bs, Bits bor ?OUTDRV);
	{outne,V} -> encode_mode2(Bs, Bits bor (V band 3))
    end;
encode_mode2([], Bits) ->
    Bits.

decode_mode2(Mode) ->
    lists:append([?DECODE(Mode,?INVERT,invert),
		  ?DECODE(Mode,?OCH,och),
		  ?DECODE(Mode,?OUTDRV,outdrv)]) ++
	[{outne,Mode band 3}].

	    
write_mode2(Bus, Val) when is_integer(Val), Val >= 0, Val =< 255 ->
    write_byte(Bus, ?MODE2, Val).

read_mode2(Bus) ->
    case read_byte(Bus, ?MODE2) of
	{ok,<<Mode>>} -> {ok,Mode};
	Error -> Error
    end.

%%
%% Prescale = round(osc_clock/(4096 *update_rate)) - 1
%% osc_freq = 25MHz
%%
%% a frequence of 200 Hz need a prescale of 30
%% Prescal = round(osc_freq/(4096*200)) - 1 = 30
%%
%%  P = osc_freq/(4096*F)-1  => (P+1)*(4096*F) = osc_freq
%%  F = osc_freq/(4096*(P+1))
%%
set_update_time(Bus, Us) when is_number(Us) ->
    set_pwm_frequency(Bus, ?MICROS/Us).

set_pwm_frequency(Bus, F) when is_number(F) ->
    P = round(?OSC_FREQ/(4096*F)) - 1,
    write_prescale(Bus, P).

write_prescale(Bus, Val) when is_integer(Val), Val >= 0, Val =< 255 ->
    {ok,Mode} = read_mode1(Bus),
    if Mode band ?SLEEP =:= ?SLEEP ->
	    write_byte(Bus, ?PRE_SCALE, Val);
       true ->
	    ok = write_mode1(Bus, Mode bor ?SLEEP),
	    write_byte(Bus, ?PRE_SCALE, Val),
	    write_mode1(Bus, Mode)
    end.

read_prescale(Bus) ->
    case read_byte(Bus, ?PRE_SCALE) of
	{ok,<<Pre>>} -> {ok, Pre};
	Error -> Error
    end.

read_pwm_frequency(Bus) ->
    {ok,Prescale} = read_prescale(Bus),
    {ok, round(?OSC_FREQ/(4096*(Prescale+1)))}.

%% set pulse length and delay length  in micro seconds
set_pulse_us(Buse, I, Pulse) ->
    set_pulse_us(Buse, I, Pulse, 0).

set_pulse_us(Bus, I, Pulse, Delay) when is_integer(Pulse), Pulse >= 0,
				     is_integer(Delay), Delay >= 0 ->
    {ok, F} = read_pwm_frequency(Bus),
    PulseLength = (?MICROS / (F*4096)),
    On1    = max(0, round(Delay / PulseLength)),
    Off1   = max(0, round((Delay+Pulse) / PulseLength)-1),
    write_pwm(Bus, I, On1, Off1).

set_duty(Bus, I, Duty) ->
    set_duty(Bus, I, Duty, 0.0).

set_duty(Bus, I, Duty, Delay) when is_number(Duty), 
				   Duty >= 0, Duty =< 1.0,
				   Delay >= 0, Delay =< 1.0 ->
    On1  = max(0, round(4096*Delay)-1),
    Off1 = max(0, round(4096*(Delay+Duty))-1),
    write_pwm(Bus,I,On1,Off1).

write_pwm(Bus, all, On, Off) when
      is_integer(On), On >= 0, On =< 16#fff,
      is_integer(Off), Off >=0, Off =< 16#fff ->
    write_bytes(Bus, ?ALL_LED_ON_L, <<On:16/little,Off:16/little>>);
write_pwm(Bus, I, On, Off) when
      I >= 0, I =< 15,
      is_integer(On), On >= 0, On =< 16#fff,
      is_integer(Off), Off >=0, Off =< 16#fff ->
    write_bytes(Bus, ?LEDX_ON_L(I), <<On:16/little,Off:16/little>>).

read_pwm(Bus, all) ->
    case read_bytes(Bus, ?ALL_LED_ON_L, 4) of
	{ok, <<On:16/little, Off:16/little>>} ->
	    {ok, {On, Off}};
	Error ->
	    Error
    end;
read_pwm(Bus, I) when I >= 0, I =< 15 ->
    case read_bytes(Bus, ?LEDX_ON_L(I), 4) of
	{ok, <<On:16/little, Off:16/little>>} ->
	    {ok, {On, Off}};
	Error ->
	    Error
    end.

write_byte(Bus, Reg, Val) ->
    A = ?PCA9685_CHIP_ADDR,
    Data = <<Reg,Val>>,
    Cmd =  [#i2c_msg { addr=A,flags=[],len=byte_size(Data),data=Data }],
    case i2c:rdwr(Bus,Cmd) of
	{ok,_} -> ok;
	Error -> Error
    end.

read_byte(Bus, Reg) ->
    A = ?PCA9685_CHIP_ADDR,
    Data = <<Reg>>,
    Cmd = [#i2c_msg { addr=A,flags=[],len=byte_size(Data), data=Data },
	   #i2c_msg { addr=A,flags=[rd],len=1, data=(<<>>) }],
    case i2c:rdwr(Bus,Cmd) of
	{ok,Byte} -> {ok,erlang:iolist_to_binary(Byte)};
	Error -> Error
    end.

write_bytes(Bus, Reg, Bytes) ->
    case i2c:rdwr(Bus, wr_multi_bytes(Reg, Bytes)) of
	{ok, _} -> ok;
	Error -> Error
    end.
	    
read_bytes(Bus, Reg, N) when is_integer(N) ->
    case i2c:rdwr(Bus, rd_multi_bytes(Reg, N)) of
	{ok,Bytes} -> {ok,erlang:iolist_to_binary(Bytes)};
	Error -> Error
    end.

wr_multi_bytes(_Reg, <<>>) ->
    [];
wr_multi_bytes(Reg, <<Val,Rest/binary>>) ->
    A = ?PCA9685_CHIP_ADDR,
    [ #i2c_msg { addr=A, flags=[], len=2, data=(<<Reg,Val>>) } |
      wr_multi_bytes(Reg+1, Rest)].

rd_multi_bytes(_Reg, 0) ->
    [];
rd_multi_bytes(Reg, I) ->
    A = ?PCA9685_CHIP_ADDR,
    [ #i2c_msg { addr=A, flags=[], len=1, data=(<<Reg>>) },
      #i2c_msg { addr=A, flags=[rd], len=1, data=(<<>>) }
      | rd_multi_bytes(Reg+1, I-1)].
