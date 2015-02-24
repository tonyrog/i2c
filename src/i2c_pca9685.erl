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
-define(PCA9685_SWRST_ADDR, 16#03).

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

start() ->
    start(1).

start(Bus) ->
    application:start(i2c),
    i2c:open(Bus).

write_mode1(Bus, Val) when is_integer(Val), Val >= 0, Val =< 255 ->
    write_byte(Bus, ?MODE1, Val).

read_mode1(Bus) ->
    read_byte(Bus, ?MODE1).

write_mode2(Bus, Val) when is_integer(Val), Val >= 0, Val =< 255 ->
    write_byte(Bus, ?MODE2, Val).

read_mode2(Bus) ->
    read_byte(Bus, ?MODE2).

write_prescale(Bus, Val) when is_integer(Val), Val >= 0, Val =< 255 ->
    write_byte(Bus, ?PRE_SCALE, Val).

read_prescale(Bus, Val) when is_integer(Val), Val >= 0, Val =< 255 ->
    read_byte(Bus, ?PRE_SCALE).

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
    read_bytes(Bus, ?ALL_LED_ON_L, 4);
read_pwm(Bus, I) when I >= 0, I =< 15 ->
    read_bytes(Bus, ?LEDX_ON_L(I), 4).

write_byte(Bus, Reg, Val) ->
    A = ?PCA9685_CHIP_ADDR,
    Data = <<Reg,Val>>,
    i2c:rdwr(Bus, [#i2c_msg { addr=A,flags=[],len=byte_size(Data),data=Data }]).

read_byte(Bus, Reg) ->
    A = ?PCA9685_CHIP_ADDR,
    Data = <<Reg>>,
    i2c:rdwr(Bus, [#i2c_msg { addr=A,flags=[],len=byte_size(Data), data=Data },
		   #i2c_msg { addr=A,flags=[rd],len=1, data=(<<>>) }]).

write_bytes(Bus, Reg, Bytes) ->
    i2c:rdwr(Bus, wr_multi_bytes(Reg, Bytes)).

read_bytes(Bus, Reg, N) when is_integer(N) ->
    i2c:rdwr(Bus, rd_multi_bytes(Reg, N)).

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
      | rd_multi_bytes(Reg, I-1)].
