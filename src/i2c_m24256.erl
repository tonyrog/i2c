%%% @author tony <tony@rogvall.se>
%%% @copyright (C) 2013, tony
%%% @doc
%%%    Access to eeprom M24256
%%% @end
%%% Created : 24 Apr 2013 by tony <tony@rogvall.se>

-module(i2c_m24256).

-export([read/3, write/3]).

-include("../include/i2c.hrl").

-ifndef(M24256_CHIP_ADDR).
-define(M24256_CHIP_ADDR, 16#00).
-endif.

-define(M24256_TYPE_ID,   16#50).
-define(M24256_BYTE_SIZE, (32*1024)).
-define(M24256_PAGE_SIZE, 64).
-define(M24256_MAX_BIT_RATE, 400000).
-define(M24256_WRITE_DELAY,  10000).   %% 10ms

slave_addr(Addr) ->
    ?M24256_TYPE_ID + (Addr band 2#111).

read(Bus, Addr, Size) when is_integer(Addr), Addr >= 0,
			   is_integer(Size), Size >= 0, Size =< 16#ffff ->
    A = slave_addr(?M24256_CHIP_ADDR),
    i2c:rdwr(Bus, [#i2c_msg {addr=A,flags=[],len=2,data=(<<Addr:16>>)},
		   #i2c_msg {addr=A,flags=[rd],len=Size,data=(<<>>)}]).

write(Bus, Addr, Data) when is_binary(Data), is_integer(Addr), Addr >= 0 ->
    A = slave_addr(?M24256_CHIP_ADDR),
    Size = byte_size(Data),
    i2c:rdwr(Bus, [#i2c_msg{addr=A,flags=[],len=2+Size,
			    data=(<<Addr:16,Data/binary>>)}]).

