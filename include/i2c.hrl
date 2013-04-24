%%% @author tony <tony@rogvall.se>
%%% @copyright (C) 2013, tony
%%% @doc
%%%      SPI records
%%% @end
%%% Created :  5 Apr 2013 by tony <tony@rogvall.se>
%%%

-ifndef(__I2C_HRL__).
-define(__I2C_HRL__, true).

-type uint32() :: 0..4294967295.
-type uint16() :: 0..65535.
-type uint8() :: 0..255.
-type posix() :: atom().

-define(is_uint8(T), (((T) band (bnot 16#ff)) =:=  0)).
-define(is_uint16(T), (((T) band (bnot 16#ffff)) =:=  0)).
-define(is_uint32(T), (((T) band (bnot 16#ffffffff)) =:=  0)).

-type i2c_flag() ::
	ten | rd | nostart | rev_dir_addr |
	ignore_nak | no_rd_ack | recv_len.

-record(i2c_msg,
	{
	  addr   :: uint16(),
	  flags  :: [i2c_flag()],
	  len    :: uint16(),
	  data   :: binary()
	}).

-endif.
