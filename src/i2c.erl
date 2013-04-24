%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2013, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author tony <tony@rogvall.se>
%%% @doc
%%%    Linx I2C api
%%% @end
%%% Created :  23 Apr 2013 by tony <tony@rogvall.se>

-module(i2c).
-behaviour(gen_server).

-export([open/1]).
-export([close/1]).
-export([set_retries/2]).
-export([set_timeout/2]).
-export([set_slave/2]).
-export([set_slave_force/2]).
-export([set_tenbit/2]).
-export([set_pec/2]).
-export([get_funcs/1]).
-export([rdwr/2]).
-export([smbus/2]).

-export([start/0,start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/i2c.hrl").

-define(I2C_PORT, i2c_port).
-define(I2C_SRV,  i2c_srv).

-define(CMD_OPEN,        1).
-define(CMD_CLOSE,       2).
-define(CMD_SET_RETRIES, 3).
-define(CMD_SET_TIMEOUT, 4).
-define(CMD_SET_SLAVE,   5).
-define(CMD_SET_SLAVEF,  6).
-define(CMD_SET_TENBIT,  7).
-define(CMD_SET_PEC,     8).
-define(CMD_GET_FUNCS,   9).
-define(CMD_RDWR,        10).
-define(CMD_SMBUS,       11).

-define(I2C_FUNC_I2C,			16#00000001).
-define(I2C_FUNC_10BIT_ADDR,		16#00000002).
-define(I2C_FUNC_PROTOCOL_MANGLING,	16#00000004). %% I2C_M_IGNORE_NAK etc.
-define(I2C_FUNC_SMBUS_PEC,		16#00000008).
-define(I2C_FUNC_NOSTART,		16#00000010). %% I2C_M_NOSTART
-define(I2C_FUNC_SMBUS_BLOCK_PROC_CALL,	16#00008000). %% SMBus 2.0
-define(I2C_FUNC_SMBUS_QUICK,		16#00010000).
-define(I2C_FUNC_SMBUS_READ_BYTE,	16#00020000).
-define(I2C_FUNC_SMBUS_WRITE_BYTE,	16#00040000).
-define(I2C_FUNC_SMBUS_READ_BYTE_DATA,	16#00080000).
-define(I2C_FUNC_SMBUS_WRITE_BYTE_DATA,	16#00100000).
-define(I2C_FUNC_SMBUS_READ_WORD_DATA,	16#00200000).
-define(I2C_FUNC_SMBUS_WRITE_WORD_DATA,	16#00400000).
-define(I2C_FUNC_SMBUS_PROC_CALL,	16#00800000).
-define(I2C_FUNC_SMBUS_READ_BLOCK_DATA,	16#01000000).
-define(I2C_FUNC_SMBUS_WRITE_BLOCK_DATA, 16#02000000).
-define(I2C_FUNC_SMBUS_READ_I2C_BLOCK,	16#04000000). %% I2C-like block xfer 
-define(I2C_FUNC_SMBUS_WRITE_I2C_BLOCK,	16#08000000). %% w/ 1-byte reg. addr.

-define(I2C_SMBUS_READ,	 1).
-define(I2C_SMBUS_WRITE, 0).

-define(I2C_SMBUS_QUICK,	    0).
-define(I2C_SMBUS_BYTE,		    1).
-define(I2C_SMBUS_BYTE_DATA,	    2).
-define(I2C_SMBUS_WORD_DATA,	    3).
-define(I2C_SMBUS_PROC_CALL,	    4).
-define(I2C_SMBUS_BLOCK_DATA,	    5).
-define(I2C_SMBUS_I2C_BLOCK_BROKEN, 6).
-define(I2C_SMBUS_BLOCK_PROC_CALL,  7).	 %% SMBus 2.0
-define(I2C_SMBUS_I2C_BLOCK_DATA,   8).


open(Bus) when ?is_uint16(Bus) ->
    call(?I2C_PORT, ?CMD_OPEN, <<Bus:16>>).

close(Bus) when ?is_uint16(Bus) ->
    call(?I2C_PORT, ?CMD_CLOSE, <<Bus:16>>).

set_retries(Bus, Retries) when ?is_uint16(Bus), 
			       ?is_uint32(Retries) ->
    call(?I2C_PORT, ?CMD_SET_RETRIES, <<Bus:16, Retries:32>>).

%% @doc
%% Set i2c operation timeout in milliseconds.
%% @end
-spec set_timeout(Bus::uint16(), Timeout::uint32()) ->
			 ok | {error, Reason::posix()}.

set_timeout(Bus, Timeout) when ?is_uint16(Bus), 
			       ?is_uint32(Timeout) ->
    call(?I2C_PORT, ?CMD_SET_TIMEOUT, <<Bus:16, Timeout:32>>).

%% @doc
%% Set i2c slave.
%% @end
-spec set_slave(Bus::uint16(), Slave::uint16()) ->
		       ok | {error, Reason::posix()}.
set_slave(Bus, Slave) when ?is_uint16(Bus), 
			   ?is_uint16(Slave) ->
    call(?I2C_PORT, ?CMD_SET_SLAVE, <<Bus:16, Slave:16>>).

%% @doc
%% Set i2c slave, or rebind to new slave.
%% @end
-spec set_slave_force(Bus::uint16(), Slave::uint16()) ->
			     ok | {error, Reason::posix()}.
set_slave_force(Bus, Slave) when ?is_uint16(Bus), 
				 ?is_uint16(Slave) ->
    call(?I2C_PORT, ?CMD_SET_SLAVEF, <<Bus:16, Slave:16>>).

%% @doc
%% Set i2c slave address style to 10-bit (normal 7-bit)
%% @end
-spec set_tenbit(Bus::uint16(), Enable::boolean()) ->
			ok | {error, Reason::posix()}.
set_tenbit(Bus, Enable) when ?is_uint16(Bus), 
			     is_boolean(Enable) ->
    E = if Enable -> 1; true -> 0 end,
    call(?I2C_PORT, ?CMD_SET_TENBIT, <<Bus:16, E:8>>).

%% @doc
%% Enable error correction code with SMBus transfers
%% @end
-spec set_pec(Bus::uint16(), Enable::boolean()) ->
		     ok | {error, Reason::posix()}.
set_pec(Bus, Enable) when ?is_uint16(Bus), 
			  is_boolean(Enable) ->
    E = if Enable -> 1; true -> 0 end,
    call(?I2C_PORT, ?CMD_SET_PEC, <<Bus:16, E:8>>).

get_funcs(Bus) when ?is_uint16(Bus) ->
    case call(?I2C_PORT, ?CMD_GET_FUNCS, <<Bus:16>>) of
	{ok,Value} ->
	    {ok, value_to_funcs(Value)};
	Error ->
	    Error
    end.

-spec rdwr(Bus::uint16(), RdWr::[#i2c_msg{}]) ->
		  {ok, [binary()]} | {error, posix()}.

rdwr(Bus, RdWr) when ?is_uint16(Bus),
		     is_list(RdWr) ->
    {N,Data} = encode_rdwr(RdWr, 0, []),
    case call(?I2C_PORT, ?CMD_RDWR, <<Bus:16, N:32, Data/binary>>) of
	{ok,Bin} ->
	    {ok, decode_rdwr(RdWr, Bin)};
	Error ->
	    Error
    end.

smbus(Bus, {read,Command,Size}) when ?is_uint16(Bus),
				    ?is_uint8(Command),
				    is_atom(Size) ->
    Size32 = smbus_size(Size),
    call(?I2C_PORT, ?CMD_SMBUS,
	 <<Bus:16, ?I2C_SMBUS_READ, Command:8, Size32:32>>);
smbus(Bus, {write,Command,Size,Data}) when ?is_uint16(Bus),
					  ?is_uint8(Command),
					  is_atom(Size),
					  is_binary(Data) ->
    Size32 = smbus_size(Size),
    Len = byte_size(Data),
    call(?I2C_PORT, ?CMD_SMBUS,
	 << Bus:16, ?I2C_SMBUS_WRITE, Command:8, Size32:32,
	    Len:32, Data/binary >>).

smbus_size(quick) -> ?I2C_SMBUS_QUICK;
smbus_size(byte) -> ?I2C_SMBUS_BYTE;
smbus_size(byte_data) -> ?I2C_SMBUS_BYTE_DATA;
smbus_size(word_data) -> ?I2C_SMBUS_WORD_DATA;
smbus_size(proc_call) -> ?I2C_SMBUS_PROC_CALL;
smbus_size(block_data) -> ?I2C_SMBUS_BLOCK_DATA;
smbus_size(i2c_block_broken) -> ?I2C_SMBUS_I2C_BLOCK_BROKEN;
smbus_size(block_proc_call) -> ?I2C_SMBUS_BLOCK_PROC_CALL;
smbus_size(i2c_block_data) -> ?I2C_SMBUS_I2C_BLOCK_DATA.
    

decode_rdwr([#i2c_msg{flags=Fs,len=Len} | RdWr], Bin) ->
    case lists:member(rd,Fs) of
	true ->
	    <<Read:Len/binary,Bin1/binary>> = Bin,
	    [Read | decode_rdwr(RdWr, Bin1)];
	false ->
	    [<<>> | decode_rdwr(RdWr, Bin)]
    end;
decode_rdwr([], _Bin) ->
    [].


encode_rdwr([#i2c_msg{addr=Addr,flags=Fs,len=Len,data=Data} | RdWr],
	    I, Acc) ->
    Flags = encode_flags(Fs,0),
    DataLen = byte_size(Data),
    encode_rdwr(RdWr,
		I+1,
		[<<Addr:16, Flags:16, Len:16, DataLen:16, Data/binary>> |
		 Acc]);
encode_rdwr([], N, Acc) -> 
    {N, list_to_binary(lists:reverse(Acc))}.

encode_flags([F|Fs], Flags) ->
    case F of
	ten          -> encode_flags(Fs, 16#0010 bor Flags);
	rd           -> encode_flags(Fs, 16#0001 bor Flags);
	nostart      -> encode_flags(Fs, 16#4000 bor Flags);
	rev_dir_addr -> encode_flags(Fs, 16#2000 bor Flags);
	ignore_nak   -> encode_flags(Fs, 16#1000 bor Flags);
	no_rd_ack    -> encode_flags(Fs, 16#0800 bor Flags);
	recv_len     -> encode_flags(Fs, 16#0400 bor Flags)
    end;
encode_flags([], Flags) ->
    Flags.
	    

%% call directly to a registered - bypass gen_server 
call(Port, Cmd, Data) ->
    case erlang:port_control(Port, Cmd, Data) of
	<<0>> ->
	    ok;
	<<255,E/binary>> -> 
	    {error, erlang:binary_to_atom(E, latin1)};
	<<1,Y>> -> {ok,Y};
	<<2,Y:16/native-unsigned>> -> {ok, Y};
	<<4,Y:32/native-unsigned>> -> {ok, Y};
	<<8,Y:64/native-unsigned>> -> {ok, Y};
	<<3,Return/binary>> -> {ok,Return}
    end.

value_to_funcs(Value) ->	
    lists:append(
      [get_value(?I2C_FUNC_I2C,i2c,Value),
       get_value(?I2C_FUNC_10BIT_ADDR,tenbit_addr,Value),
       get_value(?I2C_FUNC_PROTOCOL_MANGLING,protocol_mangling,Value),
       get_value(?I2C_FUNC_SMBUS_PEC,smbus_pec,Value),
       get_value(?I2C_FUNC_NOSTART,smbus_nostart,Value),
       get_value(?I2C_FUNC_SMBUS_BLOCK_PROC_CALL,smbus_block_proc_call,Value),
       get_value(?I2C_FUNC_SMBUS_QUICK,smbus_quick,Value),
       get_value(?I2C_FUNC_SMBUS_READ_BYTE,smbus_read_byte,Value),
       get_value(?I2C_FUNC_SMBUS_WRITE_BYTE,smbus_write_byte,Value),
       get_value(?I2C_FUNC_SMBUS_READ_BYTE_DATA,smbus_read_byte_data,Value),
       get_value(?I2C_FUNC_SMBUS_WRITE_BYTE_DATA,smbus_write_byte_data,Value),
       get_value(?I2C_FUNC_SMBUS_READ_WORD_DATA,smbus_read_word_data,Value),
       get_value(?I2C_FUNC_SMBUS_WRITE_WORD_DATA,smbus_write_word_data,Value),
       get_value(?I2C_FUNC_SMBUS_PROC_CALL,proc_call,Value),
       get_value(?I2C_FUNC_SMBUS_READ_BLOCK_DATA,read_block_data,Value),
       get_value(?I2C_FUNC_SMBUS_WRITE_BLOCK_DATA,write_block_data,Value),
       get_value(?I2C_FUNC_SMBUS_READ_I2C_BLOCK,read_i2c_block,Value),
       get_value(?I2C_FUNC_SMBUS_WRITE_I2C_BLOCK,write_i2c_block,Value)]).

get_value(Flag,Name,Value) ->
    if Flag band Value =/= 0  -> [Name];
       true -> []
    end.

%%
%% i2c gen_server just keep the port going
%%	     

start_link() ->
    gen_server:start_link({local, ?I2C_SRV}, ?MODULE, [], []).

start() ->
    application:start(i2c).

-record(state, { port} ).

init([]) ->
    Driver = "i2c_drv", 
    ok = erl_ddll:load_driver(code:priv_dir(i2c), Driver),
    Port = erlang:open_port({spawn_driver, Driver},[binary]),
    true = erlang:register(?I2C_PORT, Port),
    {ok, #state{ port=Port }}.

handle_call(_Request, _From, State) ->
    {reply, {error,bad_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
