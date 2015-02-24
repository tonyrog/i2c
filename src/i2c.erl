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
-export([smbus/5,smbus_read/3,smbus_write/4]).
-export([smbus_read_byte/1,
	 smbus_read_byte_data/2,
	 smbus_read_word_data/2,
	 smbus_read_block_data/2,
	 smbus_write_quick/2,
	 smbus_write_byte/2,
	 smbus_write_byte_data/3,
	 smbus_write_word_data/3,
	 smbus_write_block_data/3,
	 smbus_process_call/3,
	 smbus_read_i2c_block_data/3,
	 smbus_write_i2c_block_data/3,
	 smbus_block_process_call/3]).
-export([debug/1]).

%% gen_server api
-export([start/0,
	 start_link/0]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

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
-define(CMD_DEBUG,       12).

-define(DLOG_DEBUG,     7).
-define(DLOG_INFO,      6).
-define(DLOG_NOTICE,    5).
-define(DLOG_WARNING,   4).
-define(DLOG_ERROR,     3).
-define(DLOG_CRITICAL,  2).
-define(DLOG_ALERT,     1).
-define(DLOG_EMERGENCY, 0).
-define(DLOG_NONE,     -1).


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


%% @doc
%% Open i2c bus.
%% @end
-spec open(Bus::uint16()) ->
			 ok | {error, Reason::posix()}.

open(Bus) when ?is_uint16(Bus) ->
    call(?I2C_PORT, ?CMD_OPEN, <<Bus:16>>).

%% @doc
%% Close i2c bus.
%% @end
-spec close(Bus::uint16()) ->
			 ok | {error, Reason::posix()}.
close(Bus) when ?is_uint16(Bus) ->
    call(?I2C_PORT, ?CMD_CLOSE, <<Bus:16>>).

%% @doc
%% Set i2c operation number of retries.
%% @end
-spec set_retries(Bus::uint16(), Retries::uint32()) ->
			 ok | {error, Reason::posix()}.

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

%% @doc
%% Get value for i2c functions.
%% @end
-spec get_funcs(Bus::uint16()) ->
		       ok | {error, Reason::posix()}.

get_funcs(Bus) when ?is_uint16(Bus) ->
    case call(?I2C_PORT, ?CMD_GET_FUNCS, <<Bus:16>>) of
	{ok,Value} ->
	    {ok, value_to_funcs(Value)};
	Error ->
	    Error
    end.

%% @doc
%% Read/Write command.
%% @end
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



%% @doc
%%    SMbus read/write command
%% @end

smbus(Bus, ReadWrite, Command, Size, Data) ->
    call(?I2C_PORT, ?CMD_SMBUS,
	 <<Bus:16, ReadWrite, Command:8, Size:32, Data/binary>>).

smbus_read(Bus, Command, Size) ->
    smbus(Bus, ?I2C_SMBUS_READ, Command, Size, <<>>).

smbus_write(Bus, Command, Size, Data) ->
    smbus(Bus, ?I2C_SMBUS_WRITE, Command, Size, Data).

smbus_read_byte(Bus) ->
    case smbus_read(Bus,0,?I2C_SMBUS_BYTE) of
	{ok,<<Value:8,_/binary>>} ->
	    {ok,Value};
	Error -> Error
    end.

smbus_read_byte_data(Bus, Command) ->
    case smbus_read(Bus, Command,?I2C_SMBUS_BYTE_DATA) of
	{ok,<<Value:8,_/binary>>} ->
	    {ok,Value};
	Error -> Error
    end.

smbus_read_word_data(Bus, Command) ->
    case smbus_read(Bus,Command,?I2C_SMBUS_WORD_DATA) of
	{ok,<<Value:16/native,_/binary>>} ->
	    {ok,Value};
	Error ->
	    Error
    end.

smbus_read_block_data(Bus, Command) ->
    case smbus_read(Bus,Command,?I2C_SMBUS_BLOCK_DATA) of
	{ok,<<N,Return:N/binary,_/binary>>} ->
	    {ok,Return};
	Error -> Error
    end.

smbus_write_quick(Bus, Value)
  when is_integer(Value) ->
    smbus(Bus,Value,0,?I2C_SMBUS_QUICK,
	  <<>>).

smbus_write_byte(Bus, Value)
  when is_integer(Value) ->
    smbus_write(Bus,Value,?I2C_SMBUS_BYTE,
		<<>>).

smbus_write_byte_data(Bus, Command, Value)
  when is_integer(Command), is_integer(Value) ->
    smbus_write(Bus,Command,?I2C_SMBUS_BYTE_DATA,
		<<Value>>).

smbus_write_word_data(Bus, Command, Value)
  when is_integer(Command),is_integer(Value) ->
    smbus_write(Bus, Command,?I2C_SMBUS_WORD_DATA,
	  <<Value:16/native>>).

smbus_write_block_data(Bus, Command, Data)
  when is_integer(Command), is_binary(Data) ->
    N = max(byte_size(Data),32),
    smbus_write(Bus,Command,?I2C_SMBUS_BLOCK_DATA,<<N,Data:N/binary>>).

smbus_process_call(Bus, Command, Value) ->
    case smbus(Bus,?I2C_SMBUS_WRITE,Command,
	       ?I2C_SMBUS_PROC_CALL,<<Value:16/native>>) of
	{ok,<<Return:16/native>>} ->
	    {ok,Return};
	Error -> Error
    end.

smbus_read_i2c_block_data(Bus, Command, Length)
    when is_integer(Command), is_integer(Length), Length >= 0 ->
    N = max(Length, 32),
    Data = <<N>>,
    Size = if N =:= 32 ->
		   ?I2C_SMBUS_I2C_BLOCK_BROKEN;
	      true ->
		   ?I2C_SMBUS_I2C_BLOCK_DATA
	   end,
    case smbus(Bus,?I2C_SMBUS_READ,Command,Size,Data) of
	{ok,<<N,Return:N/binary,_/binary>>} ->
	    {ok,Return};
	Error -> Error
    end.

smbus_write_i2c_block_data(Bus, Command, Data)
  when is_integer(Command), is_binary(Data) ->
    N = max(byte_size(Data),32),
    smbus(Bus,?I2C_SMBUS_WRITE,Command,
	  ?I2C_SMBUS_I2C_BLOCK_BROKEN,<<N,Data:N/binary>>).

smbus_block_process_call(Bus, Command, Values) ->
    N = max(byte_size(Values),32),
    Data = <<N,Values:N/binary>>,
    case smbus(Bus,?I2C_SMBUS_WRITE,Command,
	       ?I2C_SMBUS_BLOCK_PROC_CALL,Data) of
	{ok,<<M,Return:M/binary,_/binary>>} ->
	    {ok,Return};
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

debug(Level) when is_atom(Level) ->
    L = level(Level),
    call(?I2C_PORT, ?CMD_DEBUG, <<L:32>>).

%% convert symbolic to numeric level
level(true)  -> ?DLOG_DEBUG;
level(false) -> ?DLOG_NONE;
level(debug) -> ?DLOG_DEBUG;
level(info)  -> ?DLOG_INFO;
level(notice) -> ?DLOG_NOTICE;
level(warning) -> ?DLOG_WARNING;
level(error) -> ?DLOG_ERROR;
level(critical) -> ?DLOG_CRITICAL;
level(alert) -> ?DLOG_ALERT;
level(emergency) -> ?DLOG_EMERGENCY;
level(none) -> ?DLOG_NONE.


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

%%--------------------------------------------------------------------
%% @doc
%% i2c server just used to keep the port going.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> ok.

start_link() ->
    gen_server:start_link({local, ?I2C_SRV}, ?MODULE, [], []).

%% @private
start() ->
    application:start(i2c).

-record(state, { port} ).

%% @private
init([]) ->
    Driver = "i2c_drv",
    ok = load_driver(code:priv_dir(i2c), Driver),
    Port = erlang:open_port({spawn_driver, Driver},[binary]),
    true = erlang:register(?I2C_PORT, Port),
    {ok, #state{ port=Port }}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error,bad_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% can be replaced with dloader later
load_driver(Path, Name) ->
    Ext = filename:extension(Name),
    Base = filename:basename(Name,Ext),
    NameExt = case os:type() of
		  {unix,_} ->  Base++".so";
		  {win32,_} -> Base++".dll"
	      end,
    SysPath = filename:join(Path,erlang:system_info(system_architecture)),
    case filelib:is_regular(filename:join(SysPath,NameExt)) of
	true -> erl_ddll:load(SysPath, Name);
	false ->
	    case filelib:is_regular(filename:join(Path,NameExt)) of
		true -> erl_ddll:load(Path, Name);
		false -> {error, enoent}
	    end
    end.
