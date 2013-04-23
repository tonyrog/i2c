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
-export([transfer/2, transfer/7]).
-export([start/0,start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/i2c.hrl").

-define(I2C_PORT, i2c_port).
-define(I2C_SRV,  i2c_srv).

-define(CMD_OPEN,      1).
-define(CMD_CLOSE,     2).
-define(CMD_SLAVE,     3).
-define(CMD_TRANSFER,  4).

-define(ENCODE(BufLen,Speed,Delay,BitsPerWord,Cs,TxData),
	(byte_size(TxData)):32,BufLen:32,Speed:32,Delay:16,
	BitsPerWord:8,Cs:8,TxData/binary).

open(Bus) when ?is_uint16(Bus) ->
    call(?I2C_PORT, ?CMD_OPEN, <<Bus:16>>).

close(Bus) when ?is_uint16(Bus) ->
    call(?I2C_PORT, ?CMD_CLOSE, <<Bus:16>>).

transfer(Bus,L=[#i2c_transfer{}|_])
  when ?is_uint16(Bus) ->
    {N,Data} = encode_i2c(L, [], 0),
    call(?I2C_PORT, ?CMD_TRANSFER, <<Bus:16, N:32, Data>>).
    
transfer(Bus,TxData,BufLen,Delay,Speed,BitsPerWord,Cs)
  when ?is_uint16(Bus),
       is_binary(TxData), ?is_uint32(BufLen),
       ?is_uint16(Delay), ?is_uint32(Speed),
       ?is_uint8(BitsPerWord), ?is_uint8(Cs) ->
    call(?I2C_PORT, ?CMD_TRANSFER,
	 [<<Bus:16, 1:32,
	    ?ENCODE(BufLen,Speed,Delay,BitsPerWord,Cs,TxData) >>]).


encode_i2c([#i2c_transfer {
	       tx_buf=TxData, 
	       len = BufLen,
	       speed_hz = Speed,
	       delay_usecs = Delay,
	       bits_per_word = BitsPerWord,
	       cs = Cs } | Rest ], Acc, I) when
      is_binary(TxData), ?is_uint32(BufLen),
      ?is_uint16(Delay), ?is_uint32(Speed),
      ?is_uint8(BitsPerWord), ?is_uint8(Cs) ->
    encode_i2c(Rest, 
	       [<<?ENCODE(BufLen,Speed,Delay,BitsPerWord,Cs,TxData)>> |
		Acc], I+1);
encode_i2c([],Acc,0) ->  {0, Acc};
encode_i2c([],Acc,1) ->  {1, Acc};
encode_i2c([],Acc,N) ->
    {N, lists:reverse(Acc)}.

call(Port, Cmd, Data) ->
    case erlang:port_control(Port, Cmd, Data) of
	<<0>> ->
	    ok;
	<<255,E/binary>> -> 
	    {error, erlang:binary_to_atom(E, latin1)};
	<<1,Y>> -> {ok,Y};
	<<2,Y:16/native-unsigned>> -> {ok, Y};
	<<4,Y:32/native-unsigned>> -> {ok, Y};
	<<3,Return/binary>> -> {ok,Return}
    end.
	     

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
