#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

CFLAGS+=-finline-functions -std=c99 -Wall -Werror -O -DDLOG_DEFAULT=DLOG_NONE # -DDEBUG
LDFLAGS+=-shared

OBJS = i2c_drv.o

all: ../priv/i2c_drv.so

../priv/i2c_drv.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $@

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

clean:
	rm -rf *.o ../priv/i2c_drv.so
