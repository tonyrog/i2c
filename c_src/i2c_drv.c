//
// i2c_drv.c   I2C linux / erlang driver
//
//

#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <linux/i2c.h>
#include <linux/types.h>
#include <sys/ioctl.h>

#include "erl_driver.h"
// #include "dthread.h"

#define ATOM(NAME) am_ ## NAME
#define INIT_ATOM(NAME) am_ ## NAME = driver_mk_atom(#NAME)

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif

#define PORT_CONTROL_BINARY

#define INT_EVENT(e) ((int)((long)(e)))

#define CMD_OPEN       1
#define CMD_CLOSE      2
#define CMD_SLAVE      3
#define CMD_TRANSFER   3
#define CMD_RD_MODE    4
#define CMD_RD_BPW     5
#define CMD_RD_SPEED   6

static inline uint32_t get_uint32(uint8_t* ptr)
{
    uint32_t value = (ptr[0]<<24) | (ptr[1]<<16) | (ptr[2]<<8) | (ptr[3]<<0);
    return value;
}

static inline uint16_t get_uint16(uint8_t* ptr)
{
    uint16_t value = (ptr[0]<<8) | (ptr[1]<<0);
    return value;
}

static inline uint8_t get_uint8(uint8_t* ptr)
{
    uint8_t value = (ptr[0]<<0);
    return value;
}

static inline void put_uint16(uint8_t* ptr, uint16_t v)
{
    ptr[0] = v>>8;
    ptr[1] = v;
}

static inline void put_uint32(uint8_t* ptr, uint32_t v)
{
    ptr[0] = v>>24;
    ptr[1] = v>>16;
    ptr[2] = v>>8;
    ptr[3] = v;
}

typedef struct i2c_dev_t
{
    struct i2c_dev_t* next;   // when linked
    int fd;
    uint16_t bus;  // bus number
} i2c_dev_t;

// FIXME?: speed up by makeing first an matrix [BUS]
// when BUS < 8
typedef struct _i2c_ctx_t
{
    ErlDrvPort port;
    i2c_dev_t* first;
} i2c_ctx_t;

static int  i2c_drv_init(void);
static void i2c_drv_finish(void);
static void i2c_drv_stop(ErlDrvData);
static void i2c_drv_output(ErlDrvData, char*, ErlDrvSizeT);
static void i2c_drv_event(ErlDrvData d, ErlDrvEvent e,
			  ErlDrvEventData ed);
static void i2c_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void i2c_drv_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData i2c_drv_start(ErlDrvPort, char* command);
static ErlDrvSSizeT i2c_drv_ctl(ErlDrvData,unsigned int,char*,ErlDrvSizeT,char**, ErlDrvSizeT);
static void i2c_drv_timeout(ErlDrvData);
static void i2c_drv_stop_select(ErlDrvEvent, void*);

ErlDrvTermData am_ok;
ErlDrvTermData am_error;
ErlDrvTermData am_undefined;

static ErlDrvEntry i2c_drv_entry;

#define DLOG_DEBUG     7
#define DLOG_INFO      6
#define DLOG_NOTICE    5
#define DLOG_WARNING   4
#define DLOG_ERROR     3
#define DLOG_CRITICAL  2
#define DLOG_ALERT     1
#define DLOG_EMERGENCY 0
#define DLOG_NONE     -1

#ifndef DLOG_DEFAULT
#define DLOG_DEFAULT DLOG_NONE
#endif

#define DLOG(level,file,line,args...) do { \
	if (((level) == DLOG_EMERGENCY) ||				\
	    ((debug_level >= 0) && ((level) <= debug_level))) { \
	    emit_log((level),(file),(line),args);			\
	}								\
    } while(0)

#define DEBUGF(args...) DLOG(DLOG_DEBUG,__FILE__,__LINE__,args)
#define INFOF(args...)  DLOG(DLOG_INFO,__FILE__,__LINE__,args)
#define NOTICEF(args...)  DLOG(DLOG_NOTICE,__FILE__,__LINE__,args)
#define WARNINGF(args...)  DLOG(DLOG_WARNING,__FILE__,__LINE__,args)
#define ERRORF(args...)  DLOG(DLOG_ERROR,__FILE__,__LINE__,args)
#define CRITICALF(args...)  DLOG(DLOG_CRITICAL,__FILE__,__LINE__,args)
#define ALERTF(args...)  DLOG(DLOG_ALERT,__FILE__,__LINE__,args)
#define EMERGENCYF(args...)  DLOG(DLOG_EMERGENCY,__FILE__,__LINE__,args)

static int debug_level = DLOG_DEFAULT;

static void emit_log(int level, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    if ((level == DLOG_EMERGENCY) ||
	((debug_level >= 0) && (level <= debug_level))) {
	va_start(ap, line);
	fmt = va_arg(ap, char*);
	fprintf(stderr, "%s:%d: ", file, line); 
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\r\n");
	va_end(ap);
    }
}

static i2c_dev_t** find_dev(i2c_ctx_t* ctx, uint16_t bus, i2c_dev_t*** ppp)
{
    i2c_dev_t** pp = &ctx->first;

    while(*pp) {
	i2c_dev_t* p = *pp;
	if ((p->bus == bus)) {
	    if (ppp) *ppp = pp;
	    return pp;
	}
	pp = &p->next;
    }
    return NULL;
}

// add new spidev first in list 
static void add_dev(i2c_ctx_t* ctx, uint16_t bus,
		    i2c_dev_t* ptr)
{
    ptr->next = ctx->first;
    ptr->bus = bus;
    ctx->first = ptr;
}


/* general control reply function */
static ErlDrvSSizeT ctl_reply(int rep, void* buf, ErlDrvSizeT len,
			      char** rbuf, ErlDrvSizeT rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
#ifdef PORT_CONTROL_BINARY
	ErlDrvBinary* bin = driver_alloc_binary(len+1);
	if (bin == NULL) 
	    return -1;
	ptr = bin->orig_bytes;	
	*rbuf = (char*) bin;
#else
	if ((ptr = driver_alloc(len+1)) == NULL)
	    return -1;
	*rbuf = ptr;
#endif
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}


// setup global object area
// load atoms etc.

static int i2c_drv_init(void)
{
    debug_level = DLOG_DEFAULT;
    DEBUGF("i2c_driver_init");
    INIT_ATOM(ok);
    INIT_ATOM(error);
    INIT_ATOM(undefined);
    return 0;
}

// clean up global stuff
static void i2c_drv_finish(void)
{
}

static ErlDrvData i2c_drv_start(ErlDrvPort port, char* command)
{
    (void) command;
    i2c_ctx_t* ctx;

    if ((ctx = (i2c_ctx_t*)
	 driver_alloc(sizeof(i2c_ctx_t))) == NULL) {
	errno = ENOMEM;
	return ERL_DRV_ERROR_ERRNO;
    }
    ctx->port = port;
    ctx->first = NULL;
    DEBUGF("i2c_drv: start (%s)", command);
#ifdef PORT_CONTROL_BINARY
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
#endif
    return (ErlDrvData) ctx;
}

static void i2c_drv_stop(ErlDrvData d)
{
    i2c_ctx_t* ctx = (i2c_ctx_t*) d;
    i2c_dev_t* ptr = ctx->first;
    while(ptr) {
	i2c_dev_t* ptr_n = ptr->next;
	driver_select(ctx->port, (ErlDrvEvent) ptr->fd, ERL_DRV_USE, 0);
	driver_free(ptr);
	ptr = ptr_n;
    }
    driver_free(ctx);
}

static ErlDrvSSizeT i2c_drv_ctl(ErlDrvData d, 
				 unsigned int cmd, char* buf0, ErlDrvSizeT len,
				 char** rbuf, ErlDrvSizeT rsize)
{
    i2c_ctx_t* ctx = (i2c_ctx_t*) d;
    uint8_t* buf = (uint8_t*) buf0;

    DEBUGF("i2c_drv: ctl: cmd=%u, len=%d", 
	   cmd, len);
    switch(cmd) {
    case CMD_OPEN: {
	i2c_dev_t* ptr;
	char path[32];
	uint16_t bus;
	int n;
	int fd;

	if (len != 2) goto badarg;
	bus = get_uint16(buf);
	if (find_dev(ctx, bus) != NULL)
	    goto ok; // already open
	n = snprintf(path, sizeof(path), "/dev/i2c-%d", bus);
	if (n >= sizeof(path)) goto badarg;
	if ((fd = open(path, O_RDWR, 0)) < 0)
	    goto error;
	if ((ptr = driver_alloc(sizeof(i2c_dev_t))) == NULL) {
	    close(fd);
	    errno = ENOMEM;
	    goto error;
	}
	add_dev(ctx, bus, ptr);
	ptr->fd = fd;
	goto ok;
    }
    
    case CMD_CLOSE: {
	i2c_dev_t** pp;
	i2c_dev_t* ptr;
	uint16_t bus;
	
	if (len != 2) goto badarg;
	bus = get_uint16(buf);

	if ((ptr = find_dev(ctx, bus, &pp)) == NULL)
	    goto ok;
	driver_select(ctx->port, (ErlDrvEvent)ptr->fd, ERL_DRV_USE, 0);
	*pp = ptr->next; // unlink
	driver_free(ptr);
	goto ok;
    }


    default:
	goto badarg;
    }

ok:
    return ctl_reply(0, NULL, 0, rbuf, rsize);
badarg:
    errno = EINVAL;
error:
    {
        char* err_str = erl_errno_id(errno);
	return ctl_reply(255, err_str, strlen(err_str), rbuf, rsize);
    }
}


static void i2c_drv_output(ErlDrvData d, char* buf, ErlDrvSizeT len)
{
    (void) d;
    (void) buf;
    (void) len;
    // i2c_ctx_t* ctx = (i2c_ctx_t*) d;
    DEBUGF("i2c_drv: output");
}

static void i2c_drv_outputv(ErlDrvData d, ErlIOVec *ev)
{
    (void) d;
    (void) ev;
    // i2c_ctx_t* ctx = (i2c_ctx_t*) d;
    DEBUGF("i2c_drv: outputv");
}

static void i2c_drv_event(ErlDrvData d, ErlDrvEvent e,
			  ErlDrvEventData ed)
{
    (void) d;
    (void) e;
    (void) ed;
    // i2c_ctx_t* ctx = (i2c_ctx_t*) d;
    DEBUGF("i2c_drv: event called");
}

static void i2c_drv_ready_input(ErlDrvData d, ErlDrvEvent e)
{
    (void) d;
    (void) e;
    // i2c_ctx_t* ctx = (i2c_ctx_t*) d;
    DEBUGF("i2c_drv: ready_input called");
}

static void i2c_drv_ready_output(ErlDrvData d, ErlDrvEvent e)
{
    (void) d;
    (void) e;
    // i2c_ctx_t* ctx = (i2c_ctx_t*) d;
    DEBUGF("i2c_drv: ready_output called");
}

// operation timed out
static void i2c_drv_timeout(ErlDrvData d)
{
    (void) d;
    DEBUGF("i2c_drv: timeout");
}

static void i2c_drv_stop_select(ErlDrvEvent event, void* arg)
{
    (void) arg;
    DEBUGF("i2c_drv: stop_select event=%d", INT_EVENT(event));
    close(INT_EVENT(event));
}

DRIVER_INIT(i2c_drv)
{
    ErlDrvEntry* ptr = &i2c_drv_entry;

    DEBUGF("spi DRIVER_INIT");

    ptr->driver_name = "i2c_drv";
    ptr->init  = i2c_drv_init;
    ptr->start = i2c_drv_start;
    ptr->stop  = i2c_drv_stop;
    ptr->output = i2c_drv_output;
    ptr->ready_input  = i2c_drv_ready_input;
    ptr->ready_output = i2c_drv_ready_output;
    ptr->finish = i2c_drv_finish;
    ptr->control = i2c_drv_ctl;
    ptr->timeout = i2c_drv_timeout;
    ptr->outputv = i2c_drv_outputv;
    ptr->ready_async = 0;
    ptr->flush = 0;
    ptr->call = 0;
    ptr->event = i2c_drv_event;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = 0;
    ptr->stop_select = i2c_drv_stop_select;
    return ptr;
}
