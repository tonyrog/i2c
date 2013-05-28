i2c
===================

i2c is an erlang application for controlling the i2c (Inter-Integrated Circuit) linux interface.

### Dependencies

To build rfZone you will need a working installation of Erlang R15B (or
later).<br/>
Information on building and installing [Erlang/OTP](http://www.erlang.org)
can be found [here](https://github.com/erlang/otp/wiki/Installation)
([more info](https://github.com/erlang/otp/blob/master/INSTALL.md)).

rfZone is built using rebar that can be found [here](https://github.com/basho/rebar), with building instructions [here](https://github.com/basho/rebar/wiki/Building-rebar).

### Download

Clone the repository in a suitable location:

```sh
$ git clone git://github.com/tonyrog/i2c.git
```

### Configuration
#### Raspberry pi

Comment out (enable) the i2c blacklist entry in ```/etc/modprove.d/raspo-blacklist.conf```, it should read:
```sh
# blacklist i2c-bcm2708
```

Install the i2c tools:

```sh
sudo apt-get install i2c-tools
```

Load the driver:

```sh
sudo modprobe i2c-dev
```

Test the driver - example, list the i2c supported functions:

```sh
sudo i2cdetect -F 0
```


### Run

### Run

i2c is started in a standard erlang fashion:

```
$ erl
(node@host) 1> application:start(i2c).
```

#### Experiment without hardware

modprobe i2c-stub


### API

The following interface functions exist:
<ul>
<li>opent</li>
<li>close</li>
<li>set_retries</li>
<li>set_timeout</li>
<li>set_slave</li>
<li>set_slave_force</li>
<li>set_tenbit</li>
<li>set_pec</li>
<li>get_funcs</li>
<li>rdwr</li>
<li>smbus</li>
</ul>

For details see the source code documentation or the source code.

### Documentation

i2c is (partly) documented using edoc. To generate the documentation do:

```sh
$ cd i2c
$ rebar doc
```
The result is a collection of html-documents under ```i2c/doc```.
