Erlang Logging Libraries
########################

:date: 2020-05-07
:tags: Erlang, Logging, Libraries

Re-posting from 2015-01-07

Erlang has many logging libraries for any taste. We want to collect all of them to a single list and help you select the best one for your next or current project.

error_logger
------------

**error_logger** is a built in Erlang/OTP event manager which is can be used for error and warning logging. This is a great solution for simple logs.

**License:** Mit

**Rebar compatible:**

**Link:** http://erlang.org/doc/man/error_logger.html

Lager
-----

**Lager** is a great logging framework, built by company Basho_.

Lager now is de-facto a standard library for logging logging  in the Erlang world. There are many custom backends and extensions for Lager you can find on GitHub.

**License:** Apache

**Rebar compatible:** Yes

**Link:** https://github.com/basho/lager

Log4erl
-------

**Log4erl** is another great logging framework for Erlang. Has simple and clean API.

**License:** MOZILLA PUBLIC

**Rebar compatible:** Yes

**Link:** https://github.com/ahmednawras/log4erl

alogger
-------

**alogger** is positioning itself as an "Abstract Logger Interface" which allows you implement you own logger. It also has a few built in interfaces which coveres basic use cases.

**License:** Apache

**Rebar compatible:** Yes

**Link:** https://github.com/siberian-fast-food/alogger

Twig
----

**Twig** is a "SASL-compliant Erlang/OTP logger". Provides a bridge between the "error_logger" and "syslog" server over UDP protocol.

**License:** Unknown

**Rebar compatible:** No

**Link:** https://github.com/cloudant/twig


elogger
-------

**elogger** is a logging library extracted from the jungerl_ project. Unfortunately has almost no documentation except basic configuration examples.

**License:** Unknown

**Rebar compatible:** No

**Link:** https://github.com/etnt/elogger

elog
----

**elog** is a last but not least logging library for Erlang. Unfortunately project is not supported anymore. Authors are suggesting to use Lager instead.

**License:** Mit

**Rebar compatible:** Yes

**Link:** https://github.com/inaka/elog


.. _Basho: http://basho.com

.. _jungerl: http://jungerl.sourceforge.net/


Thank you!
