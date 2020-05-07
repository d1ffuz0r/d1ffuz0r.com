N2O Logging. Custom backend
###########################

:date: 2020-05-07
:tags: Erlang, Lager, N2O, logging

Re-posting from 2014-10-17

In this tutorial we will show you how to create a custom logging backend for N2O. We will use Lager_ as a logging library.

As we described in the previous (`N2O Logging`_) tutorial, the logging backend should be implementing 3 methods:

  * warning/3
  * info/3
  * error/3

So, let's get started.

**Note:** For this example, we will use code from (`N2O Logging`_) tutorial.

As a first step we should add ``lager`` to the dependencies list in ``rebar.config``.

.. code::

  vim rebar.config

.. code::

  {deps, [
    {n2o, ".*", {git, "git://github.com/5HT/n2o.git", {tag, "HEAD"}}},
    {lager, ".*", {git, "git://github.com/basho/lager", {tag, "2.0.3"}}}  % added lager
  ]}.

.. image:: images/n2o_lager/logb_rebar.png
    :width: 750

And add to the applications list in ``web.app.src`` for an automatic lager_ starting

.. code::

  vim apps/web/src/web.app.src

.. code::

  {application, web,
   [
    {description, ""},
    {vsn, "1"},
    {registered, []},
    {applications, [
                    kernel,
                    stdlib,
                    n2o,
                    lager   % added lager
                   ]},
    {mod, { web_app, []}},
    {env, []}
   ]}.

.. image:: images/n2o_lager/logb_cfg.png

Next, create a new file where we will implement the required functions.

.. code::

  cd apps/web/src      % go to the app directory
  touch n2o_lager.erl  % create an empty file

Open ``n2o_lager.erl`` file

.. code::

  vim n2o_lager.erl

And write the implementation. The code is pretty straight forward. You can extend this code also. How? I we will show you later.

.. code::

  -module(n2o_lager).
  -export([info/3, warning/3, error/3]).
  -compile([{parse_transform, lager_transform}]).            % force compile

  format_message(Module, String) ->                          % log formatter "module:message"
      wf:to_list([Module, ":", String]).

  info(Module, String, Args) ->
      lager:info(format_message(Module, String), Args).

  warning(Module, String, Args) ->
      lager:warning(format_message(Module, String), Args).

  error(Module, String, Args) ->
      lager:error(format_message(Module, String), Args).

.. image:: images/n2o_lager/logb_code.png

Now, when the implementation is ready, let's configure N2O for using our module.

Open ``web_app.erl`` and edit ``start/0`` function. We should set up out module as the N2O logging backend.

.. code::

  vim web_app.erl

.. code::

   start() ->
      application:ensure_all_started(web),
      application:set_env(n2o, route, routes),
      application:set_env(n2o, log_modules, web_sup),
      application:set_env(n2o, log_backend, n2o_lager),  % setup a backend for logging
      application:start(web).

.. image:: images/n2o_lager/logb_app.png

The backend is ready. Let's add some logs to the view files.

.. code::

  cd ../../../      % go to rood directory
  ./rebar get-deps  % get dependencies
  ./run.sh          % start app

And make a call

.. code::

  curl localhost:9002

.. image:: images/n2o_lager/logb_example.png
    :width: 750

Links
_____

* Source code: https://github.com/d1ffuz0r/n2o_tutorials/tree/master/6_n2o_logging
* Custom N2O logging backends: https://github.com/d1ffuz0r/n2o.logging

* `N2O Logging`_

.. _`N2O Logging`: /n2o-logging.html
.. _Lager: https://github.com/basho/lager
.. _Erlang: http://erlang.org/
