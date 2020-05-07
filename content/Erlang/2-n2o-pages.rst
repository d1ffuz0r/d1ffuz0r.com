N2O templates. Static HTML
##########################

:date: 2020-05-02
:tags: Erlang, N2O, Tutorial

Re-posting from 2014-09-24

It's a time to learn more N2O. In this article we will show how to render templates using different techniques:


* Static html template
* N2O record-based templates
* Erlydtl_ templates

Environment
-----------

For this example, we will use a project we have created in the previous tutorial_.
A directory structure will be like that:

.. code::

    example|master⚡ ⇒ tree .
    .
    ├── apps
    │   └── web
    │       ├── ebin
    │       └── src
    │           ├── index.erl
    │           ├── routes.erl
    │           ├── web.app.src
    │           ├── web_app.erl
    │           └── web_sup.erl
    ├── deps
    ├── rebar
    └── rebar.config

Static html
-----------

This method will be useful in case if you want to have some static files, but do not want to run a web-server on the development machine.

For routing and handling requests N2O is using the cowboy_ web server, so rendering a static html file basically will be through configuring a cowboy handler. For this step we should create an HTML file in the `priv` directory of our `web` app and add route to the routes map.

Create a directory and file:

.. code::

  $ mkdir apps/web/priv                 # create a 'priv' directory
  $ touch apps/web/priv/example.html    # create an HTML file
  $ echo "<h1>Example page</h1>" > apps/web/priv/example.html # insert content

As you remember we defined routes in `web_sup:rules/0` function.
So let's go and add our example page to the routes map.

.. code::

  $ vim apps/web/src/web_sup.erl

.. code::

  rules() ->
    cowboy_router:compile(
      [{'_', [
         {"/example",                                    % handling url
           cowboy_static,                                % cowboy handler
           {priv_file, web, "example.html",              % a place where the file is saved
             [
               {mimetypes, {<<"text">>, <<"html">>, []}} % mime type
             ]
           }
         },
         {'_', n2o_cowboy, []}
       ]}]
    ).

A 'cowboy_router:compile/1' is accepting a list of tuples with domain + routes definitions.

.. image:: images/n2o_pages/static-routes.png
    :width: 750

Well, it's done. Let's compile our project, run and check it out in browser by  http://localhost:8002/example url.


.. code::

  $ ./rebar compile
  $ erl -name "web@$(hostname)" -pa deps/*/ebin -pa apps/*/ebin -boot start_sasl -s web_app start

.. image:: /images/n2o_pages/static-example.png
    :width: 750

Source code
___________

https://github.com/d1ffuz0r/n2o_tutorials/tree/master/2_n2o_pages

Links
_____

How to create a page using Nitrogen templates read in the next part: `N2O templates. Nitrogen`_

.. _Erlydtl: https://github.com/erlydtl/erlydtl
.. _tutorial: /hello-n2o.html
.. _cowboy: https://github.com/ninenines/cowboy
.. _`N2O templates. Nitrogen`: /n2o-templates-nitrogen.html
