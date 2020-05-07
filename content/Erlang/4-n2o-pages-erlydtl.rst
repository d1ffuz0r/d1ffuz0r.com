N2O templates. Erlydtl
######################

:date: 2020-05-04
:tags: Erlang, N2O, Tutorial

Re-posting from 2014-09-26

If you do use Django, ROR or any other modern framework, you should be familiar with templates. In N2O you can use Erlydtl_ templates. In this part we will show how to create a view that will be rendering a simple Erlydtl template.

For this step we should follow these steps

* Create a view
* Create a Erlydtl template
* Add a route to the routes.erl file
* Create a new rebar.config in `web` directory for the proper template compilation

.. code::

  $ touch apps/web/src/erlydtl_page.erl       # create a view
  $ touch apps/web/src/rebar.config           # create rebar config
  $ mkdir apps/web/priv/templates             # create templates directory
  $ touch apps/web/priv/templates/example.dtl # create dtl template file

Let's start from a route

.. code::

  $ vim apps/web/src/routes.erl

.. code::

  route(<<"/erlydtl_page">>) -> erlydtl_page;   % erlydtl page mapping
  route(<<"/n2o_page">>)     -> n2o_page;
  route(_)                   -> index.

Next we will add a content to the dtl template. Basic template with a single variable

.. code::

  $ echo "Hello Erlydtl. I am {{name}}!" > apps/web/priv/templates/example.dtl

Now when template is ready, let's configure rebar.

Open rebar.config in the web directory

.. code::

  $ vim apps/web/rebar.config

.. code::

  {erlydtl_opts, [
      {doc_root, "priv/templates"},                     % directory with templates
      {out_dir, "ebin"},                                % output directory
      {compiler_options, [report, return, debug_info]}, % compiler options
      {source_ext, ".dtl"},                             % templates extension
      {module_ext, "_view"}                             % module extension
  ]}.

After that, during compilation you should see messages about template compilation

.. image:: images/n2o_pages/erlydtl-compilation.png
    :width: 750

And finally, edit the view

.. code::

  $ vim apps/web/src/erlydtl_page.erl


.. code::

  -module(erlydtl_page).
  -export([main/0]).
  -include_lib("n2o/include/wf.hrl").

  main() ->
    [
      #dtl{file="example",     % filename
           ext="dtl",          % extension
           bindings=[
             {name, <<"N2O">>} % template bindings
           ]}
    ].

.. image:: images/n2o_pages/erlydtl-view.png
    :width: 750

Now we can compile, run our project and check it out in browser by  http://localhost:8002/erlydtl_page url.


.. code::

  $ ./rebar compile
  $ erl -name "web@$(hostname)" -pa deps/*/ebin -pa apps/*/ebin -boot start_sasl -s web_app start


.. image:: images/n2o_pages/erlydtl-example.png
    :width: 750

Source code
___________

https://github.com/d1ffuz0r/n2o_tutorials/tree/master/2_n2o_pages

Links
_____

How to create a page using Nitrogen templates read in the previous part: `N2O templates. Nitrogen`_


.. _Erlydtl: https://github.com/erlydtl/erlydtl
.. _tutorial: /hello-n2o.html
.. _cowboy: https://github.com/ninenines/cowboy
.. _`N2O templates. Nitrogen`: /n2o-templates-nitrogen.html
