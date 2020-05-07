N2O templates. Nitrogen
#######################

:date: 2020-05-03
:tags: Erlang, N2O, Tutorial

Re-posting from 2014-09-25

N2O template (Nitrogen) basically is a bunch of Erlang records compiled into HTML code. You can read more about templates at https://synrc.com/apps/n2o/doc/web/ page. Here we want to show how all steps of the page creation. It will be done in several steps:

* Create a view
* Add a route to the routes.erl file

Let's create a view

.. code::

  $ touch apps/web/src/n2o_page.erl

Open file in an editor and add required functions

.. code::

  $ vim apps/web/src/n2o_page.erl


.. code::

  -module(n2o_page).                       % module name
  -export([main/0]).                       % function extraction
  -include_lib("n2o/include/wf.hrl").      % include wf.hrl header file

  main() ->                                % definition of default called function
    #panel{body = <<"Hello records template!">>}.

.. image:: images/n2o_pages/page-view.png
    :width: 750

Our first simplest view is ready. On the next step we should add the view to the routes.

Open routes.erl file and map path to the view

.. code::

  $ vim apps/web/src/routes.erl


.. code::

  route(<<"/n2o_page">>) -> n2o_page;  % n2o_page view will be rendered by /n2o_page url
  route(_)               -> index.     % default view. '_' is matching any url

.. image:: images/n2o_pages/page-route.png
    :width: 750

Ready. Let's compile the project and open in browser  http://localhost:8002/n2o_page url.


.. code::

  $ ./rebar compile
  $ erl -name "web@$(hostname)" -pa deps/*/ebin -pa apps/*/ebin -boot start_sasl -s web_app start

.. image:: images/n2o_pages/page-example.png
    :width: 750

Source code
___________

https://github.com/d1ffuz0r/n2o_tutorials/tree/master/2_n2o_pages

Links
_____

How to use Erlydtl templates read in the next part: `N2O templates. Erlydtl`_

How to create a view with a static HTML page read in a previous article: `N2O templates. Static HTML`_


.. _Erlydtl: https://github.com/erlydtl/erlydtl
.. _tutorial: /hello-n2o.html
.. _cowboy: https://github.com/ninenines/cowboy
.. _`N2O templates. Static HTML`: /n2o-templates-static-html.html
.. _`N2O templates. Erlydtl`: /n2o-templates-erlydtl.html
