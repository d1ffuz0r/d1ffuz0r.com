crashkeeper
###########

:date: 2014-02-21 22:35
:tags: erlang, projects, dev



`CrashKeeper`_ is a web service for collecting exceptions, logs and other things happened in your application.

The main point of the CrashKeeper - deep understanding of your information, such like logs with custom user-based parsers for their logs, better understanding language dependents (**WOW ERLANG, SUCH CLOJURE**) exceptions and messages (hello Sentry) and building statistics. There are also will be available integration with tons of task-tracker services and notification using a lot of protocols, everyone has it. And bigger messages limits per second.

.. image:: http://crashkeeper.com/images/logo1.png
   :align: center

Eventually, after 6 months of lazy development in a free time I would to say that `CrashKeeper`_ is going to testing stage. Lot of things done but much more I need to do.
Started development when I've lived in Thailand after move to Califonia the process has slow down and I didn't finish a beta-version before 2014 as I expected. So now it's a time to catch up.

Little bit about project. A backend has written on `Erlang`_ and `N2O`_ web-framework, at client-side I use `Chaplin.js`_. Also there are an awesome thinks uch like RabbitMQ, PostgreSQL, Memcached, Mnesia. With all awesome thing in n2o using early versions had give some problems, like I've been forced to rewrite api to be able use "new ideas" of the framework. After 6 months using Chaplin.js I can say that It's the best thing which I have ever seen for JavaScript. Very helpful for development now is using Vagrant and Puppet. For now that's it, I will publish more information at Crashkeeper Blog later.


Apply for a beta-test now and you will get a free year `Ultimate`_-subscription.

.. _crashkeeper: http://crashkeeper.com
.. _erlang: http://www.erlang.org/
.. _n2o: https://github.com/5HT/n2o
.. _chaplin.js: http://chaplinjs.org/
.. _ultimate: http://crashkeeper.com/pricing
