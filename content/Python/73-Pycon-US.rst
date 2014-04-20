PyCon US 2014
#############

:date: 2014-04-29 15:33
:tags: python, pycon, canada


PyCon is over. This year the conference was at Montreal, Canada. The country looks pretty much as Russia from the window of airplane and city is very european. In comparison with PyCons I've visited before it was my biggest PyCon. I was impressed with how many people do use iPython[ notebook].
I have learned a lot of new stuff there and met many new people. Hope I will visit PyCon next year again.

All Your Ducks In A Row: Data Structures in the Standard Library and Beyond
---------------------------------------------------------------------------
A great talk. Brandon explained how does work and what you should expect from standard types and libraries.

There're several notes:

..

    list is an array of references

    class is a dict

    implementation __slots__ converts an object to the struct (takes less memory)

And also provided a list of libraries what you could know but have never use:

..

    heapq.PriorityQueue

    sched.schdules

Must have to watch for every pythonista

Rate: 10/10

Link: http://www.pyvideo.org/video/2571/all-your-ducks-in-a-row-data-structures-in-the-s

Cache me if you can: memcached, caching patterns and best practices
-------------------------------------------------------------------

As described in title this talk has covered various cashing patterns and best practices.

Tip: use versioning for cashing you data

Rate: 10/10

Link: http://www.pyvideo.org/video/2578/cache-me-if-you-can-memcached-caching-patterns

Import-ant Decisions
--------------------

Basic example how you can implement your own import system. Watch just for fun.

Rate: 4/10

Link: http://www.pyvideo.org/video/2567/import-ant-decisions


Enough Machine Learning to Make Hacker News Readable Again
----------------------------------------------------------

A good example how you can spend 6 months to filter HN clicking on your own "like" button.

Rate: Silicon Valley^hipster/10

Link: http://www.pyvideo.org/video/2612/enough-machine-learning-to-make-hacker-news-reada


How to Get Started with Machine Learning
----------------------------------------

Description and introduction into things you necessary to know if you want to dive into machine learning.

Rate: Silicon Valley/10

Link: http://www.pyvideo.org/video/2604/how-to-get-started-with-machine-learning


Realtime predictive analytics using scikit-learn & RabbitMQ
-----------------------------------------------------------

Explained on example how to build the system using scikit-learn and RabbitMQ. Can be useful who is joining the rabbitmq + scikit-learn world.

Rate: 8/10

Link: http://www.pyvideo.org/video/2606/realtime-predictive-analytics-using-scikit-learn


Distributed Computing Is Hard, Lets Go Shopping
-----------------------------------------------

Greates talk I've visited on this PyCon. Must have to watch for people is who working on distributed data processing. You will learn what issues you can meet and how to solve it, how to do proper testing and monitoring.

Notes:

..

    testing: test task function, test single task, test 1 process, test several processes to avoid race condition
    monitoring: celery - flower, rabbitmq - rabbitmq management


Rate: 100/10

Link: http://www.pyvideo.org/video/2598/distributed-computing-is-hard-lets-go-shopping


Fan-in and Fan-out: The crucial components of concurrency
---------------------------------------------------------

import asyncio

Rate: asyncio/10

Link: http://www.pyvideo.org/video/2572/fan-in-and-fan-out-the-crucial-components-of-con

Track memory leaks in Python
----------------------------

Must have to watch every pythonista. Explained GC in python and techniques of debugging and detecting memory leaks.

..

    objgraph - http://mg.pov.lt/objgraph/

    memory_profiler - https://pypi.python.org/pypi/memory_profiler

    pytracemalloc - https://github.com/wyplay/pytracemalloc

    tracemallocqt - https://bitbucket.org/haypo/tracemallocqt

Tip: keep eyes on object's references

Rate: 10/10

Link: http://www.pyvideo.org/video/2698/track-memory-leaks-in-python


Garbage Collection in Python
----------------------------

If you trying to work with many objects and want to help python handle all manually this talk is good for you.

Tip: it happens rarely but when it happened, you need to be ready

Rate: 10/10

Link: http://www.pyvideo.org/video/2633/garbage-collection-in-python


Designing Poetic APIs
---------------------

Explanation on example how to make your code, API, UI cleaner and better. Must have to watch

Rate: 10/10

Link: http://www.pyvideo.org/video/2647/designing-poetic-apis


Introduction to SQLAlchemy Core
-------------------------------

Use cases and examples how to use SQLAlchemy without using ORM.

Tip: use declarative for convinient interaction with existing databases (e.g. migrations or data extract).

Rate: 7/10

Link: http://www.pyvideo.org/video/2654/introduction-to-sqlalchemy-core


Sane schema migrations with Alembic and SQLAlchemy
--------------------------------------------------

Migrations for SQLAlchemy.

Tip: http://alembic.readthedocs.org/en/latest/

Rate: 10/10

Link: http://www.pyvideo.org/video/2693/sane-schema-migrations-with-alembic-and-sqlalchem


In Depth PDB
------------

Tips & tricks.


Tip: install pdb++


Rate: 10/10

Link: http://www.pyvideo.org/video/2673/in-depth-pdb


Python packaging simplified, for end users, app developers, and open source contributors
----------------------------------------------------------------------------------------

Described techniques and examples of packaging libraries in Python. Make your work and deploy easier. Must have.

Tip: vagrant

Rate: wheels/10

Link: http://www.pyvideo.org/video/2631/python-packaging-simplified-for-end-users-app-d


What is coming in Python packaging
----------------------------------

Explained current state and the future of Python packaging systems.

PYPI's hidden gems

..

    https://pypi.python.org/pypi/<name>/json

    https://pypi.python.org/pypi/<name>/<ver>/json

    https://pypi.python.org/pypi/requests/json


Tip: do not use wheels if you have C extensions


Rate: 10/10

Link: http://www.pyvideo.org/video/2677/what-is-coming-in-python-packaging


Deliver Your Software In An Envelope
------------------------------------

Good talk about designing, delivering code and how good documentation is important. Must have to watch.

Rate: 10/10

Link: http://www.pyvideo.org/video/2632/deliver-your-software-in-an-envelope


PostgreSQL is Web Scale (Really :) )
------------------------------------

Exaplained how to scale and use classic RDBMS.

Tip: PostgreSQL is enough for 90% of projects.

Rate: 10/10

Link: http://www.pyvideo.org/video/2650/postgresql-is-web-scale-really


Set your code free: releasing and maintaining an open-source Python project
---------------------------------------------------------------------------

Deliver your stuff properly! Must have to watch.

Rate: Github/10

Link: http://www.pyvideo.org/video/2637/set-your-code-free-releasing-and-maintaining-an
