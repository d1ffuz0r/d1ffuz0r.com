10 Erlang one liners to impress your friends
############################################
:date: 2012-05-28 21:20
:tags: erlang


1. Multiple Each Item in a List by 2
------------------------------------

::

    [X * 2 || X <- lists:seq(1, 11)].


2. Sum a List of Numbers
------------------------

::

    lists:sum(lists:seq(1, 1001)).


3.Verify if Exists in a String
------------------------------

::

    Wordslist = ["scala", "akka", "play framework", "sbt", "typesafe"].
    Tweet = "This is an example tweet talking about scala and sbt".
    [lists:member(S, Wordslist) || S <- string:tokens(Tweet, " ")].


4. Read in a File
-----------------

::

    file:read_file("ten_one_liners.erl").


5. Happy Birthday to You
------------------------

::

    ["Happy Birthday " ++ case X of 2 -> "dear Robert"; _ -> "You" end || X <- lists:seq(1, 4)].


6. Filter list of nubmers
-------------------------

::

    [X || X <- lists:seq(40, 60), X >= 50].


7. Fetch and Parse an XML web service
-------------------------------------

::

    inets:start().
    xmerl_scan:string(element(3, element(2, httpc:request("http://search.twitter.com/search.atom?&q=erlang")))).


8. Find minimum (or maximum) in a List
--------------------------------------

::

    lists:min(lists:seq(1, 10)).
    lists:max(lists:seq(1, 10)).


9. Parralel Processing
----------------------

::

    [spawn(fun() -> io:format("~w~n", [X * 2]) end) || X <- lists:seq(1, 10)].


10. Sieve of Eratosthenes
-------------------------

::

    N = 50.
    [X || X <- lists:usort(lists:seq(2, N + 1)), not lists:member(X, lists:usort([(P * F) || P <- lists:seq(2, round(math:pow(N, 0.5)) + 2), F <- lists:seq(2, round(N / P))]))].

Bonus

11. Error Logging with [CrashDump.io](https://crashdump.io)
-----------------------------------------------------------

::

    fink:add_sasl_backend().        % Enable logging
    error_logger:error_msg("HELP"). % Send notification

