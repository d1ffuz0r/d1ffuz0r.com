Title: 10 Erlang one liners to impress your friends
Date: 2012-05-28 21:20
Tags: erlang

10 Erlang one liners to impress your friends
=========

1. Multiple Each Item in a List by 2:
-------------------------------------------------------------------------------

```erlang
[X * 2 || X <- lists:seq(1, 11)].
```

2. Sum a List of Numbers:
-------------------------------------------------------------------------------

```erlang
lists:sum(lists:seq(1, 1001)).
```

3.Verify if Exists in a String:
-------------------------------------------------------------------------------

```erlang
Wordslist = ["scala", "akka", "play framework", "sbt", "typesafe"].
Tweet = "This is an example tweet talking about scala and sbt".
[lists:member(S, Wordslist) || S <- string:tokens(Tweet, " ")].
```

4. Read in a File:
-------------------------------------------------------------------------------

```erlang
file:read_file("ten_one_liners.erl").
```

5. Happy Birthday to You:
-------------------------------------------------------------------------------

```erlang
["Happy Birthday " ++ case X of 2 -> "dear Robert"; _ -> "You" end || X <- lists:seq(1, 4)].
```

6. Filter list of nubmers:
-------------------------------------------------------------------------------

```erlang
[X || X <- lists:seq(40, 60), X >= 50].
```

7. Fetch and Parse an XML web service:
-------------------------------------------------------------------------------

```erlang
inets:start().
xmerl_scan:string(element(3, element(2, httpc:request("http://search.twitter.com/search.atom?&q=erlang")))).
```

8. Find minimum (or maximum) in a List
-------------------------------------------------------------------------------

```erlang
lists:min(lists:seq(1, 10)).
lists:max(lists:seq(1, 10)).
```

9. Parralel Processing
-------------------------------------------------------------------------------

```erlang
[spawn(fun() -> io:format("~w~n", [X * 2]) end) || X <- lists:seq(1, 10)].
```

10. Sieve of Eratosthenes
-------------------------------------------------------------------------------

```erlang
N = 50.
[X || X <- lists:usort(lists:seq(2, N + 1)), not lists:member(X, lists:usort([(P * F) || P <- lists:seq(2, round(math:pow(N, 0.5)) + 2), F <- lists:seq(2, round(N / P))]))].
```
