Title: SQLAlchemy managers
Date: 2013-02-04 22:55
Tags: sqlalchemy, django


So, Django have "manager" for the queries https://docs.djangoproject.com/en/1.4/topics/db/managers/#django.db.models.Manager, that is good for creating DSL.

For example, if you want get all users from database for chosen date, you can write some query:

```python
User.objects.filter(birth_date > (date - timedelta(1)),
                    birth_date < date).order_by('-birth_date').all()
```

It looks informative, but hard for testing and reading, so better to write the same code with other way use Manager (in internet you can find how to prepare manager for call with many methods):

```python
User.objects.get_for_date(date).is_index().has_published()
```

That better! Managers makes our live easiest. But, come back to SQLAlchemy.
In SQLAlchemy, previous query you can write through few methods:

```python
session.query(User).filter(User.birth_date >= (date - timedelta(1)),
                           User.birth_date <= date).order_by(User.birth_date.desc()).all()
```

Or

```python
session.query(User).filter_by(birth_date > (date - timedelta(1)),
                              birth_date < date).order_by(User.birth_date.desc()).all()
```

That more informative than query on Django, code repeats SQL query,  complex queries write easy.
But, let's try make it short, like example with Manager in Django. SQLAlchemy do not have that functionality.
Exist few ways to do query is more short:

First: classmethod. People already asked on Stackoverflow about it:
http://stackoverflow.com/questions/6606745/equivalent-of-models-manager-for-sqlalchemy
http://stackoverflow.com/questions/4866489/is-there-something-equivalent-to-djangos-managers-in-sqlalchemy

```python
class User(Base):
    birth_date = Column(DateTime)

    @classmethod
    def get_for_date(cls, session, date):
        return session.query(cls).filter(
            cls.birth_date >= (date - timedelta(1)),
            cls.birth_date <= date
        ).order_by(cls.birth_date.desc()).all()

# use it
users = User.get_for_date(session, date)
```

Okay, looks better than raw query, but what if we are want use more filter, or use few methods in query.
We can to do few methods for each case or pass query in method and return new query, but it's not "DRY" way.

Two: functions

```python
def get_for_date(query, date):
    return query.filter(
              cls.birth_date >= (date - timedelta(1)),
              cls.birth_date <= date
          ).order_by(cls.birth_date.desc())

users = get_for_date(session.query(User))
```

Functional guys probably must to love it. But again, more filters (e.g. we want to get only published entries and who marked for showing at home page):

```python
query = is_index(has_published(get_for_date(session.query(User)))).all()
```

Oh, no. We want Python, but no LISP.

Three: Inherite a `Query` or `DBSession` classes, something like a manager!
For example, recipie for prefiltered queries http://www.sqlalchemy.org/trac/wiki/UsageRecipes/PreFilteredQuery.

You can see, method defined in FilteredQuery, will to avaible in query:

```python
assert session.query(Address).private
```

So, may we must to add methods in runtime? Let's try:

Make "Manager" with staticmethods, what added to our queries new expressions, methods must to get query, add our expression and return new query.

```python
class MainManager:

    @staticmethod
    def is_index(self):
        return self.filter_by(is_index=True)

    @staticmethod
    def is_public(self):
        return self.filter_by(is_public=True)
```

And define attribute to model, for manager. Manager will get class with methods and set their to query

```python
class User(Base):
    __tablename__ = 'main'
    id = Column(Integer, primary_key=True)
    birth_date = Column(DateTime)
    is_index = Column(Boolean, default=False)
    is_public = Column(Boolean, default=False)

    __manager__ = MainManager
```

Then create new session with the `ManagerQuery`. Now, query each time will receive attributes of the model what passed to query, and set methods of  the class with methods to a new query:

```python
...
from alchmanager import ManagedQuery

engine = create_engine('sqlite:///:memory:')
session = sessionmaker(query_cls=ManagedQuery, bind=engine)()
```

Now, this session will add methods from manager each time for in time of creating query if in model which passed to query, have a manager

`__manager__` attribute:
```python
>>>print(session.query(User).is_index)
<bound method ManagedQuery.is_index of <alchmanager.ManagedQuery object at 0x102853e10>>
```

Or, if not exists

```python
>>>print(session.query(User).other_method)
...
AttributeError: 'ManagedQuery' object has no attribute 'other_method'
```

But, what if we want have managers not only for few classes? We can make base class and to inherite from him, that good idea for few classes, or who have little differences. What if we want make manager not only for query, and for all session.
We also can to inherit `DBSession` class and do small hack, to get query class from `DBSession._query_cls` and define methods for all session, to add methods in Query class. That method is not pretty, because I wrote the small decorator, what in starting project loaded and adds methods to query class. See example

```python
...
from alchmanager import ManagedSession

engine = create_engine('sqlite:///:memory:')
session = sessionmaker(class_=ManagedSession, bind=engine)()

@session.load_manager()
class SessionManager:

    @staticmethod
    def published(self):
         return self.filter_by(is_public=True)

    @staticmethod
    def has_index(self):
        return self.filter_by(is_index=True)


class Test(Base):
    id = Column(Integer)


>>>print(session.query(TestModel).published)
<bound method Query.published of <sqlalchemy.orm.query.Query object at 0x109ac5750>>
```

We have manager for session, all queries have methods defined in `SessionManager`, query manager can to replace session methods in query instance.

So now, we have managers for SQLAlchemy for making simple DSL. Get the source code you can at [sqlalchemy-manager](https://github.com/d1ffuz0r/sqlalchemy-manager)


P.S.
Currently I looking for a new remote job, contact me if you have something for me.

P.S.1
If you're hate me and my English, send me some money for my English courses ;)
