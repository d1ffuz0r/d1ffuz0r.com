#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals
import datetime

AUTHOR = u'Roman Gladkov'
SITENAME = u'd1ffuz0r - python, erlang, l1f3'
SITEURL = 'https://d1ffuz0r.com'

FEED_DOMAIN = SITEURL
# FEED_ALL_RSS = '/feeds/rss/'
THEME = 'themes/mytheme'
THEME_STATIC_DIR = 'theme'
STATIC_PATHS = ["images"]

TIMEZONE = 'Europe/Moscow'

DEFAULT_LANG = u'en'
DEFAULT_DATE = [2013, 9, 10, 23, 10, 2, 1]
# Feed generation is usually not desired when developing
FEED_ALL_RSS = 'rss.xml'
FEED_ALL_ATOM = False
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
DISPLAY_PAGES_ON_MENU = False
DISPLAY_CATEGORIES_ON_MENU = False

# Blogroll
# LINKS =  (('Pelican', 'http://getpelican.com/'),
#           ('Python.org', 'http://python.org/'),
#           ('Jinja2', 'http://jinja.pocoo.org/'),)

MENUITEMS = (('Home', '/'),
             ('About', '/pages/about.html'),
             ('Links', '/pages/links.html'),
             ('Blog', '/category/blog.html'),
             ('Python', '/category/python.html'),
             ('Erlang', '/tag/erlang.html'),
             ('Archive', '/archives.html'))

GITHUB_URL = 'https://github.com/d1ffuz0r'
# DISQUS_SITENAME = "d1ffuz0rhomesite"

# Social widget
SOCIAL = (('GitHub', GITHUB_URL),
          ('LinkedIn', 'http://www.linkedin.com/in/romangladkov'),)
DEFAULT_PAGINATION = 10

# Uncomment following line if you want document-relative URLs when developing
RELATIVE_URLS = True

PLUGINS = (
    'pelican_gist',
)
