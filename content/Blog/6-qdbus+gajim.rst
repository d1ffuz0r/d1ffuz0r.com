qdbus+gajim
############

:date: 2011-10-01 02:37

статус aкка jabber.ru

::

  qdbus org.gajim.dbus /org/gajim/dbus/RemoteObject org.gajim.dbus.RemoteInterface.get_status_message jabber.ru

количество непрочитаных сообщений

::

   qdbus org.gajim.dbus /org/gajim/dbus/RemoteObject org.gajim.dbus.RemoteInterface.get_unread_msgs_number
