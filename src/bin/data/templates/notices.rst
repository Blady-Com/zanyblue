.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

Notices
=======

ZanyBlue software includes code and data from the following open source
projects:
{% for notice in notices %}
* :ref:`zb-notice-{{ notice.name }}-{{ notice.version }}`
{% endfor %}

The conditions for the use is detailed below.

{% for notice in notices %}
.. _zb-notice-{{ notice.name }}-{{ notice.version }}:

{{ notice.title }}

{{ notice.description }}

{% if notice.notice %}
The copyright notice for this project is:

.. code-block:: none

    {{ notice.notice }}
{% endif %}

{% for subnotice in notice.notices %}

.. _zb-notice-$notice.name-{{ notice.version }}-{{ subnotice.name }}:

{{ subnotice.title }}

The copyright notice for this is:

.. code-block:: none

    {{ subnotice.notice }}
{% endfor %}
{% endfor %}
