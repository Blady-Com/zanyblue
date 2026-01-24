.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

Notices
=======

ZanyBlue software includes code and data from the following open source
projects:
#foreach( $notice in $notices )
* :ref:`zb-notice-$notice.name-$notice.version`
#end

The conditions for the use is detailed below.

#foreach( $notice in $notices )
.. _zb-notice-$notice.name-$notice.version:

$notice.title

$notice.description

#if($notice.notice)
The copyright notice for this project is:

.. code-block:: none

    $notice.notice
#end

#foreach( $subnotice in $notice.notices )

.. _zb-notice-$notice.name-$notice.version-$subnotice.name:

$subnotice.title

The copyright notice for this is:

.. code-block:: none

    $subnotice.notice
#end
#end
