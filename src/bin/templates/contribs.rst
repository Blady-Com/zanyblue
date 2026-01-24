.. -*- coding: utf-8 -*-
   Copyright © 2016, Michael Rohan <mrohan@zanyblue.com>
   All rights reserved.

Contributions
=============

This software includes contributions from the following open source developers:
#foreach( $contributor in $contributors )
* :ref:`zb-contrib-$contributor.key`
#end

Contributions to ZanyBlue are contributed under the same BSD style license that
covers the ZanyBlue project.
The following sections document the contributions made to the ZanyBlue project.

#foreach( $contributor in $contributors )
.. _zb-contrib-$contributor.key:

$contributor.title

Contact: `$contributor.email <mailto:$contributor.email>`_.

#foreach( $contribution in $contributor.contributions )
:$contribution.when:
    $contribution.description
#end
#end
