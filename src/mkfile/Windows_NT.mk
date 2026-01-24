#
# Unix GNU make definitions to support ZanyBlue builds.
#

# Extension used for executables
E=.exe

# Explicitly use "python script.py" on Windows
PYTHON=python

# This is a real hack to get the year and probably doesn't work on non-English
# systems!
CURRENT_YEAR=$(notdir $(lastword $(shell echo %DATE%)))

define COPY
copy $(subst /,\,$1) $(subst /,\,$2)
endef

define DELETE
del /f $(subst /,\,$1)
endef

define MKDIR
mkdir $(subst /,\,$1)
endef
