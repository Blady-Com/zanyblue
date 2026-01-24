#
# Unix GNU make definitions to support ZanyBlue builds.
#

# Extension used for executables
E=

# Don't need to explicitly use "python script.py" on Unix
PYTHON=

# Current year
CURRENT_YEAR=$(shell date +%Y)

define COPY
cp -p $1 $2
endef

define DELETE
rm -f $1
endef

define MKDIR
mkdir $1
endef
