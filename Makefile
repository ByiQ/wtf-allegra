
#
# Makefile for Allegra, Ada IRC info-bot
#
# Chip Richards, NiEstu, Phoenix AZ, December 2003
#

# Configuration
PGADA_INC   = $(shell pgada-conf compile incdir cflags)
PGADA_LIB   = $(shell pgada-conf compile slibdir ldflags) -lpq
DEBUG       =

# Program structure
MAIN          = allegra
IDENTITY_BODY = source/identity.adb
TASK_PACKAGES = \
	source/command \
	source/database \
	source/file \
	source/input \
	source/net \
	source/output \
	source/ping
QUEUE_PACKAGES = \
	source/commandq \
	source/outputq
QUEUE_SPECS = \
	source/databaseq \
	source/netq \
	source/fileq
UTIL_PACKAGES = \
	source/auth \
	source/config \
	source/db \
	source/irc \
	source/log
PACKAGES       = ${TASK_PACKAGES} ${QUEUE_PACKAGES} ${UTIL_PACKAGES}
PACKAGE_SPECS  = $(addsuffix .ads,${PACKAGES} ${QUEUE_SPECS})
PACKAGE_BODIES = $(addsuffix .adb,${PACKAGES})

# The top-level target
all: app_ident ${MAIN}

# Program-building targets
${MAIN}: source/${MAIN}.adb ${PACKAGE_SPECS} ${PACKAGE_BODIES} ${IDENTITY_BODY}
	gnatmake -fstack-check -g -gnata -gnat05 -gnato -gnatVa -gnatwa -gnatW8 -gnatiw -s -z ${DEBUG} source/${MAIN}.adb -L/opt/postgresql/current/lib -largs "-lpq"

# App identity package body is produced by preprocessor
NAME=$(shell perl -e 'print ucfirst ("${MAIN}")')
VER=$(shell svn propget version .)
REV=$(shell svnversion)
VERREV="${VER}r${REV}"
BUILD_DATE=$(shell date +'%d %b %Y')

${IDENTITY_BODY}: app_ident

app_ident: source/identity.adp source/identity.ads
	gnatprep -DAppName=\"${NAME}\" -DAppVer=\"${VERREV}\" -DBuild_Date="\"${BUILD_DATE}\"" $< ${IDENTITY_BODY}

# Utility targets
clean:
	rm -f b~* *.o *.ali

realclean: clean
	rm ${MAIN}

# Target to create snapshot tarball
snapshot:
	support/make-snapshot ${MAIN}-${VERREV}
