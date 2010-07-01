
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
IDENTITY_BODY = identity.adb
TASK_PACKAGES = \
	command \
	database \
	file \
	input \
	net \
	output \
	ping
QUEUE_PACKAGES = \
	commandq \
	outputq
QUEUE_SPECS = \
	databaseq \
	netq \
	fileq
UTIL_PACKAGES = \
	auth \
	config \
	db \
	irc \
	log
PACKAGES       = ${TASK_PACKAGES} ${QUEUE_PACKAGES} ${UTIL_PACKAGES}
PACKAGE_SPECS  = $(addsuffix .ads,${PACKAGES} ${QUEUE_SPECS})
PACKAGE_BODIES = $(addsuffix .adb,${PACKAGES})

# The top-level target
all: app_ident ${MAIN}

# Program-building targets
${MAIN}: ${MAIN}.adb ${PACKAGE_SPECS} ${PACKAGE_BODIES} ${IDENTITY_BODY}
	gnatmake ${DEBUG} ${MAIN}.adb ${PGADA_INC} -largs -lnail ${PGADA_LIB}

# App identity package body is produced by preprocessor
NAME=$(shell perl -e 'print ucfirst ("${MAIN}")')
VER=$(shell svn propget version .)
REV=$(shell svnversion)
VERREV="${VER}r${REV}"
BUILD_DATE=$(shell date +'%d %b %Y')

${IDENTITY_BODY}: app_ident

app_ident: identity.adp identity.ads
	gnatprep -DAppName=\"${NAME}\" -DAppVer=\"${VERREV}\" -DBuild_Date="\"${BUILD_DATE}\"" $< ${IDENTITY_BODY}

# Utility targets
clean:
	rm -f b~* *.o *.ali

realclean: clean
	rm ${MAIN}

# Target to create snapshot tarball
snapshot:
	support/make-snapshot ${MAIN}-${VERREV}
