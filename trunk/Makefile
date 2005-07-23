
#
# Makefile for Allegra, Ada IRC info-bot
#
# Chip Richards, NiEstu, Phoenix AZ, December 2003
#

#
# Configuration
PGADA_DIR   = /opt/pgada
PGADA_INC   = ${PGADA_DIR}/include
PGADA_LIB   = ${PGADA_DIR}/lib
DEBUG       =

#
# Program structure
MAIN          = allegra
IDENTITY_BODY = identity.adb
TASK_PACKAGES = \
	command \
	database \
	file \
	input \
	output \
	ping
QUEUE_PACKAGES = \
	commandq \
	outputq
QUEUE_SPECS = \
	databaseq \
	fileq
UTIL_PACKAGES = \
	auth \
	config \
	db \
	irc \
	log
LIBRARY_PACKAGES = \
	pqueue \
	strings \
	times
PACKAGES       = ${TASK_PACKAGES} ${QUEUE_PACKAGES} ${UTIL_PACKAGES} ${LIBRARY_PACKAGES}
PACKAGE_SPECS  = $(addsuffix .ads,${PACKAGES} ${QUEUE_SPECS})
PACKAGE_BODIES = $(addsuffix .adb,${PACKAGES})

#
# The top-level target
all: app_ident ${MAIN}

#
# Program-building targets
${MAIN}: ${MAIN}.adb ${PACKAGE_SPECS} ${PACKAGE_BODIES} ${IDENTITY_BODY}
	gnatmake ${DEBUG} ${MAIN}.adb -aI${PGADA_INC} -aO${PGADA_LIB} $$(adasockets-config) -lpgada

#
# App identity package body is produced by preprocessor
NAME=$(shell perl -e 'print ucfirst ("${MAIN}")')
VER=$(shell svn propget version .)
REV=$(shell svn info . | grep Revision | sed -e 's/.* //')
VERREV="${VER}r${REV}"
BUILD_DATE=$(shell date +'%d %b %Y')

${IDENTITY_BODY}: app_ident

app_ident: identity.adp identity.ads
	gnatprep -DAppName=\"${NAME}\" -DAppVer=\"${VERREV}\" -DBuild_Date="\"${BUILD_DATE}\"" $< ${IDENTITY_BODY}

#
# Utility targets
clean:
	rm -f b~* *.o *.ali

realclean: clean
	rm ${MAIN}
