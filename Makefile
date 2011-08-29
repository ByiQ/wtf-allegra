
#
# Makefile for Allegra, Ada IRC info-bot
#
# Chip Richards, NiEstu, Phoenix AZ, December 2003
#

MAIN = allegra
IDENTITY_BODY = source/identity.adb

all: app_ident ${MAIN}

${MAIN}:
	gnatmake -P ${MAIN}.gpr

# App identity package body is produced by preprocessor
VERSION    = ""
BUILD_DATE = $(shell date +'%d %b %Y')

${IDENTITY_BODY}: app_ident

app_ident: source/identity.adp source/identity.ads
	gnatprep -DAppName=\"${MAIN}\" -DAppVer=\"${VERSION}\" -DBuild_Date="\"${BUILD_DATE}\"" $< ${IDENTITY_BODY}

# Utility targets
clean:
	rm ${IDENTITY_BODY}
	gnatclean -P allegra

realclean: clean
	rm ${MAIN}

# Target to create snapshot tarball
snapshot:
	support/make-snapshot ${MAIN}-${VERREV}
