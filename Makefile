
#
# Makefile for Allegra, Ada IRC info-bot
#
# Chip Richards, NiEstu, Phoenix AZ, December 2003
#

MAIN = allegra
IDENTITY_DEF  = templates/identity.adp
IDENTITY_SPEC = source/identity.ads

all: app_ident ${MAIN}

${MAIN}:
	gnatmake -P ${MAIN}.gpr

# App identity package body is produced by preprocessor
APP_NAME   = Allegra
VERSION    = $(shell git log -1 --oneline | cut -d \  -f 1)
BUILD_DATE = $(shell date)

app_ident: ${IDENTITY_SPEC}

${IDENTITY_SPEC}: ${IDENTITY_DEF}
	gnatprep -DApp_Name="\"${APP_NAME}\"" -DApp_Version="\"${VERSION}\"" -DBuild_Date="\"${BUILD_DATE}\"" ${IDENTITY_DEF} ${IDENTITY_SPEC}

# Utility targets
clean:
	rm ${IDENTITY_SPEC}
	gnatclean -P allegra
	rm build/identity.*

# Target to create snapshot tarball
snapshot:
	support/make-snapshot ${MAIN}-${VERREV}
