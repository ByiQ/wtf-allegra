
#
# Makefile for Allegra, Ada IRC info-bot
#
# Chip Richards, NiEstu, Phoenix AZ, December 2003
#

#
# Configuration
XSLT        = xslt
LITPROG_DIR = /opt/litprog
XSL_DIR     = ${LITPROG_DIR}/xsl
TANGLE      = ${XSL_DIR}/tangle.xsl
WEAVE       = ${XSL_DIR}/wdocbook.xsl
HTML_XSLT   = ${XSL_DIR}/ldocbook.xsl
PGADA_DIR   = /opt/pgada
PGADA_INC   = ${PGADA_DIR}/include
PGADA_LIB   = ${PGADA_DIR}/lib

#
# Program structure
MAIN           = allegra
MAIN_DOC = $(addsuffix .html,${MAIN})
TASK_PACKAGES = \
	command \
	database \
	file \
	input \
	output \
	ping
UTIL_PACKAGES = \
	config \
	db \
	irc \
	log
PACKAGES = ${TASK_PACKAGES} ${UTIL_PACKAGES}
XWEB_PARTS     = $(addsuffix .xweb,${PACKAGES})
PACKAGE_SPECS  = $(addsuffix .ads,${PACKAGES})
PACKAGE_BODIES = $(addsuffix .adb,${PACKAGES})
PACKAGE_DOCS   = $(addsuffix .html,${PACKAGES})

#
# The top-level target
all: ${MAIN} init-config

#
# Program-building targets
${MAIN}: ${MAIN}.adb ${PACKAGE_SPECS} ${PACKAGE_BODIES}
	gnatmake ${MAIN}.adb -aI${PGADA_INC} -aO${PGADA_LIB} $$(adasockets-config) -lpgada

%.ads: %.xweb entities.inc.dtd
	$(XSLT) top="$(basename $(notdir $@))-pkg-spec" $< $(TANGLE) $@

%.adb: %.xweb entities.inc.dtd
	$(XSLT) top="$(basename $(notdir $@))-pkg-body" $< $(TANGLE) $@

init-config: config.xweb entities.inc.dtd
	$(XSLT) top="$(basename $(notdir $@))" $< $(TANGLE) $@
	chmod +x $@

#
# Document-building targets
doc: ${MAIN_DOC} ${PACKAGE_DOCS}

%.html: %.xweb entities.inc.dtd
	${XSLT} $< ${WEAVE} $*.xml
	${XSLT} $*.xml ${HTML_XSLT} $@
	rm $*.xml

#
# Utility targets
clean:
	rm -f b~* *.o *.ali ${MAIN}.adb *.xml ${PACKAGE_SPECS} ${PACKAGE_BODIES}

realclean: clean
	rm ${MAIN} *.html
