
#
# Makefile for "alba" Allegra technology demonstrator
#
# Chip Richards, NiEstu, Phoenix AZ, November 2003
#

#
# Configuration
XSLT        = xslt
LITPROG_DIR = /opt/litprog
XSL_DIR     = ${LITPROG_DIR}/xsl
TANGLE      = ${XSL_DIR}/tangle.xsl
WEAVE       = ${XSL_DIR}/wdocbook.xsl
HTML_XSLT   = ${XSL_DIR}/ldocbook.xsl
XWEBPP      = xweb-include

#
# Program structure
MAIN           = alba
PACKAGES       = command file database input output ping
XWEB_PARTS     = $(addsuffix .xweb,${PACKAGES})
PACKAGE_SPECS  = $(addsuffix .ads,${PACKAGES})
PACKAGE_BODIES = $(addsuffix .adb,${PACKAGES})
FULL_XWEB      = ${MAIN}-full.xweb

#
# The top-level target
all: ${MAIN}

#
# Program-building targets
${MAIN}: ${MAIN}.adb ${PACKAGE_SPECS} ${PACKAGE_BODIES}
	gnatmake -gnatv ${MAIN}.adb

${MAIN}.adb: ${MAIN}.xweb
	${XWEBPP} ${MAIN}.xweb ${FULL_XWEB}
	$(XSLT) top="alba-body" ${FULL_XWEB} $(TANGLE) $@

%.ads: ${MAIN}.xweb %.xweb
	${XWEBPP} ${MAIN}.xweb ${FULL_XWEB}
	$(XSLT) top="$(basename $(notdir $@))-pkg-spec" ${FULL_XWEB} $(TANGLE) $@

%.adb: ${MAIN}.xweb %.xweb
	${XWEBPP} ${MAIN}.xweb ${FULL_XWEB}
	$(XSLT) top="$(basename $(notdir $@))-pkg-body" ${FULL_XWEB} $(TANGLE) $@

#
# Document-building targets
doc: ${MAIN}.html

${MAIN}.html: ${MAIN}.xweb ${XWEB_PARTS}
	${XWEBPP} ${MAIN}.xweb ${FULL_XWEB}
	${XSLT} ${FULL_XWEB} ${WEAVE} ${MAIN}.xml
	${XSLT} ${MAIN}.xml ${HTML_XSLT} $@

#
# Utility targets
clean:
	rm -f b~* *.o *.ali ${MAIN}.adb ${MAIN}.xml ${FULL_XWEB} ${PACKAGE_SPECS} ${PACKAGE_BODIES}

realclean: clean
	rm ${MAIN} ${MAIN}.html
