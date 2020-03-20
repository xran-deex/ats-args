CMAKE_MINIMUM_REQUIRED (VERSION 3.10)

PROJECT (ATSARGS C)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/node_modules)
	ADD_SUBDIRECTORY(node_modules/ats-result ats-result)
	ADD_SUBDIRECTORY(node_modules/hashtable-vt hashtable-vt)
ENDIF()

FIND_PACKAGE (ATS REQUIRED)
IF (NOT ATS_FOUND)
	MESSAGE (FATAL_ERROR ”ATS Not Found!”)
ENDIF ()

ATS_INCLUDE (src/SATS src/DATS src/HATS)
set(CMAKE_C_FLAGS "-DATS_MEMALLOC_LIBC -D_DEFAULT_SOURCE -g")

IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/node_modules)
	ATS_INCLUDE (node_modules)
else()
	ATS_INCLUDE (.. "$ENV{ATSPKGS}")
endif()

ATS_COMPILE (SRC
	src/SATS/arg.sats
	src/DATS/arg.dats
	src/SATS/args.sats
	src/DATS/args.dats
	src/SATS/parser.sats
	src/DATS/parser.dats
	src/SATS/helper.sats
	src/DATS/helper.dats
	)
ADD_LIBRARY (atsargs ${SRC})
