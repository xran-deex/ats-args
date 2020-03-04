CMAKE_MINIMUM_REQUIRED (VERSION 3.10)
set(ATS_HOME /home/randy/ATS/ATS-Postiats-Github)

PROJECT (ARGARGS C)

FIND_PACKAGE (ATS REQUIRED)
IF (NOT ATS_FOUND)
	MESSAGE (FATAL_ERROR ”ATS Not Found!”)
ENDIF ()

ATS_INCLUDE (src/SATS src/DATS src/HATS)
set(CMAKE_C_FLAGS "-DATS_MEMALLOC_LIBC -D_DEFAULT_SOURCE")

IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/node_modules)
	ATS_INCLUDE (node_modules)
	ADD_SUBDIRECTORY(node_modules/ats-result ats-result)
else()
	ATS_INCLUDE (.. "$ENV{ATSPKGS}")
endif()

ATS_COMPILE (SRC
	src/SATS/arg.sats
	src/DATS/arg.dats
	src/SATS/lib.sats
	src/DATS/lib.dats
	)
ADD_LIBRARY (atsargs ${SRC})
