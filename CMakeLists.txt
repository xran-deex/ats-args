CMAKE_MINIMUM_REQUIRED (VERSION 3.10)
set(ATS_HOME /home/randy/ATS/ATS-Postiats-Github)
set(CMAKE_C_COMPILER "${ATS_HOME}/bin/atscc")
PROJECT (DEPTEST C)
FIND_PACKAGE (ATS REQUIRED)
IF (NOT ATS_FOUND)
	MESSAGE (FATAL_ERROR ”ATS Not Found!”)
ENDIF ()
ATS_INCLUDE (src/SATS src/DATS src/HATS)
ATS_INCLUDE (node_modules)
set(CMAKE_C_FLAGS "-DATS_MEMALLOC_LIBC -D_DEFAULT_SOURCE")

IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/node_modules)
ADD_SUBDIRECTORY(node_modules/ats-result ats-result)
else()
ATS_INCLUDE (..)
# ADD_SUBDIRECTORY(../ats-result ats-result)
endif()

ATS_COMPILE (TEST_SRC
	src/SATS/arg.sats
	src/DATS/arg.dats
	src/SATS/lib.sats
	src/DATS/lib.dats
	)
ADD_LIBRARY (atsargs ${TEST_SRC})
