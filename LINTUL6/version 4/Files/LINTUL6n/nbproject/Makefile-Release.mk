#
# Generated Makefile - do not edit!
#
# Edit the Makefile in the project folder instead (../Makefile). Each target
# has a -pre and a -post target defined where you can add customized code.
#
# This makefile implements configuration specific macros and targets.


# Environment
MKDIR=mkdir
CP=cp
GREP=grep
NM=nm
CCADMIN=CCadmin
RANLIB=ranlib
CC=gcc
CCC=g++
CXX=g++
FC=gfortran
AS=as

# Macros
CND_PLATFORM=MinGW-Windows
CND_DLIB_EXT=dll
CND_CONF=Release
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/_ext/796314804/CLS.o \
	${OBJECTDIR}/_ext/188910145/aux_routines.o \
	${OBJECTDIR}/_ext/188910145/ttutil_win32.o \
	${OBJECTDIR}/_ext/1472/Lintul6vr.o \
	${OBJECTDIR}/_ext/1029452278/Cropv6.o \
	${OBJECTDIR}/_ext/1029452278/Dailout6.o \
	${OBJECTDIR}/_ext/1029452278/INITP6.o \
	${OBJECTDIR}/_ext/1029452278/LINTULSUB.o \
	${OBJECTDIR}/_ext/1029452278/STOUT6.o \
	${OBJECTDIR}/_ext/1029452278/SWEAF.o \
	${OBJECTDIR}/_ext/1029452278/Timer6.o \
	${OBJECTDIR}/_ext/1029452278/WATBALS6.o \
	${OBJECTDIR}/_ext/1029452278/WEATHR.o


# C Compiler Flags
CFLAGS=

# CC Compiler Flags
CCFLAGS=
CXXFLAGS=

# Fortran Compiler Flags
FFLAGS=

# Assembler Flags
ASFLAGS=

# Link Libraries and Options
LDLIBSOPTIONS=

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/lintul6n.exe

${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/lintul6n.exe: ${OBJECTFILES}
	${MKDIR} -p ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}
	${LINK.f} -o ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/lintul6n ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/_ext/796314804/CLS.o: ../LIBRARY/CLS.for 
	${MKDIR} -p ${OBJECTDIR}/_ext/796314804
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/_ext/796314804/CLS.o ../LIBRARY/CLS.for

${OBJECTDIR}/_ext/188910145/aux_routines.o: ../LIBRWOF/ttutil/aux_routines.for 
	${MKDIR} -p ${OBJECTDIR}/_ext/188910145
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/_ext/188910145/aux_routines.o ../LIBRWOF/ttutil/aux_routines.for

${OBJECTDIR}/_ext/188910145/ttutil_win32.o: ../LIBRWOF/ttutil/ttutil_win32.for 
	${MKDIR} -p ${OBJECTDIR}/_ext/188910145
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/_ext/188910145/ttutil_win32.o ../LIBRWOF/ttutil/ttutil_win32.for

${OBJECTDIR}/_ext/1472/Lintul6vr.o: ../Lintul6vr.for 
	${MKDIR} -p ${OBJECTDIR}/_ext/1472
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/_ext/1472/Lintul6vr.o ../Lintul6vr.for

${OBJECTDIR}/_ext/1029452278/Cropv6.o: ../SOURCENEW/Cropv6.for 
	${MKDIR} -p ${OBJECTDIR}/_ext/1029452278
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/_ext/1029452278/Cropv6.o ../SOURCENEW/Cropv6.for

${OBJECTDIR}/_ext/1029452278/Dailout6.o: ../SOURCENEW/Dailout6.for 
	${MKDIR} -p ${OBJECTDIR}/_ext/1029452278
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/_ext/1029452278/Dailout6.o ../SOURCENEW/Dailout6.for

${OBJECTDIR}/_ext/1029452278/INITP6.o: ../SOURCENEW/INITP6.for 
	${MKDIR} -p ${OBJECTDIR}/_ext/1029452278
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/_ext/1029452278/INITP6.o ../SOURCENEW/INITP6.for

${OBJECTDIR}/_ext/1029452278/LINTULSUB.o: ../SOURCENEW/LINTULSUB.for 
	${MKDIR} -p ${OBJECTDIR}/_ext/1029452278
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/_ext/1029452278/LINTULSUB.o ../SOURCENEW/LINTULSUB.for

${OBJECTDIR}/_ext/1029452278/STOUT6.o: ../SOURCENEW/STOUT6.for 
	${MKDIR} -p ${OBJECTDIR}/_ext/1029452278
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/_ext/1029452278/STOUT6.o ../SOURCENEW/STOUT6.for

${OBJECTDIR}/_ext/1029452278/SWEAF.o: ../SOURCENEW/SWEAF.for 
	${MKDIR} -p ${OBJECTDIR}/_ext/1029452278
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/_ext/1029452278/SWEAF.o ../SOURCENEW/SWEAF.for

${OBJECTDIR}/_ext/1029452278/Timer6.o: ../SOURCENEW/Timer6.for 
	${MKDIR} -p ${OBJECTDIR}/_ext/1029452278
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/_ext/1029452278/Timer6.o ../SOURCENEW/Timer6.for

${OBJECTDIR}/_ext/1029452278/WATBALS6.o: ../SOURCENEW/WATBALS6.for 
	${MKDIR} -p ${OBJECTDIR}/_ext/1029452278
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/_ext/1029452278/WATBALS6.o ../SOURCENEW/WATBALS6.for

${OBJECTDIR}/_ext/1029452278/WEATHR.o: ../SOURCENEW/WEATHR.for 
	${MKDIR} -p ${OBJECTDIR}/_ext/1029452278
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/_ext/1029452278/WEATHR.o ../SOURCENEW/WEATHR.for

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/lintul6n.exe
	${RM} *.mod

# Subprojects
.clean-subprojects:

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
