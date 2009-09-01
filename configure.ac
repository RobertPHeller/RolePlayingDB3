AC_INIT()

AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_SUBST(build)
AC_CANONICAL_TARGET
AC_SUBST(target)

dnl Force cross_compiling, even if it *looks* like we can run cross-built 
dnl binaries (get around wine's cleverness).
if test ${cross_compiling} = "maybe"; then
  if test ${target_os} != ${build_os}; then
    cross_compiling=yes
  fi
fi  


RPGDB_VERSION=3.0
AC_DEFINE_UNQUOTED([RPGDB_VERSION],$RPGDB_VERSION,[RPGDB_VERSION])
RPGDB_MAJOR_VERSION=3
AC_DEFINE_UNQUOTED([RPGDB_MAJOR_VERSION],$RPGDB_MAJOR_VERSION,[RPGDB Major])
RPGDB_MINOR_VERSION=0
AC_DEFINE_UNQUOTED([RPGDB_MINOR_VERSION],$RPGDB_MINOR_VERSION,[RPGDB Minor])
RPGDB_PATCH_LEVEL="3.0.0"
AC_DEFINE_UNQUOTED([RPGDB_PATCH_LEVEL],$RPGDB_PATCH_LEVEL,[RPGDB Patch Level])
RPGDB_PATCH_LEVELLIB=3.0.0

AM_INIT_AUTOMAKE( RolePlayingDB, $RPGDB_PATCH_LEVELLIB )



if test "x${prefix}" = "xNONE"; then
    prefix=/usr/local
fi
if test "x${exec_prefix}" = "xNONE"; then
    exec_prefix="${prefix}"
fi

BUILD_DIR=`pwd`
TOP_DIR=${BUILD_DIR}

AC_SUBST(TOP_DIR)
AC_SUBST(BUILD_DIR)

AC_SUBST(target)

dnl Checks for programs.

AC_PROG_CPP
AC_PROG_CXX
AC_PROG_CC
dnl if test ${cross_compiling} = "no"; then
dnl   AM_DISABLE_STATIC
dnl fi
AC_LIBTOOL_DLOPEN
if test ${cross_compiling} = "yes"; then
  lt_cv_deplibs_check_method='pass_all'
fi
AM_PROG_LIBTOOL
AC_PATH_PROG(PDFLATEX,pdflatex,/bin/true)
AC_PATH_PROGS(HTLATEX,[htlatex htlatex.sh],/bin/true)
AC_PATH_PROG(LATEX,latex,/bin/true)
AC_PATH_PROG(MAKEINDEX,makeindex,/bin/true)
AC_PATH_PROG(BIBTEX,bibtex,/bin/true)
AC_PATH_PROG(DVIPS,dvips,/bin/true)
AC_PATH_PROG(PS2PDF,ps2pdf,/bin/true)
dnl AC_PATH_PROG(DOCXX,doc++,/bin/true)
AC_PATH_PROG(SWIG,swig,/bin/true)
AC_MSG_CHECKING([Checking Swig version])
  $SWIG -version >/dev/null 2> /tmp/swig_version$pid
  SWIGVERSION=`cat /tmp/swig_version$pid | grep Version | awk '{print $3}'`
  rm /tmp/swig_version$pid
  AC_SUBST(SWIGVERSION)
  AC_MSG_RESULT([SWIG Version is $SWIGVERSION])
  SWIGVERSIONMAJOR=`echo $SWIGVERSION|awk -F. '{print $1}'`
  AC_SUBST(SWIGVERSIONMAJOR)
  AC_MSG_RESULT([SWIG Major version is $SWIGVERSIONMAJOR])
  SWIGVERSIONMINOR=`echo $SWIGVERSION|awk -F. '{print $2}'`
  AC_SUBST(SWIGVERSIONMINOR)
  AC_MSG_RESULT([SWIG Minor version is $SWIGVERSIONMINOR])
  if test ${SWIGVERSIONMINOR} == 1 && test ${SWIGVERSIONMAJOR} == 1; then
    AC_MSG_ERROR([Your version of SWIG is old, you need to upgrade to 1.3!])
  else
    SWIGVERSIONFLAGS="-DSWIGVERSION=\$(SWIGVERSION) -DSWIGVERSIONMAJOR=\$(SWIGVERSIONMAJOR) \
-DSWIGVERSIONMINOR=\$(SWIGVERSIONMINOR)"
  fi
  AC_SUBST(SWIGVERSIONFLAGS)

dnl if test ${cross_compiling} = "yes"; then
dnl   LIB_RPGDB_PATCH_LEVELLIB=`echo $RPGDB_PATCH_LEVELLIB|sed 's/\./-/g'`
dnl else
dnl   LIB_RPGDB_PATCH_LEVELLIB=$RPGDB_PATCH_LEVELLIB
dnl fi
dnl AC_SUBST(LIB_RPGDB_PATCH_LEVELLIB)

dnl Checks for header files.
AC_HEADER_STDC

AC_C_CONST
AC_C_INLINE
AC_MSG_CHECKING(tcl build dir)
dnl AC_MSG_NOTICE([cross_compiling = ${cross_compiling}])
if test ${cross_compiling} = "yes"; then
  dnl AC_MSG_NOTICE([cross_compiling -- finding cross tcl: CC is $CC])
  gccbin=`which $CC`
  gccbindir=`dirname $gccbin`
  crossdir=`dirname $gccbindir`
  crossexec_prefix=$crossdir/${target_cpu}-${target_os}
  TCL_LIB_DIR=$crossexec_prefix/lib
else
  TCL_LIB_DIR=$exec_prefix/lib
  dirs="${BUILD_DIR}/../tcl8.3a*/unix \
	${BUILD_DIR}/../tcl8.3b*/unix \
	${BUILD_DIR}/../tcl8.4.*/unix \
	${BUILD_DIR}/../tcl8.5.*/unix \
	/usr/share /usr/lib"
  for i in $dirs ; do
    if test -d "$i" -a -f "$i/tclConfig.sh"; then
	TCL_LIB_DIR=`cd $i; pwd`
    fi
  done
fi
dnl AC_MSG_NOTICE([TCL_LIB_DIR initialized as $TCL_LIB_DIR])
AC_ARG_WITH(tcl, [  --with-tcl=DIR          use Tcl binaries from DIR], TCL_LIB_DIR=$withval,)
if test -d $TCL_LIB_DIR; then
    AC_MSG_RESULT([using tcl library in $TCL_LIB_DIR])
else
    AC_MSG_ERROR(Tcl directory $TCL_LIB_DIR doesn't exist)
fi

cd ${TCL_LIB_DIR}/..
TCL_TOP_DIR=`pwd`
cd ${BUILD_DIR}

#--------------------------------------------------------------------

file=$TCL_LIB_DIR/tclConfig.sh
. $file
SHLIB_CFLAGS=`echo "$TCL_SHLIB_CFLAGS" |sed -e "s|PIC|pic|" -e "s|+Z|+z|"`
SHLIB_LD=`echo $TCL_SHLIB_LD|sed "s/-Wl,-soname,libtcl.so.0//g"`
SHLIB_SUFFIX=$TCL_SHLIB_SUFFIX
DL_LIBS=$TCL_DL_LIBS
LD_FLAGS=$TCL_LD_FLAGS

TCL_BASE=`echo ${TCL_LIB_SPEC} |sed -e "s|^.*-l||"`
TCL_STUB_BASE=`echo ${TCL_STUB_LIB_SPEC} |sed -e "s|^.*-l||"`

AC_SUBST(TCL_LIB_DIR)
if test -f ${TCL_LIB_DIR}/tclsh${TCL_VERSION}; then
    TCL_BIN_DIR=${TCL_LIB_DIR}
elif test -f ${TCL_EXEC_PREFIX}/bin/tclsh${TCL_VERSION}; then
    TCL_BIN_DIR=${TCL_EXEC_PREFIX}/bin
elif test -f ${TCL_EXEC_PREFIX}/tclsh; then
    TCL_BIN_DIR=${TCL_EXEC_PREFIX}
elif test -f ${TCL_EXEC_PREFIX}/bin/tclsh; then
    TCL_BIN_DIR=${TCL_EXEC_PREFIX}/bin
else
    TCL_BIN_DIR=$exec_prefix/bin
fi
AC_SUBST(TCL_BIN_DIR)
if test -f ${TCL_BIN_DIR}/tclsh${TCL_VERSION}; then
    TCLSH=${TCL_BIN_DIR}/tclsh${TCL_VERSION}
else
    TCLSH=${TCL_BIN_DIR}/tclsh
fi
AC_SUBST(TCLSH)
if test -f ${TCL_BIN_DIR}/wish${TCL_VERSION}; then
    WISH=${TCL_BIN_DIR}/wish${TCL_VERSION}
else
    WISH=${TCL_BIN_DIR}/wish
fi
AC_SUBST(WISH)
if test -d ${TCL_TOP_DIR}/generic -a -f ${TCL_TOP_DIR}/generic/tcl.h; then
    TCL_INC_DIR=${TCL_TOP_DIR}/generic
elif test -d ${TCL_TOP_DIR}/include -a -f ${TCL_TOP_DIR}/include/tcl.h; then
    TCL_INC_DIR=${TCL_TOP_DIR}/include
elif test -d ${TCL_TOP_DIR}/include -a -f ${TCL_TOP_DIR}/include/tcl.h; then
    TCL_INC_DIR=${TCL_TOP_DIR}/include
elif test -d ${TCL_EXEC_PREFIX}/include -a -f ${TCL_EXEC_PREFIX}/include/tcl.h; then
    TCL_INC_DIR=${TCL_EXEC_PREFIX}/include
elif test -d ${TCL_EXEC_PREFIX}/include -a -f ${TCL_EXEC_PREFIX}/include/tcl.h; then
    TCL_INC_DIR=${TCL_EXEC_PREFIX}/include
else
    TCL_INC_DIR=$prefix/include
fi
AC_SUBST(TCL_INC_DIR)
if test -d ${TCL_TOP_DIR}/lib/tcl${TCL_VERSION} -a -f ${TCL_TOP_DIR}/lib/tcl${TCL_VERSION}/init.tcl; then
    TCL_LIBRARY=${TCL_TOP_DIR}/lib/tcl${TCL_VERSION}
elif test -d ${TCL_TOP_DIR}/lib/tcl${TCL_VERSION} -a -f ${TCL_TOP_DIR}/lib/tcl${TCL_VERSION}/init.tcl; then
    TCL_LIBRARY=${TCL_TOP_DIR}/lib/tcl${TCL_VERSION}
elif test -d ${TCL_TOP_DIR}/library -a -f ${TCL_TOP_DIR}/library/init.tcl; then
    TCL_LIBRARY=${TCL_TOP_DIR}/library
elif test -d ${TCL_PREFIX}/lib/tcl${TCL_VERSION} -a -f ${TCL_PREFIX}/lib/tcl${TCL_VERSION}/init.tcl; then
    TCL_LIBRARY=${TCL_PREFIX}/lib/tcl${TCL_VERSION}
elif test -d ${TCL_PREFIX}/lib/tcl${TCL_VERSION} -a -f ${TCL_PREFIX}/lib/tcl${TCL_VERSION}/init.tcl; then
    TCL_LIBRARY=${TCL_PREFIX}/lib/tcl${TCL_VERSION}
else
    TCL_LIBRARY=$prefix/lib/tcl${TCL_VERSION}
fi
AC_SUBST(TCL_LIBRARY)

TCL_LIB_SPEC="-L${TCL_LIB_DIR} -l${TCL_BASE}"
TCL_STUB_LIB_SPEC="-L${TCL_LIB_DIR} -l${TCL_STUB_BASE}"
eval "TCL_UNSHARED_LIB_FILE=lib${TCL_BASE}${TCL_UNSHARED_LIB_SUFFIX}"
eval "TCL_SHARED_LIB_FILE=lib${TCL_BASE}${TCL_SHARED_LIB_SUFFIX}"

#--------------------------------------------------------------------
LIB_INSTALL_DIR="${LIB_RUNTIME_DIR}"
#--------------------------------------------------------------------
#	The statements below define a collection of symbols related to
#	building libimg as a shared library instead of a static library.
#--------------------------------------------------------------------

TCL_SHARED_LIBS=""
#--------------------------------------------------------------------
AC_ARG_ENABLE(stubs, [  --disable-stubs         disable stub support for Img],
    [img_ok=$enableval], [img_ok=yes])
if test "$img_ok" != "no"; then
    TCL_DEFS="$TCL_DEFS -DUSE_TCL_STUBS"
    TCL_SHARED_LIBS="${TCL_STUB_LIB_SPEC}"
else
    case "$SHLIB_LD" in
	*ldAix*)
	TCL_SHARED_LIBS='${TCL_LIB_SPEC}'
	;;
    esac
fi

case "$SHLIB_LD" in
    *ldAix*)
    TCL_SHARED_LIBS="${TCL_SHARED_LIBS}"
    ;;
esac

if test ${cross_compiling} = "no"; then
  HOSTTCLSH=${TCLSH}
else
  AC_PATH_PROGS(HOSTTCLSH,[tclsh8.4 tclsh])
fi

AC_MSG_CHECKING(bwidget dir)
BWLIB=`echo 'puts $tcl_library'|${HOSTTCLSH}`
dirs="${TCL_PREFIX}/lib/bwidget* ${TCL_PREFIX}/lib/tcllib*/bwidget* ${BWLIB}/bwidget* ${BWLIB}/../tcllib*/bwidget* ${BWLIB}/../bwidget* ${BWLIB}/tcllib*/bwidget*"
for i in $dirs ; do
  if test -d "$i" -a -f "$i/pkgIndex.tcl"; then
    BWLIB=`cd $i; pwd`
  fi
done
AC_ARG_WITH(bwlib, [  --with-bwlib=DIR          use BWidget from DIR], BWLIB=$withval,)
if test -d $BWLIB; then
   AC_MSG_RESULT([using BWidget library in $BWLIB])
else
   AC_MSG_ERROR(BWidget library directory $BWLIB doesn't exist)
fi
AC_SUBST(BWLIB)

AC_MSG_CHECKING(snit dir)
SNITLIB=`echo 'puts $tcl_library'|${HOSTTCLSH}`

dirs="${TCL_PREFIX}/lib/snit* ${TCL_PREFIX}/lib/tcllib*/snit* ${SNITLIB}/../tcllib*/snit* ${SNITLIB}/../snit* ${SNITLIB}/tcllib*/snit* ${SNITLIB}/snit*"
for i in $dirs ; do
  if test -d "$i" -a -f "$i/pkgIndex.tcl"; then
    SNITLIB=`cd $i; pwd`
  fi
done
AC_ARG_WITH(snitlib, [  --with-snitlib=DIR          use snit from DIR], SNITLIB=$withval,)
if test -d $SNITLIB; then
   AC_MSG_RESULT([using snit library in $SNITLIB])
else
   AC_MSG_ERROR(Snit library directory $SNITLIB doesn't exist)
fi
AC_SUBST(SNITLIB)

AC_MSG_CHECKING(uri dir)
URILIB=`echo 'puts $tcl_library'|${HOSTTCLSH}`

dirs="${TCL_PREFIX}/lib/uri* ${TCL_PREFIX}/lib/tcllib*/uri* ${URILIB}/../tcllib*/uri* ${URILIB}/../uri* ${URILIB}/tcllib*/uri* ${URILIB}/uri*"
for i in $dirs ; do
  if test -d "$i" -a -f "$i/pkgIndex.tcl"; then
    URILIB=`cd $i; pwd`
  fi
done
AC_ARG_WITH(urilib, [  --with-urilib=DIR          use uri from DIR], URILIB=$withval,)
if test -d $URILIB; then
   AC_MSG_RESULT([using uri library in $URILIB])
else
   AC_MSG_ERROR(Uri library directory $URILIB doesn't exist)
fi
AC_SUBST(URILIB)

AC_PATH_PROGS(UNZIP,unzip)

AC_MSG_CHECKING([Checking target Img, Tclxml,  and tclkit])
case $target_os in 
  macos | darwin) 
    AC_SUBST(IMG_ZIP, Img-1.3-Mac-Universal.zip)
    AC_SUBST(TCLKITRUN, tclkit-darwin-univ-aqua)
    AC_MSG_ERROR([Unsupported target os for Tclxml: $target_os])
    ;;
  linux-gnu)
    case $target_cpu in
      x86_64)
	AC_SUBST(IMG_ZIP, Img-1.3-Linux64.zip)
	AC_SUBST(TCLKITRUN, tclkit-linux-x86_64)
	AC_MSG_ERROR([Unsupported Linux arch for Tclxml: $target_cpu])
	;;
      i*86)
	AC_SUBST(IMG_ZIP, Img-1.3-Linux32.zip)
	AC_SUBST(TCLKITRUN, tclkit-linux-x86)
	AC_SUBST(TCLXML_ZIP, Tclxml3.1-Linux32.zip)
	;;
      *)
	AC_MSG_ERROR([Unsupported Linux arch: $target_cpu])
	;;
     esac
     ;;

   mingw32 | winnt | windows* | cygwin)
     AC_SUBST(IMG_ZIP, Img-1.3-win32.zip)
     AC_SUBST(TCLKITRUN, tclkit-win32.exe)
     AC_SUBST(TCLXML_ZIP, Tclxml2.6-win32.zip)
     AC_SUBST(EXEEXT, .exe)
     ;;
   *) 
     AC_MSG_ERROR([Unsupported Operating System: $target_os])
     ;;
esac
AC_MSG_RESULT([IMG_ZIP is $IMG_ZIP, TCLXML_ZIP is $TCLXML_ZIP, and TCLKITRUN is $TCLKITRUN])


AC_MSG_CHECKING([Checking build tclkit])
case $build_os in 
  macos | darwin) 
    AC_SUBST(TCLKITBSRC, tclkit-darwin-univ-aqua)
    AC_SUBST(BEXEEXT, )
    ;;
  linux-gnu)
    AC_SUBST(BEXEEXT, )
    case $build_cpu in
      x86_64)
	AC_SUBST(TCLKITBSRC, tclkit-linux-x86_64)
	;;
      i*86)
	AC_SUBST(TCLKITBSRC, tclkit-linux-x86)
	;;
      *)
	AC_MSG_ERROR([Unsupported Linux arch: $build_cpu])
	;;
     esac
     ;;

   mingw32 | winnt | windows* | cygwin)
     AC_SUBST(BEXEEXT, .exe)
     AC_SUBST(TCLKITBSRC, tclkit-win32.exe)
     ;;
   *) 
     AC_MSG_ERROR([Unsupported Operating System: $build_os])
     ;;
esac
AC_MSG_RESULT([TCLKITBSRC is $TCLKITBSRC])

AC_PATH_PROG(SED,sed)

dnl Substitutions...

AC_SUBST(CXX)
AC_SUBST(CXXFLAGS)
AC_SUBST(SHLIB_CFLAGS)
AC_SUBST(SHLIB_CXXFLAGS)
SHLIB_LD=`echo $SHLIB_LD|sed -e "s/${TCL_CC}/${CXX}/g"`
AC_SUBST(SHLIB_LD)
SHLIB_LD_LIBS=$TCL_SHLIB_LD_LIBS
AC_SUBST(SHLIB_LD_LIBS)
AC_SUBST(SHLIB_SUFFIX)
AC_SUBST(TCL_DBGX)
AC_SUBST(TCL_VERSION)
AC_SUBST(TCL_DEFS)
AC_SUBST(TCL_LIBS)
AC_SUBST(TCL_STUB_LIB_SPEC)
AC_SUBST(TCL_STUB_LIB_PATH)
AC_SUBST(TCL_SHARED_LIBS)
AC_SUBST(TCL_SHARED_LIB_FILE)
AC_SUBST(TCL_UNSHARED_LIB_FILE)
AC_SUBST(TCL_SHLIB_CFLAGS)
AC_SUBST(TCL_SHLIB_SUFFIX)
TCL_SHLIB_LD=`echo $TCL_SHLIB_LD|sed -e "s/${TCL_CC}/${CXX}/g"`
AC_SUBST(TCL_SHLIB_LD)
AC_SUBST(TCL_PREFIX)
AC_SUBST(TCL_EXEC_PREFIX)
AC_SUBST(TCL_CFLAGS)
AC_SUBST(TCL_LIB_SPEC)
SYSTEMVERSION="$RPGDB_PATCH_LEVEL"
AC_SUBST(SYSTEMVERSION)

AC_OUTPUT(Makefile \
	  BuildScripts/Makefile \
	  BuildData/Makefile \
	  Scripts/Makefile \
	  Doc/Makefile \
		Doc/Support/Makefile \
		Doc/User/Makefile \
		Doc/Internals/Makefile \
	  ManPages/Makefile \
		ManPages/TclAPI/Makefile \
		ManPages/Programs/Makefile \
	  Installer/Makefile \
	  RolePlayingDB3.spec
	  )




