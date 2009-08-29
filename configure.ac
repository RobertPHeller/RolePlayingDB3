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

dnl AC_PROG_CPP
dnl AC_PROG_CXX
dnl AC_PROG_CC
dnl dnl if test ${cross_compiling} = "no"; then
dnl dnl   AM_DISABLE_STATIC
dnl dnl fi
dnl AC_LIBTOOL_DLOPEN
dnl if test ${cross_compiling} = "yes"; then
dnl   lt_cv_deplibs_check_method='pass_all'
dnl fi
dnl AM_PROG_LIBTOOL
AC_PATH_PROG(PDFLATEX,pdflatex,/bin/true)
AC_PATH_PROGS(HTLATEX,[htlatex htlatex.sh],/bin/true)
AC_PATH_PROG(LATEX,latex,/bin/true)
AC_PATH_PROG(MAKEINDEX,makeindex,/bin/true)
AC_PATH_PROG(BIBTEX,bibtex,/bin/true)
AC_PATH_PROG(DVIPS,dvips,/bin/true)
AC_PATH_PROG(PS2PDF,ps2pdf,/bin/true)
dnl AC_PATH_PROG(DOCXX,doc++,/bin/true)
dnl AC_PATH_PROG(SWIG,swig,/bin/true)
dnl AC_MSG_CHECKING([Checking Swig version])
dnl   $SWIG -version >/dev/null 2> /tmp/swig_version$pid
dnl   SWIGVERSION=`cat /tmp/swig_version$pid | grep Version | awk '{print $3}'`
dnl   rm /tmp/swig_version$pid
dnl   AC_SUBST(SWIGVERSION)
dnl   AC_MSG_RESULT([SWIG Version is $SWIGVERSION])
dnl   SWIGVERSIONMAJOR=`echo $SWIGVERSION|awk -F. '{print $1}'`
dnl   AC_SUBST(SWIGVERSIONMAJOR)
dnl   AC_MSG_RESULT([SWIG Major version is $SWIGVERSIONMAJOR])
dnl   SWIGVERSIONMINOR=`echo $SWIGVERSION|awk -F. '{print $2}'`
dnl   AC_SUBST(SWIGVERSIONMINOR)
dnl   AC_MSG_RESULT([SWIG Minor version is $SWIGVERSIONMINOR])
dnl   if test ${SWIGVERSIONMINOR} == 1 && test ${SWIGVERSIONMAJOR} == 1; then
dnl     AC_MSG_ERROR([Your version of SWIG is old, you need to upgrade to 1.3!])
dnl   else
dnl     SWIGVERSIONFLAGS="-DSWIGVERSION=\$(SWIGVERSION) -DSWIGVERSIONMAJOR=\$(SWIGVERSIONMAJOR) \
dnl -DSWIGVERSIONMINOR=\$(SWIGVERSIONMINOR)"
dnl   fi
dnl   AC_SUBST(SWIGVERSIONFLAGS)

dnl if test ${cross_compiling} = "yes"; then
dnl   LIB_RPGDB_PATCH_LEVELLIB=`echo $RPGDB_PATCH_LEVELLIB|sed 's/\./-/g'`
dnl else
dnl   LIB_RPGDB_PATCH_LEVELLIB=$RPGDB_PATCH_LEVELLIB
dnl fi
dnl AC_SUBST(LIB_RPGDB_PATCH_LEVELLIB)


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




