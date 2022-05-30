AC_INIT(RolePlayingDB,3.1.3,[support@deepsoft.com], RolePlayingDB)

AC_CONFIG_MACRO_DIR([m4])

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


RPGDB_VERSION=3.1
AC_DEFINE_UNQUOTED([RPGDB_VERSION],$RPGDB_VERSION,[RPGDB_VERSION])
RPGDB_MAJOR_VERSION=3
AC_DEFINE_UNQUOTED([RPGDB_MAJOR_VERSION],$RPGDB_MAJOR_VERSION,[RPGDB Major])
RPGDB_MINOR_VERSION=1
AC_DEFINE_UNQUOTED([RPGDB_MINOR_VERSION],$RPGDB_MINOR_VERSION,[RPGDB Minor])
RPGDB_PATCH_LEVEL="3.1.3"
AC_DEFINE_UNQUOTED([RPGDB_PATCH_LEVEL],$RPGDB_PATCH_LEVEL,[RPGDB Patch Level])
RPGDB_PATCH_LEVELLIB=3.1.3

AM_INIT_AUTOMAKE( [-Wno-portability tar-ustar] )



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

AC_PATH_PROG(DOXYGEN,doxygen,/bin/true)
AC_PATH_PROG(PDFLATEX,pdflatex,/bin/true)
AC_PATH_PROG(LATEX,latex,/bin/true)
AC_PATH_PROG(MAKEINDEX,makeindex,/bin/true)
AC_PATH_PROG(BIBTEX,bibtex,/bin/true)
AC_PATH_PROG(DVIPS,dvips,/bin/true)
AC_PATH_PROG(PS2PDF,ps2pdf,/bin/true)

AC_PATH_PROGS(HOSTTCLSH,[tclsh8.6 tclsh])


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

dnl AC_MSG_CHECKING(pdf4tcl dir)
dnl PDF4TCLLIB=`echo 'puts $tcl_library'|${HOSTTCLSH}`
dnl dirs="${TCL_PREFIX}/lib/pdf4tcl* ${PDF4TCLLIB}/pdf4tcl*"
dnl for i in $dirs ; do
dnl   if test -d "$i" -a -f "$i/pkgIndex.tcl"; then
dnl     PDF4TCLLIB=`cd $i; pwd`
dnl   fi
dnl done
dnl AC_ARG_WITH(pdf4tcllib, [  --with-pdf4tcllib=DIR          use pdf4tcl from DIR], PDF4TCLLIB=$withval,)
dnl if test -d $PDF4TCLLIB; then
dnl    AC_MSG_RESULT([using pdf4tcl library in $PDF4TCLLIB])
dnl else
dnl    AC_MSG_ERROR(Pdf4tcl library directory $PDF4TCLLIB doesn't exist)
dnl fi
dnl AC_SUBST(PDF4TCLLIB)

AC_PATH_PROGS(UNZIP,unzip)

AC_MSG_CHECKING([Checking target tclkit])
case $target_os in 
  macos | darwin*) 
    AC_SUBST(TCLKITRUN, tclkit-8.6.11-Darwin-Img1.4.13)
    AC_SUBST(NEED_APP_BUNDLE,[true])
    ;;
  linux-gnu*)
    case $target_cpu in
      x86_64)
	AC_SUBST(TCLKITRUN, tclkit-8.6.11-x86_64-Img1.4.7)
	;;
      i*86)
	AC_SUBST(TCLKITRUN, tclkit-8.6.11-i386-Img1.4.7)
	;;
      arm*)
        AC_SUBST(TCLKITRUN, tclkit-8.6.11-armv7l-Img1.4.8)
        ;;
      aarch64)
        AC_SUBST(TCLKITRUN, tclkit-8.6.10-aarch64-Img1.4.9)
        ;;
      *)
	AC_MSG_ERROR([Unsupported Linux arch: $target_cpu])
	;;
     esac
     AC_SUBST(NEED_APP_BUNDLE,[false])
     ;;

   mingw* | winnt | windows* | cygwin)
     AC_SUBST(TCLKITRUN, tclkit-8.6.11-WIN64-Img1.4.13.exe)
     AC_MSG_NOTICE([Only supporting 64-bit!])
     AC_SUBST(NEED_APP_BUNDLE,[false])
     AC_SUBST(EXEEXT, .exe)
     ;;
   *) 
     AC_MSG_ERROR([Unsupported Operating System: $target_os])
     ;;
esac

AC_MSG_RESULT(TCLKITRUN is [$TCLKITRUN])

AM_CONDITIONAL(MACOSX, test x${NEED_APP_BUNDLE} = xtrue)
AC_PATH_PROG(CONVERT,convert,/bin/true)


AC_MSG_CHECKING([Checking build tclkit])
case $build_os in 
  macos | darwin*) 
    AC_SUBST(TCLKITBSRC, tclkit-8.6.11-Darwin-Img1.4.13)
    AC_SUBST(BEXEEXT, )
    ;;
  linux-gnu)
    AC_SUBST(BEXEEXT, )
    case $build_cpu in
      x86_64)
	AC_SUBST(TCLKITBSRC, tclkit-8.6.11-x86_64-Img1.4.7)
	;;
      i*86)
	AC_SUBST(TCLKITBSRC, tclkit-8.6.11-i386-Img1.4.7)
	;;
      arm*)
        AC_SUBST(TCLKITBSRC, tclkit-8.6.11-armv7l-Img1.4.8)
        ;;
      aarch64)
        AC_SUBST(TCLKITBSRC, tclkit-8.6.10-aarch64-Img1.4.9)
        ;;
      *)
	AC_MSG_ERROR([Unsupported Linux arch: $build_cpu])
	;;
     esac
     ;;

   mingw* | winnt | windows* | cygwin)
     AC_SUBST(BEXEEXT, .exe)
     AC_SUBST(TCLKITBSRC, tclkit-8.6.11-WIN64-Img1.4.13.exe)
     AC_MSG_NOTICE([Only supporting 64-bit])
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
		Doc/User/Makefile \
		Doc/User/Doxyfile \
		Doc/Support/Makefile \
		Doc/API/Makefile \
		Doc/API/Doxyfile \
	  SampleData/Makefile \
	  Doxyfile \
	  RolePlayingDB3.spec
	  )




