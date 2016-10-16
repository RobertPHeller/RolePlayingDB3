SUBDIRS = BuildScripts BuildData Doc Scripts SampleData

EXTRA_DIST = RolePlayingDB3.spec RolePlayingDB3.spec.in bootstrap Doxyfile.in \
	Doxyfile RolePlayingDB3.desktop RolePlayingDB3.png

TCLKIT = $(top_builddir)/BuildData/tclkit${BEXEEXT}

BUILT_SOURCES = $(TCLKIT)

bin_SCRIPTS = RolePlayingDB3$(EXEEXT)

desktopdir = $(datadir)/applications
desktop_DATA = RolePlayingDB3.desktop

pixmapdir = $(datadir)/pixmaps
pixmap_DATA = RolePlayingDB3.png

man_MANS = man1/RolePlayingDB3.1

AllScripts = $(wildcard $(top_srcdir)/Scripts/*.tcl) $(wildcard Scripts/*.tcl)
LibScripts = $(filter-out %RolePlayingDB3.tcl,$(AllScripts))
AllImages  = $(wildcard $(top_srcdir)/Scripts/*.gif) \
		$(wildcard $(top_srcdir)/Scripts/*.png)
AllHelpDep = Doc/User/UserManual/UserManual.html
AllHelpFiles = $(wildcard Doc/User/UserManual/*)

RolePlayingDB3$(EXEEXT): $(AllScripts) $(AllImages) $(AllHelpDep)
	-rm -rf RolePlayingDB3.vfs
	$(TCLKIT) $(top_srcdir)/BuildScripts/sdx.kit qwrap \
					$(srcdir)/Scripts/RolePlayingDB3.tcl
	$(TCLKIT) $(top_srcdir)/BuildScripts/sdx.kit unwrap RolePlayingDB3.kit
	-rm -rf RolePlayingDB3.kit
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitDir.kit RolePlayingDB3 \
		lib $(BWLIB)
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitDir.kit RolePlayingDB3 \
		lib $(SNITLIB)
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitDir.kit RolePlayingDB3 \
		lib $(URILIB)
	$(UNZIP) -qq -d RolePlayingDB3.vfs/lib \
		$(top_srcdir)/BuildData/$(IMG_ZIP)
	$(UNZIP) -qq -d RolePlayingDB3.vfs/lib \
		$(top_srcdir)/BuildData/$(TCLXML_ZIP)
	$(UNZIP) -qq -d RolePlayingDB3.vfs/lib \
		$(top_srcdir)/BuildData/pdf4tcl05.zip
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitFile.kit -srcdir $(srcdir) \
		RolePlayingDB3 lib/Common $(LibScripts)		
	$(TCLKIT) $(top_srcdir)/BuildScripts/MakePkgIndex.kit RolePlayingDB3 \
		Common 
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitFile.kit -srcdir $(srcdir) \
		RolePlayingDB3 Images $(AllImages)
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitFile.kit -srcdir $(srcdir) \
		RolePlayingDB3 Help $(AllHelpFiles)
	$(TCLKIT) $(top_srcdir)/BuildScripts/sdx.kit wrap \
		RolePlayingDB3$(EXEEXT) \
		-runtime $(top_srcdir)/BuildData/$(TCLKITRUN)
	-rm -rf RolePlayingDB3.vfs RolePlayingDB3.kit
if MACOSX
# create application for Mac OSX
	-rm -rf RolePlayingDB3.app
	mkdir -p RolePlayingDB3.app/Contents/MacOS/
	cp RolePlayingDB3$(EXEEXT) RolePlayingDB3.app/Contents/MacOS/
	-rm -rf RolePlayingDB3.iconset
	mkdir RolePlayingDB3.iconset
	sips -z 16 16     $(srcdir)/RolePlayingDB3.png --out RolePlayingDB3.iconset/icon_16x16.png
	sips -z 32 32     $(srcdir)/RolePlayingDB3.png --out RolePlayingDB3.iconset/icon_16x16@2x.png
	sips -z 32 32     $(srcdir)/RolePlayingDB3.png --out RolePlayingDB3.iconset/icon_32x32.png
	sips -z 64 64     $(srcdir)/RolePlayingDB3.png --out RolePlayingDB3.iconset/icon_32x32@2x.png
	sips -z 128 128   $(srcdir)/RolePlayingDB3.png --out RolePlayingDB3.iconset/icon_128x128.png
	sips -z 256 256   $(srcdir)/RolePlayingDB3.png --out RolePlayingDB3.iconset/icon_128x128@2x.png
	sips -z 256 256   $(srcdir)/RolePlayingDB3.png --out RolePlayingDB3.iconset/icon_256x256.png
	sips -z 512 512   $(srcdir)/RolePlayingDB3.png --out RolePlayingDB3.iconset/icon_256x256@2x.png
	sips -z 512 512   $(srcdir)/RolePlayingDB3.png --out RolePlayingDB3.iconset/icon_512x512.png
	sips -z 1024 1024 $(srcdir)/RolePlayingDB3.png --out RolePlayingDB3.iconset/icon_512x512@2x.png
	iconutil -c icns RolePlayingDB3.iconset
	-rm -rf RolePlayingDB3.iconset
	mkdir -p RolePlayingDB3.app/Contents/Resources/
	cp RolePlayingDB3.icns RolePlayingDB3.app/Contents/Resources/
	$(TCLKIT) $(top_srcdir)/BuildScripts/MacOSXPlist.kit RolePlayingDB3 1.0 "1994,1995,2002-2005  Robert Heller D/B/A Deepwoods Software"
	plutil -convert binary1 -o RolePlayingDB3.app/Contents/Info.plist \
		RolePlayingDB3.plist
	-rm -f RolePlayingDB3.plist RolePlayingDB3.icns

install-exec-local: RolePlayingDB3.app
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(top_srcdir)/BuildScripts/macappInstall.sh RolePlayingDB3.app $(DESTDIR)$(bindir)

RolePlayingDB3.app: RolePlayingDB3${BEXEEXT}

endif
	
$(top_builddir)/BuildData/tclkit${BEXEEXT}: FORCE
	$(MAKE) -C $(top_builddir)/BuildData tclkit${BEXEEXT}

$(top_builddir)/Scripts/Version.tcl: $(top_builddir)/config.status
	$(MAKE) -C $(top_builddir)/Scripts Version.tcl


FORCE:

Doc/User/UserManual/UserManual.html: FORCE
	$(MAKE) -C $(top_builddir)/Doc/User UserManual/UserManual.html

dist-binary-tarbz2: all
	$(MAKE) $(AM_MAKEFLAGS) DESTDIR=/tmp/$(PACKAGE)-$(VERSION)-$(target) install
	tar cjf $(PACKAGE)-$(VERSION)-$(target).tar.bz2 -C /tmp/$(PACKAGE)-$(VERSION)-$(target)${prefix} .
	rm -rf /tmp/$(PACKAGE)-$(VERSION)-$(target)

dist-binary-zip: all
	$(MAKE) $(AM_MAKEFLAGS) DESTDIR=/tmp/$(PACKAGE)-$(VERSION)-$(target) install
	rm -f $(PACKAGE)-$(VERSION)-$(target).zip
	(cd  /tmp/$(PACKAGE)-$(VERSION)-$(target)${prefix}; zip -rq $(BUILD_DIR)/$(PACKAGE)-$(VERSION)-$(target).zip .)

	
man1/RolePlayingDB3.1 : Doxyfile Scripts/RolePlayingDB3.tcl
	-rm -rf man1
	$(DOXYGEN)

clean-local:
	-rm -rf man1


