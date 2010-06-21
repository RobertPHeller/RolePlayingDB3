#!/bin/sh
make -C Linux32 dist-binary-tarbz2
version=`grep '@VERSION@' Linux32/config.status | awk -F , '{print $3;}'`
mv Linux32/RolePlayingDB-$version-*.tar.bz2 ./RolePlayingDB-$version-Linux32BIN.tar.bz2
make -C Linux32 dist
mv Linux32/RolePlayingDB-$version.tar.gz ./
make -C Linux32 dist-zip
mv Linux32/RolePlayingDB-$version.zip ./
make -C Linux64 dist-binary-tarbz2
mv Linux64/RolePlayingDB-$version-*.tar.bz2 ./RolePlayingDB-$version-Linux64BIN.tar.bz2
make -C Win32 dist-binary-zip
mv Win32/RolePlayingDB-$version-*.zip ./RolePlayingDB-$version-Win32BIN.zip
make -C MacOSX dist-binary-zip
mv MacOSX/RolePlayingDB-$version-*.zip ./RolePlayingDB-$version-MacOSXUnivBIN.zip
rm -rf /extra/RolePlayingDB-$version
mkdir /extra/RolePlayingDB-$version
cp RolePlayingDB-$version-*BIN.tar.bz2 RolePlayingDB-$version-*BIN.zip \
	RolePlayingDB-$version.tar.gz RolePlayingDB-$version.zip README COPYING \
	INSTALL ChangeLog /extra/RolePlayingDB-$version/
tar cf - SampleData | tar xf - -C /extra/RolePlayingDB-$version/
mkisofs -abstract README -ldots -copyright COPYING -J -p 'Robert Heller' \
	-publisher 'Deepwoods Software' -r -V RolePlayingDB-$version \
	-o /extra/RolePlayingDB-$version.iso -gui -f \
	/extra/RolePlayingDB-$version
rm -rf /extra/RolePlayingDB-$version-UPLOADS
mkdir /extra/RolePlayingDB-$version-UPLOADS
pushd /extra/RolePlayingDB-$version-UPLOADS
ln -s ../RolePlayingDB-$version.iso ../RolePlayingDB-$version/*.zip \
	../RolePlayingDB-$version/*.tar.* ./
md5sum * >RolePlayingDB-$version.md5sums
popd
cat >/extra/RolePlayingDB-$version-UPLOADS/putem.sh <<EOF
#!/bin/bash -v
cd `dirname $0`
ls -FChLl *.{gz,iso,zip,bz2,rpm}
rsync -rLptgoDvz -P -e ssh --exclude=putem.sh ./ sharky.deepsoft.com:/var/ftp/pub/deepwoods/Products/RolePlayingDB/V3.0/
ssh sharky.deepsoft.com ./cd_md5sum /var/ftp/pub/deepwoods/Products/RolePlayingDB/V3.0/ -c RolePlayingDB-$version.md5sums
ssh sharky.deepsoft.com ls -Fltrh /var/ftp/pub/deepwoods/Products/RolePlayingDB/V3.0/ | tail
EOF
chmod +x /extra/RolePlayingDB-$version-UPLOADS/putem.sh
