#!/bin/sh

set -x

HANCOCK_DIR=`pwd`
INSTALL_DIR=$HANCOCK_DIR/install

if [ "$#" = "0" ] ; then
. $INSTALL_DIR/install_form
else
. $1
fi
  

echo Adding a link for sml.
if [ ! -f sml ]
then
 ln -s $SMLDIR/bin/sml sml
fi

echo Adding bin directory.
if [ ! -d bin ]
then
 mkdir bin
fi

echo Adding lib directory.
if [ ! -d lib ]
then
 mkdir lib
fi

echo Adding include directory.
    if [ ! -d include ]
then
 mkdir include
fi

echo Adding src/build directory.
    if [ ! -d src/build ]
then
 mkdir src/build
fi



echo Generating architecture and op sys info
. $INSTALL_DIR/_arch-n-opsys

echo Adding a link for sml runtime.
if [ ! -f sml.runtime ]
then
 ln -s $SMLDIR/bin/.run/run.$ARCH-$OPSYS lib/sml.runtime
fi

echo Generating hcc script
sed 's,^HANCOCKDIR=.*$,HANCOCKDIR='$HANCOCK_DIR',g; s,^SMLBIN=.*$,SMLBIN='$SMLDIR'/bin/.run/run.'"$ARCH"'-'"$OPSYS"',g' $INSTALL_DIR/hcc.template > bin/hcc
chmod a+x bin/hcc

echo Generating internal_cc script
echo "$CC $CPPFLAGS $CXFLAGS" \$* > bin/internal_cc
chmod a+x bin/internal_cc
 
echo Compiling compiler and runtime system
cd src
sed 's,^CC=.*$,CC='"$CC"',g; s,^CXFLAGS=.*$,CXFLAGS='"$CXFLAGS"',g; s,^AR=.*$,AR='"$AR"',g' Makefile.in > Makefile
$MAKE -f Makefile
if [ "$?" != 0 ] ; then
  echo Compile failed.  Aborting.
  exit 1
fi
cd ..

echo Compiling Hancock Library
cd hl
sed 's,^CC=.*$,CC='"$CC"',g; s,^CXFLAGS=.*$,CXFLAGS='"$CXFLAGS"',g; s,^AR=.*$,AR='"$AR"',g' Makefile.in > Makefile
$MAKE -f Makefile
if [ "$?" != 0 ] ; then
  echo Library compile failed.  Aborting.
  exit 1
fi
cd ..

echo Making example calls
cd examples
rm Makefile
sed 's,^CC=.*$,CC='"$CC"',g; s,^CXFLAGS=.*$,CXFLAGS='"$CXFLAGS"',g; s,^AR=.*$,AR='"$AR"',g' Makefile.in > Makefile
$MAKE calls10
cd ..

echo Running test suite
cd tests/suite
$MAKE install_check
cd ../..

echo done





