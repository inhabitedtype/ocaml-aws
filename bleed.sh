#!/usr/bin/env bash

BLEED_DIR=~/code/opam-bleeding
VERSION=dev

cd libraries
for dir in */; do
    dir=${dir%*/}
    lib=aws_${dir}.${VERSION}
    echo "bleeding ${lib}..."
    LIB_DIR=$BLEED_DIR/packages/aws_$dir/$lib
    mkdir -p $LIB_DIR/files
    tar czf $BLEED_DIR/archives/${lib}+opam.tar.gz $dir
    cp $dir/opam $LIB_DIR/opam
    echo "archive: \"${lib}+opam.tar.gz\"" > $LIB_DIR/url
    echo "checksum: \"$(md5 -q $BLEED_DIR/archives/${lib}+opam.tar.gz)\"" >> $LIB_DIR/url
done;
