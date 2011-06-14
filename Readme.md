# hdfs-haskell

Haskell bindings to libhdfs (http://hadoop.apache.org/common/docs/current/libhdfs.html)

## Install

* Download a copy of the Hadoop source distribution, preferably version
  0.20.2+228 by cloudera:
  http://archive.cloudera.com/cdh/3/hadoop-0.20.2+228.tar.gz
* Extract the tarball and build Hadoop including native libraries:

    `ant compile -Dcompile.c++=true -Dlibhdfs=true`

  * On Mac OSX, you might need to set LDFLAGS:

      `LDFLAGS="-framework JavaVM" ant compile -Dcompile.c++=true -Dlibhdfs=true`

* set the environment variable HADOOP_HOME to the directory where you extracted
  the tarball, eg.:

    `export HADOOP_HOME=/tmp/hadoop-0.20.2+228`

* Build hdfs-haskell using cabal (replace PLATFORM with your respective
  architecture):

    ```
    cabal install --extra-include-dirs=$HADOOP_HOME/build/c++/PLATFORM/include \
                  --extra-lib-dirs=$HADOOP_HOME/build/c++/PLATFORM/lib
    ```

* With a running (single-node) Hadoop, you may run `test.sh`. Have a look at
  `test.hs` to see example code.

## Contributing

Send questions, bugs or patches to kim.altintop@gmail.com
Alternatively, fork and send pull requests through github.

## License

LGPL 3, see LICENSE

This library links against software developed by the Apache Software Foundation.
