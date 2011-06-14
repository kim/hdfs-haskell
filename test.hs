import System.HDFS
import Data.ByteString.Char8 (pack)

import Prelude hiding (read)
import Debug.Trace

main =
  withHdfs "localhost" 9000 $ \hdfs -> do
    trace ("ls /tmp")
      ls hdfs "/tmp" >>= print

    trace ("creating test.txt" )
      withFile hdfs "/tmp/test.txt" Write $ \f ->
        write hdfs f (pack "hello world")

    trace ("stat test.txt")
      stat hdfs "/tmp/test.txt" >>= print

    trace ("cat test.text")
      withFile hdfs "/tmp/test.txt" Read $ \f ->
        read hdfs f 255 >>= print

    trace ("cp test.txt test2.text")
      cp hdfs "/tmp/test.txt" hdfs "/tmp/test2.txt"
    trace ("ls /tmp")
      ls hdfs "/tmp" >>= print
    trace ("rm test.txt")
      rm hdfs "/tmp/test.txt"
    trace ("rm test2.txt")
      rm hdfs "/tmp/test2.txt"
