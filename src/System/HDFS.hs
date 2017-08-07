module System.HDFS
  ( FileSystem
  , File
  , IOMode(..)
  , FileInfo(..)

  , withHdfs
  , connect
  , disconnect

  , withFile
  , open
  , close
  , write
  , writeL
  , flush
  , read
  , available
  , seek
  , tell
  , pread

  , exists
  , cp
  , mv
  , rm
  , mv'
  , ls
  , cwd
  , cd
  , mkdir
  , chown
  , chmod
  , stat

  -- * HDFS specific
  -- , getHosts
  -- , setReplication
  -- , defaultBlocksize
  -- , getCapacity
  -- , getUsed
  -- , utime
  ) where

import Prelude hiding (read)

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Int
import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types

import System.HDFS.Base

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Unsafe as UB


-- | HDFS File System Handle
newtype FileSystem = FileSystem { _hdfs :: HDFSFsPtr }

-- | HDFS File Handle
newtype File = File { _hfdsFile :: HDFSFilePtr }

-- | Modes for opening HDFS files
data IOMode = Read | Write | Append

-- | HDFS File Metadata
data FileInfo = FileInfo
  { mKind        :: Char
  , mName        :: String
  , mLastMod     :: Int64
  , mSize        :: Int64
  , mReplication :: Int16
  , mBlocksize   :: Int64
  , mOwner       :: String
  , mGroup       :: String
  , mPermissions :: Int16
  , mLastAccess  :: Int64
  } deriving (Show)

type Host = String
type Port = Int
type Path = String

withHdfs :: Host -> Port -> (FileSystem -> IO a) -> IO a
withHdfs h p = bracket (connect h p) disconnect

connect :: Host -> Port -> IO FileSystem
connect h p = withCString h $ \host -> do
  fs <- throwErrnoIfNull "connect" $ c_hdfs_connect host (fromIntegral p)
  return $ FileSystem fs

disconnect :: FileSystem -> IO ()
disconnect (FileSystem fs) =
  throwErrnoIfMinus1_ "disconnect" (c_hdfs_disconnect fs)

withFile :: FileSystem -> Path -> IOMode -> (File -> IO a) -> IO a
withFile fs p m = bracket (open fs p m) (close fs)

open :: FileSystem -> Path -> IOMode -> IO File
open (FileSystem fs) p m =
  withCString p $ \path -> do
  f <- throwErrnoIfNull "open" $
         c_hdfs_open_file fs path iomode 0 0 0 -- note: using defaults
  return . File $ f
  where
    iomode = toHDFSIOMode m

close :: FileSystem -> File -> IO ()
close (FileSystem fs) (File f) =
  throwErrnoIfMinus1_ "close" (c_hdfs_close_file fs f) >> return ()

exists :: FileSystem -> Path -> IO Bool
exists (FileSystem fs) p = do
  t <- withCString p $ \path -> c_hdfs_exists fs path
  return $ (0 :: CInt) == t

write :: FileSystem -> File -> SB.ByteString -> IO Int
write (FileSystem fs) (File f) b =
  UB.unsafeUseAsCStringLen b $ \(cstr, len) -> do
    written <- throwErrnoIfMinus1 "write" $
                 c_hdfs_write fs f cstr (fromIntegral len)
    return . fromIntegral $ written

writeL :: FileSystem -> File -> LB.ByteString -> IO Int
writeL (FileSystem fs) (File f) lbs = do
  let fn offset bs = UB.unsafeUseAsCStringLen bs $ \(cstr, len) -> do
      written <- throwErrnoIfMinus1 "writeL" $
                   c_hdfs_write fs f cstr (fromIntegral len)
      return $ offset + fromIntegral written
  foldM fn 0 (LB.toChunks lbs)

read :: FileSystem -> File -> Int -> IO SB.ByteString
read (FileSystem fs) (File f) len =
  allocaArray len $ \buf -> do
    nread <- throwErrnoIfMinus1 "read" $
               c_hdfs_read fs f buf (fromIntegral len)
    SB.packCStringLen (buf, fromIntegral nread)

pread :: FileSystem -> File -> Int64 -> Int -> IO SB.ByteString
pread (FileSystem fs) (File f) offset len =
  allocaArray len $ \buf -> do
    nread <- throwErrnoIfMinus1 "pread" $
               c_hdfs_pread fs f (fromIntegral offset) buf (fromIntegral len)
    SB.packCStringLen (buf, fromIntegral nread)

flush :: FileSystem -> File -> IO ()
flush (FileSystem fs) (File f) =
  throwErrnoIfMinus1_ "flush" (c_hdfs_flush fs f) >> return ()

available :: FileSystem -> File -> IO Int
available (FileSystem fs) (File f) =
  fmap fromIntegral $ throwErrnoIfMinus1 "available" $ c_hdfs_available fs f

seek :: FileSystem -> File -> Int64 -> IO ()
seek (FileSystem fs) (File f) offset =
  throwErrnoIfMinus1_ "seek" $ c_hdfs_seek fs f (fromIntegral offset)

tell :: FileSystem -> File -> IO Int64
tell (FileSystem fs) (File f) =
  fmap fromIntegral $ throwErrnoIfMinus1 "tell" $ c_hdfs_tell fs f

cp :: FileSystem -> Path -> FileSystem -> Path -> IO ()
cp (FileSystem fs) p (FileSystem fs') p' =
  withCString p $ \src -> withCString p' $ \dst ->
   throwErrnoIfMinus1_ "cp" $ c_hdfs_copy fs src fs' dst

rm :: FileSystem -> Path -> IO ()
rm (FileSystem fs) p =
  withCString p $ \path ->
    throwErrnoIfMinus1_ "delete" $ c_hdfs_delete fs path

mv :: FileSystem -> Path -> Path -> IO ()
mv (FileSystem fs) p p' =
  withCString p $ \path -> withCString p' $ \path' ->
    throwErrnoIfMinus1_ "rename" $ c_hdfs_rename fs path path'

-- | Move across FileSystems
mv' :: FileSystem -> Path -> FileSystem -> Path -> IO ()
mv' (FileSystem fs) p (FileSystem fs') p' =
  withCString p $ \src -> withCString p' $ \dst ->
    throwErrnoIfMinus1_ "mv" $ c_hdfs_move fs src fs' dst

ls :: FileSystem -> Path -> IO [FileInfo]
ls (FileSystem fs) p =
  withCString p $ \path   ->
  alloca        $ \numptr -> do
    cinfo <- c_hdfs_list_directory fs path numptr
    num   <- peek numptr
    info  <- peekArray (fromIntegral num) cinfo >>= mapM mkFileInfo
    c_hdfs_free_file_info cinfo num
    errNo <- getErrno
    if errNo == eOK
      then return info
      else throwErrno "ls"

cwd :: FileSystem -> IO String
cwd (FileSystem fs) =
  allocaArray 255 $ \buf -> do
    wd <- throwErrnoIfNull "cwd" $
            c_hdfs_get_working_directory fs buf 255
    -- this is kinda silly...
    SBC.unpack <$> SB.packCStringLen (wd, 255)

cd :: FileSystem -> Path -> IO ()
cd (FileSystem fs) p =
  withCString p $ \path ->
    throwErrnoIfMinus1_ "cd" $ c_hdfs_set_working_directory fs path

mkdir :: FileSystem -> Path -> IO ()
mkdir (FileSystem fs) p =
  withCString p $ \path ->
    throwErrnoIfMinus1_ "mkdir" $ c_hdfs_create_directory fs path

chown :: FileSystem -> Path -> String -> String -> IO ()
chown (FileSystem fs) p u g =
  withCString p $ \path  ->
  withCString u $ \user  ->
  withCString g $ \group ->
    throwErrnoIfMinus1_ "chown" $ c_hdfs_chown fs path user group

chmod :: FileSystem -> Path -> Int16 -> IO ()
chmod (FileSystem fs) p m =
  withCString p $ \path ->
    throwErrnoIfMinus1_ "chmod" $ c_hdfs_chmod fs path (fromIntegral m)

stat :: FileSystem -> Path -> IO FileInfo
stat (FileSystem fs) p =
  withCString p $ \path -> do
    cinfo <- throwErrnoIfNull "stat" (c_hdfs_get_path_info fs path)
    info  <- peek cinfo >>= mkFileInfo
    c_hdfs_free_file_info cinfo 1
    return info

-- internal

mkFileInfo :: HDFSFileInfo -> IO FileInfo
mkFileInfo (HDFSFileInfo k n l s r b o g m a) = do
  name <- peekCString n
  ownr <- peekCString o
  grp  <- peekCString g
  let kind = castCCharToChar k
      lmod = fromIntegral l
      size = fromIntegral s
      repl = fromIntegral r
      blck = fromIntegral b
      perm = fromIntegral m
      lacc = fromIntegral a
  return (FileInfo kind name lmod size repl blck ownr grp perm lacc)

toHDFSIOMode :: IOMode -> HDFSIOMode
toHDFSIOMode Read   = readOnly  -- ^ O_RDONLY
toHDFSIOMode Write  = writeOnly -- ^ O_WRONLY
toHDFSIOMode Append = append    -- ^ O_APPEND
