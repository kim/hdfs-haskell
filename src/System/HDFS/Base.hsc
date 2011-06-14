{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.HDFS.Base where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <hdfs.h>

data HDFSFs = HDFSFs
type HDFSFsPtr = Ptr HDFSFs

data HDFSFile = HDFSFile
type HDFSFilePtr = Ptr HDFSFile

newtype HDFSIOMode = HDFSIOMode { ioMode :: CInt } deriving (Eq, Show)
#{enum HDFSIOMode, HDFSIOMode
 , readOnly  = O_RDONLY
 , writeOnly = O_WRONLY
 , append    = O_APPEND
 }

data HDFSFileInfo = HDFSFileInfo
  { mKind        :: CChar
  , mName        :: CString
  , mLastMod     :: HDFSTime
  , mSize        :: HDFSOffset
  , mReplication :: CShort
  , mBlocksize   :: HDFSBlocksize
  , mOwner       :: HDFSUser
  , mGroup       :: HDFSGroup
  , mPermissions :: CShort
  , mLastAccess  :: HDFSTime
  } deriving (Show)

instance Storable HDFSFileInfo where
  alignment _ = #{alignment hdfsFileInfo}
  sizeOf    _ = #{size hdfsFileInfo}
  peek p = do
    k <- #{peek hdfsFileInfo, mKind} p
    n <- #{peek hdfsFileInfo, mName} p
    l <- #{peek hdfsFileInfo, mLastMod} p
    s <- #{peek hdfsFileInfo, mSize} p
    r <- #{peek hdfsFileInfo, mReplication} p
    b <- #{peek hdfsFileInfo, mBlockSize} p
    o <- #{peek hdfsFileInfo, mOwner} p
    g <- #{peek hdfsFileInfo, mGroup} p
    m <- #{peek hdfsFileInfo, mPermissions} p
    a <- #{peek hdfsFileInfo, mLastAccess} p
    return $ HDFSFileInfo k n l s r b o g m a
  poke p (HDFSFileInfo k n l s r b o g m a) = do
    #{poke hdfsFileInfo, mKind} p k
    #{poke hdfsFileInfo, mName} p n
    #{poke hdfsFileInfo, mLastMod} p l
    #{poke hdfsFileInfo, mSize} p s
    #{poke hdfsFileInfo, mReplication} p r
    #{poke hdfsFileInfo, mBlockSize} p b
    #{poke hdfsFileInfo, mOwner} p o
    #{poke hdfsFileInfo, mGroup} p g
    #{poke hdfsFileInfo, mPermissions} p m
    #{poke hdfsFileInfo, mLastAccess} p a

type HDFSFileInfoPtr = Ptr HDFSFileInfo

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

type HDFSHost = CString
type HDFSPort = CUShort
type HDFSUser = CString
type HDFSGroup = CString
type HDFSPath = CString
type HDFSBlocksize = CInt
type HDFSOffset = CLong
type HDFSReadBuffer = Ptr (CChar)
type HDFSWriteBuffer = Ptr (CChar)
type HDFSTime = CLong -- time_t

foreign import ccall unsafe "hdfs.h hdfsConnect"
  c_hdfs_connect :: HDFSHost -> HDFSPort -> IO HDFSFsPtr

foreign import ccall unsafe "hdfs.h hdfsDisconnect"
  c_hdfs_disconnect :: HDFSFsPtr -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsOpenFile"
  c_hdfs_open_file :: HDFSFsPtr
                   -> HDFSPath
                   -> HDFSIOMode
                   -> CInt           -- ^ buffer size
                   -> CShort         -- ^ replication
                   -> HDFSBlocksize
                   -> IO HDFSFilePtr

foreign import ccall unsafe "hdfs.h hdfsCloseFile"
  c_hdfs_close_file :: HDFSFsPtr -> HDFSFilePtr -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsExists"
  c_hdfs_exists :: HDFSFsPtr -> CString -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsSeek"
  c_hdfs_seek :: HDFSFsPtr -> HDFSFilePtr -> HDFSOffset -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsTell"
  c_hdfs_tell :: HDFSFsPtr -> HDFSFilePtr -> IO HDFSOffset

foreign import ccall unsafe "hdfs.h hdfsRead"
  c_hdfs_read :: HDFSFsPtr -> HDFSFilePtr -> HDFSReadBuffer -> CInt -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsPread"
  c_hdfs_pread :: HDFSFsPtr -> HDFSFilePtr -> HDFSOffset -> HDFSReadBuffer -> CInt -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsWrite"
  c_hdfs_write :: HDFSFsPtr -> HDFSFilePtr -> HDFSWriteBuffer -> CInt -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsFlush"
  c_hdfs_flush :: HDFSFsPtr -> HDFSFilePtr -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsAvailable"
  c_hdfs_available :: HDFSFsPtr -> HDFSFilePtr -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsCopy"
  c_hdfs_copy :: HDFSFsPtr -> HDFSPath -> HDFSFsPtr -> HDFSPath -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsMove"
  c_hdfs_move :: HDFSFsPtr -> HDFSPath -> HDFSFsPtr -> HDFSPath -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsDelete"
  c_hdfs_delete :: HDFSFsPtr -> HDFSPath -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsRename"
  c_hdfs_rename :: HDFSFsPtr -> HDFSPath -> HDFSPath -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsGetWorkingDirectory"
  c_hdfs_get_working_directory :: HDFSFsPtr -> HDFSReadBuffer -> CInt -> IO HDFSReadBuffer

foreign import ccall unsafe "hdfs.h hdfsSetWorkingDirectory"
  c_hdfs_set_working_directory :: HDFSFsPtr -> HDFSPath -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsCreateDirectory"
  c_hdfs_create_directory :: HDFSFsPtr -> HDFSPath -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsListDirectory"
  c_hdfs_list_directory :: HDFSFsPtr -> HDFSPath -> Ptr CInt -> IO HDFSFileInfoPtr -- returns an array

foreign import ccall unsafe "hdfs.h hdfsGetPathInfo"
  c_hdfs_get_path_info :: HDFSFsPtr -> HDFSPath -> IO HDFSFileInfoPtr

foreign import ccall unsafe "hdfs.h hdfsFreeFileInfo"
  c_hdfs_free_file_info :: HDFSFileInfoPtr -> CInt -> IO ()

foreign import ccall unsafe "hdfs.h hdfsChown"
  c_hdfs_chown :: HDFSFsPtr -> HDFSPath -> HDFSUser -> HDFSGroup -> IO CInt

foreign import ccall unsafe "hdfs.h hdfsChmod"
  c_hdfs_chmod :: HDFSFsPtr -> HDFSPath -> CShort -> IO CInt

-- not (yet) provided:
--
-- hdfsConnectAsUser
--
-- hdfsUtime
-- hdfsGetHosts
-- hdfsFreeHosts
-- hdfsUsed
--
-- foreign import ccall unsafe "hdfs.h hdfsGetDefaultBlockSize"
--   c_hdfs_get_default_blocksize :: HDFSFsPtr -> IO HDFSOffset
--
-- foreign import ccall unsafe "hdfs.h hdfsSetReplication"
--   c_hdfs_set_replication :: HDFSFsPtr -> HDFSPath -> CShort -> IO CInt
--
-- foreign import ccall unsafe "hdfs.h hdfsGetCapacity"
--   c_hdfs_get_capacity :: HDFSFsPtr -> IO HDFSOffset
