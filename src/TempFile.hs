module TempFile where

import System.Directory
import System.FilePath
import System.IO.Temp

-- getKeystorePath' :: IO FilePath
-- getKeystorePath' = do
--     tempDir <- getTemporaryDirectory
--     let dir = tempDir </> "ariadne-test"
--     createDirectoryIfMissing False dir
--     (fp, hdl) <- openTempFile dir "keystore.key"
--     hClose hdl
--     removeFile fp
--     pure fp

getKeystorePath :: IO Bool
getKeystorePath = do
    tempDir  <- getTemporaryDirectory
    tempFile <- emptyTempFile tempDir "keystore.keys"
    print tempFile
    -- pure True
    doesFileExist tempFile -- returns false, but should be true (bug)
    -- doesDirectoryExist path -- returns true

-- getDataDir :: IO FilePath
-- getDataDir = do
--     dir <- getXdgDirectory XdgData "ariadne"
--     createDirectoryIfMissing True dir
--     return dir

-- getStoragePath :: IO Bool
-- getStoragePath = do
--     path <- (</> "wallet.json") <$> getDataDir
--     doesDirectoryExist path
--     -- void $ readFile path
