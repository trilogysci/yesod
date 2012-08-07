{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP                 #-}
module Devel
    ( devel
    ) where


import qualified Distribution.Simple.Utils as D
import qualified Distribution.Verbosity as D
import qualified Distribution.PackageDescription.Parse as D
import qualified Distribution.PackageDescription as D
import qualified Distribution.ModuleName as D

import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as Ex
import           Control.Monad (forever, when, unless)

import           Data.Char (isUpper, isNumber)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set

import           System.Directory
import           System.Exit (exitFailure, exitSuccess, ExitCode (..))
import           System.FilePath (splitDirectories, dropExtension, takeExtension)
import           System.Posix.Types (EpochTime)
import           System.PosixCompat.Files (modificationTime, getFileStatus)
import           System.Process (createProcess, proc, terminateProcess, readProcess,
                                           waitForProcess, rawSystem, runInteractiveProcess,
                                           ProcessHandle)
import           System.IO (hClose, hIsEOF, hGetLine, stdout, stderr, hPutStrLn, hFlush)
import           System.INotify
import           Control.Concurrent.MVar
import           System.CPUTime (getCPUTime)

import Build (recompDeps, getDeps)

lockFile :: FilePath
lockFile = "dist/devel-terminate"

writeLock :: IO ()
writeLock = do
    createDirectoryIfMissing True "dist"
    writeFile lockFile ""

removeLock :: IO ()
removeLock = try_ (removeFile lockFile)


terminate :: ProcessHandle -> IO ()
terminate ph = do
    putStrLn "Stopping development server..."
    -- writeLock
    threadDelay 1000000
    putStrLn "Terminating development server..."
    terminateProcess ph
    return ()
notifyhandler :: INotify -> MVar (ProcessHandle,[WatchDescriptor])  -> String -> [String] -> [FilePath] -> Event -> IO ()
notifyhandler inotify mvar cmd devArgs hsSourceDirs event = do
    forkIO . try_ $ do
        -- putStrLn $ "starting notifyhandler "++show event
        pvar <- tryTakeMVar mvar
        case pvar of
            Nothing -> do
                -- putStrLn "starting notifyhandler with Nothing"
                return ()
            Just (oph,oldwatchlist) -> do
                    -- putStrLn "starting notifyhandler with ph,watchlist"
                    terminate oph
                    rmWatchList oldwatchlist
                    watchlist <- mkWatchList inotify mvar cmd devArgs hsSourceDirs


                    putStrLn "Rebuilding application..."

                    recompDeps hsSourceDirs

                    -- list <- getFileList hsSourceDirs
                    exit <- rawSystemFilter cmd ["build"]
                    -- exit <- return ExitSuccess

                    case exit of
                     ExitFailure _ -> putStrLn "Build failure, pausing..."
                     _ -> do
                           -- removeLock
                           putStrLn $ "Starting development server: runghc " ++ L.unwords devArgs
                           (_,_,_,ph) <- createProcess $ proc "runghc" devArgs
                           putMVar mvar (ph,watchlist)
                           -- putStrLn "waiting for process ph"
                           -- ec <- waitForProcess ph
                           -- putStrLn $ "Exit code: " ++ show ec
                           -- Ex.throwTo watchTid (userError "process finished")

    return ()

mkWatchList :: INotify -> MVar (ProcessHandle,[WatchDescriptor]) -> String -> [String] -> [FilePath] -> IO [WatchDescriptor]
mkWatchList inotify mvar cmd devArgs hsSourceDirs = do
    (files, deps) <- getDeps hsSourceDirs
    let files' = files ++ map fst (Map.toList deps)
    list <- mapM (\p -> addWatch inotify [Modify] p (notifyhandler inotify mvar cmd devArgs hsSourceDirs)) files'
    -- list <- mapM (\p -> addWatch inotify [Modify] p (\ev -> putStrLn "Notify Event")) files'
    putStrLn "Created new watchlist "
    -- putStrLn $ "Created new watchlist " ++ show files'
    return list

rmWatchList :: [WatchDescriptor] -> IO ()
rmWatchList watchlist = do
    putStrLn "Clearing watchlist"
    mapM (removeWatch) watchlist
    return ()

devel :: Bool -> [String] -> IO ()
devel isCabalDev passThroughArgs = do
    checkDevelFile
    writeLock
    inotify <- initINotify
    mvar <- newEmptyMVar

    ghcVer <- ghcVersion
    pkgArgs <- ghcPackageArgs isCabalDev ghcVer
    devArgs <- return $ pkgArgs ++ ["devel.hs"] ++ passThroughArgs

    cabal <- D.findPackageDesc "."
    gpd   <- D.readPackageDescription D.normal cabal

    hsSourceDirs <- checkCabalFile gpd

    watchlist <- mkWatchList inotify mvar cmd devArgs hsSourceDirs

    putStrLn "Yesod devel server. Press ENTER to quit"
    _ <- forkIO $ do

      _<- rawSystem cmd args

      putStrLn "Create dummy process"
      (_,_,_,ph) <- createProcess $ proc "sleep" ["500"]
      putMVar mvar (ph,watchlist)

      notifyhandler inotify mvar cmd devArgs hsSourceDirs Ignored
      putStrLn "Exited main notifyhandler"
      -- watchlist <- mkWatchList inotify mvar mph cmd devArgs hsSourceDirs
      -- putMVar mvar watchlist

      -- mainLoop hsSourceDirs INotify

    putStrLn "wait for enter"
    _ <- getLine
    pvar <- tryTakeMVar mvar
    case pvar of
        Nothing -> return ()
        Just (fph,fwatchlist) -> do
            terminate fph
            rmWatchList fwatchlist
    killINotify inotify
    writeLock
    exitSuccess
  where
    cmd | isCabalDev = "cabal-dev"
        | otherwise  = "cabal"

    diffArgs | isCabalDev = [
              "--cabal-install-arg=-fdevel" -- legacy
            , "--cabal-install-arg=-flibrary-only"
            ]
             | otherwise  = [
              "-fdevel" -- legacy
            , "-flibrary-only"
            ]
    args = "configure":diffArgs ++ ["--disable-library-profiling" ]

try_ :: forall a. IO a -> IO ()
try_ x = (Ex.try x :: IO (Either Ex.SomeException a)) >> return ()

type FileList = Map.Map FilePath EpochTime

getFileList :: [FilePath] -> IO FileList
getFileList hsSourceDirs = do
    (files, deps) <- getDeps hsSourceDirs
    let files' = files ++ map fst (Map.toList deps)
    fmap Map.fromList $ flip mapM files' $ \f -> do
        efs <- Ex.try $ getFileStatus f
        return $ case efs of
            Left (_ :: Ex.SomeException) -> (f, 0)
            Right fs -> (f, modificationTime fs)

checkDevelFile :: IO ()
checkDevelFile = do
  e <- doesFileExist "devel.hs"
  unless e $ failWith "file devel.hs not found"

checkCabalFile :: D.GenericPackageDescription -> IO [FilePath]
checkCabalFile gpd = case D.condLibrary gpd of
    Nothing -> failWith "incorrect cabal file, no library"
    Just ct ->
      case lookupDevelLib gpd ct of
        Nothing   ->
          failWith "no development flag found in your configuration file. Expected a 'library-only' flag or the older 'devel' flag"
        Just dLib -> do
           let hsSourceDirs = D.hsSourceDirs . D.libBuildInfo $ dLib
           fl <- getFileList hsSourceDirs
           let unlisted = checkFileList fl dLib
           unless (null unlisted) $ do
                putStrLn "WARNING: the following source files are not listed in exposed-modules or other-modules:"
                mapM_ putStrLn unlisted
           when (D.fromString "Application" `notElem` D.exposedModules dLib) $
                putStrLn "WARNING: no exposed module Application"
           return hsSourceDirs

failWith :: String -> IO a
failWith msg = do
    putStrLn $ "ERROR: " ++ msg
    exitFailure

checkFileList :: FileList -> D.Library -> [FilePath]
checkFileList fl lib = filter isUnlisted . filter isSrcFile $ sourceFiles
  where
    al = allModules lib
    -- a file is only a possible 'module file' if all path pieces start with a capital letter
    sourceFiles = filter isSrcFile . map fst . Map.toList $ fl
    isSrcFile file = let dirs = filter (/=".") $ splitDirectories file
                     in  all (isUpper . head) dirs && (takeExtension file `elem` [".hs", ".lhs"])
    isUnlisted file = not (toModuleName file `Set.member` al)
    toModuleName = L.intercalate "." . filter (/=".") . splitDirectories . dropExtension

allModules :: D.Library -> Set.Set String
allModules lib = Set.fromList $ map toString $ D.exposedModules lib ++ (D.otherModules . D.libBuildInfo) lib
    where
      toString = L.intercalate "." . D.components

ghcVersion :: IO String
ghcVersion = fmap getNumber $ readProcess "runghc" ["--numeric-version", "0"] []
    where
      getNumber = filter (\x -> isNumber x || x == '.')

ghcPackageArgs :: Bool -> String -> IO [String]
ghcPackageArgs isCabalDev ghcVer
  | isCabalDev = do
      r <- readProcess "cabal-dev" ["buildopts"] []
      let opts = L.lines r
      return $ "-hide-all-packages" : "-no-user-package-conf" : inplacePkg : cabaldevConf : pkgid opts : depPkgIds opts
  | otherwise = return [inplacePkg]
      where
        pkgid opts      = let (_,p) = head (selectOpts ["-package-name"] opts) in "-package-id" ++ p ++ "-inplace"
        depPkgIds opts  = map (uncurry (++)) (selectOpts ["-package-id"] opts)
        inplacePkg   = "-package-confdist/package.conf.inplace"
        cabaldevConf = "-package-confcabal-dev/packages-" ++ ghcVer ++ ".conf"
        selectOpts opts (x1:x2:xs)
           | x1 `elem` opts = (x1,x2):selectOpts opts xs
           | otherwise      = selectOpts opts (x2:xs)
        selectOpts _ _ = []

lookupDevelLib :: D.GenericPackageDescription -> D.CondTree D.ConfVar c a -> Maybe a
lookupDevelLib gpd ct | found     = Just (D.condTreeData ct)
                      | otherwise = Nothing
  where
    flags = map (unFlagName . D.flagName) $ D.genPackageFlags gpd
    unFlagName (D.FlagName x) = x
    found = any (`elem` ["library-only", "devel"]) flags

-- | Acts like @rawSystem@, but filters out lines from the output that we're not interested in seeing.
rawSystemFilter :: String -> [String] -> IO ExitCode
rawSystemFilter command args = do
    (inh, outh, errh, ph) <- runInteractiveProcess command args Nothing Nothing
    hClose inh
    let go handlein handleout = do
            isEof <- hIsEOF handlein
            if isEof
                then hClose handlein
                else do
                    line <- hGetLine handlein
                    unless ("Loading package " `L.isPrefixOf` line) $ hPutStrLn handleout line
                    go handlein handleout
    _ <- forkIO $ go outh stdout
    _ <- forkIO $ go errh stderr
    waitForProcess ph
