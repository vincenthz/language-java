module Main where

import qualified Language.Java.Parser as J

import qualified Control.Exception as CX
import qualified System.Directory as SD
import qualified System.IO as SIO
import qualified System.Exit as SX
import qualified System.Environment as SE
import qualified System.FilePath as SF
import Text.Printf(printf)

type Log = Maybe SIO.Handle

-- "C:\\jdk6\\src\\java\\lang\\ref"
main = do
 as <- SE.getArgs
 case as of
   [dir] -> processDir Nothing dir
   _     -> fatal "expected dir argument"
 SX.exitSuccess

processDirLog = processDir (Just "log.txt")

processDir :: Maybe FilePath -> FilePath -> IO ()
processDir mfp root = do
 mh <- case mfp of
         Just fp -> fmap Just $ SIO.openFile fp SIO.WriteMode
         Nothing -> return Nothing

 tree <- enumTree root
 res <-  mapM (processFile mh) $ filter ((==".java") .
                                         SF.takeExtension) tree
 let num = length res
 let suc = length (filter id res)
 putStrLn $ printf "%d / %d" suc num

 case mh of
   Just h -> SIO.hClose h
   Nothing -> return ()


-- Sometimes the lexer blows up and the parser cannot successfully
-- produce a proper error value.
processFile :: Log -> FilePath -> IO Bool
processFile lg javaf = CX.catch (processFileBody lg javaf) handler
 where handler :: CX.SomeException -> IO Bool
       handler se = do logLn lg $ fmtFileFail javaf (show se)
                       return False

processFileBody :: Log -> FilePath -> IO Bool
processFileBody lg javaf = do
 SIO.withFile javaf SIO.ReadMode $ \h -> do
   str <- SIO.hGetContents h
   case J.parseCompilationUnit str of
     Left pe -> do logLn lg $ fmtFileFail javaf (show pe)
                   return False
     Right r -> return True


fmtFileFail :: FilePath -> String -> String
fmtFileFail f s = "FILE: " ++ f ++ "\n" ++ s ++ "\n\n"

logLn :: Log -> String -> IO ()
logLn (Just h) msg = SIO.hPutStrLn h msg -- >> SIO.hFlush h
logLn Nothing  msg = return ()


enumTree :: FilePath -> IO [FilePath]
enumTree = fmap concat . enumTree'
 where enumTree' :: FilePath -> IO [[FilePath]]
       enumTree' dir = do
         let dotOrDotDot "."  = True
             dotOrDotDot ".." = True
             dotOrDotDot _ = False

             pfxDir = ((dir++"/")++)

         allFs <- fmap (map pfxDir . filter (not . dotOrDotDot))
                           (SD.getDirectoryContents dir)

         (ds,fs) <- partitionM SD.doesDirectoryExist allFs
         subDs <- mapM enumTree' ds
         return (fs : concat subDs)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM _ [] = return ([],[])
partitionM p (a:as) = do
 (ts,fs) <- partitionM p as
 z <- p a
 return $ if z then (a:ts,fs) else (ts,a:fs)

fatal m = do
 SIO.hPutStrLn SIO.stderr m
 SX.exitFailure