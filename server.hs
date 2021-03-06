{-# LANGUAGE RecordWildCards #-}
module Main where

import ConcurrentUtils

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.Map as Map
import Data.Map (Map)
import System.IO
import Control.Exception
import Network
import Control.Monad
import Text.Printf
import System.Directory
import Data.Char
import GHC.Conc.Sync
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Email.Validate
import qualified Data.ByteString.Char8 as BS
import Data.List
import System.FilePath ((</>))

-- <<main
main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  createDirectoryIfMissing False "files"
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk handle server) (\_ -> hClose handle)

port :: Int
port = 44444
-- >>


-- ---------------------------------------------------------------------------
-- Data structures and initialisation

-- <<Client
type ClientName = String

data Client = Client
  { clientName     :: ClientName
  , clientHandle   :: Handle
  , clientKicked   :: TVar (Maybe String)
  , clientSendChan :: TChan Message
  }
-- >>

-- <<newClient
newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  return Client { clientName     = name
                , clientHandle   = handle
                , clientSendChan = c
                , clientKicked   = k
                }
-- >>

-- <<Server
data Server = Server
  { clients :: TVar (Map ClientName Client)
  }

newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  return Server { clients = c }
-- >>

-- <<Message
data Message = Notice String
             | Send ClientName String String
             | Broadcast ClientName String
             | Command String
-- >>

-- -----------------------------------------------------------------------------
-- Basic operations

-- <<broadcast
broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientmap <- readTVar clients
  mapM_ (\client -> sendEmail client msg) (Map.elems clientmap)
-- >>

-- <<sendEmail
sendEmail :: Client -> Message -> STM ()
sendEmail Client{..} msg =
  writeTChan clientSendChan msg
-- >>

-- <<sendToName
sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
  clientmap <- readTVar clients
  case Map.lookup name clientmap of
    Nothing -> writeEmailFile name msg >> return True
    Just client -> sendEmail client msg >> return True
-- >>

writeEmailFile :: ClientName ->Message -> STM Bool
writeEmailFile name message = do
  time <- round `fmap` (unsafeIOToSTM $ getPOSIXTime)
  case message of
     Send n s b -> do{
       (unsafeIOToSTM $ createDirectoryIfMissing False ("files/" ++ name));
       (unsafeIOToSTM $ writeFile ("files/" ++ name ++ "/"++(show time)++".mail") ("From: " ++ n ++ "\nSubject: " ++ s ++ "\nBody: " ++ b ++ "\n------------------------------------------\n"));
     } >> return True

-- -----------------------------------------------------------------------------
-- The main server

talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
  hSetNewlineMode handle universalNewlineMode
      -- Swallow carriage returns sent by telnet clients
  hSetBuffering handle LineBuffering
  readName
 where
-- <<readName
  readName = do
    hPutStrLn handle "What is your email address?"
    name <- hGetLine handle
    if False == isValid (BS.pack name)
      then do
        hPutStrLn handle "The email address is invalid, please choose another"
        readName
      else if null name
        then readName
        else mask $ \restore -> do        -- <1>
               ok <- checkAddClient server name handle
               case ok of
                 Nothing -> restore $ do  -- <2>
                    hPrintf handle "The email address is in use, please choose another\n"
                    readName
                 Just client -> do{
                    restore (runClient server client) -- <3>
                        `finally` removeClient server name
                 }
-- >>

-- <<checkAddClient
checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  if Map.member name clientmap
    then return Nothing
    else do client <- newClient name handle
            writeTVar clients $ Map.insert name client clientmap
            broadcast server  $ Notice (name ++ " is online")
            return (Just client)
-- >>

-- <<removeClient
removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' clients $ Map.delete name
  broadcast server $ Notice (name ++ " is offline")
-- >>

getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir =
  getDirectoryContents dir >>= mapM (canonicalizePath . (dir </>))

readFilesFromDirectory :: Client -> IO ()
readFilesFromDirectory client@Client{..} = do {
      directoryExists <- doesDirectoryExist ("files/"++clientName);
      if directoryExists then do{
            hPutStrLn clientHandle "Unread emails:\n";
           (getAbsDirectoryContents ("files/"++clientName))
              >>= filterM(fmap not .doesDirectoryExist)
              >>= mapM readFile
              >>= mapM_ (hPutStrLn clientHandle)
          }
      else
        return ()
}

removeEmailDirectory :: Client -> IO()
removeEmailDirectory client@Client{..} = do{
  directoryExists <- doesDirectoryExist ("files/"++clientName);
  if directoryExists then 
    removeDirectoryRecursive ("files/"++clientName)
    else
  return ()
}


-- <<runClient
runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  readFilesFromDirectory client  
  removeEmailDirectory client
  race server receive
  return ()
 where
  receive = forever $ do
    hPutStrLn clientHandle "\nAvailable commands:"
    hPutStrLn clientHandle "/send <email_address> --Send an email"
    hPutStrLn clientHandle "/quit --Exit\n"
    msg <- hGetLine clientHandle
    case words msg of
      ["/send", who] -> do
        if False == isValid (BS.pack who)
          then do
            hPutStrLn clientHandle "The email address is invalid"
            return False
          else do
            hPutStrLn clientHandle "Subject: "
            subject <- hGetLine clientHandle
            hPutStrLn clientHandle "Body: "
            body <- hGetLine clientHandle
            atomically $ sendToName serv who (Send clientName subject body)
            return True
      _ -> do
          atomically $ sendEmail client (Command msg)
          return True

  server = join $ atomically $ do
    k <- readTVar clientKicked
    case k of
      Just reason -> return $
        hPutStrLn clientHandle $ "You have been kicked: " ++ reason
      Nothing -> do
        msg <- readTChan clientSendChan
        return $ do
            continue <- handleMessage serv client msg
            when continue $ server
-- >>

-- <<handleMessage
handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
     Notice msg         -> output $ "*** " ++ msg
     Send from sbj body -> output $ "From: " ++ from ++ "\nSubject: " ++ sbj ++ "\nBody: " ++ body
     Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
     Command msg ->
       case words msg of
           ["/quit"] ->
               return False
           _ -> do
               hPutStrLn clientHandle $ "Unrecognised command: " ++ msg
               return True
 where
   output s = do hPutStrLn clientHandle s; return True
-- >>
