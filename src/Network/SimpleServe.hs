{-# LANGUAGE NamedFieldPuns #-}

module Network.SimpleServe (listen, makeStore)
where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import qualified Data.Map as M
import Data.Typeable
import Network
import System.IO
import System.Timeout (timeout)
import Text.Read (readMaybe)

type Ext = String
type Mime = String

data Method = DELETE | GET | HEAD | POST | PUT
  deriving (Read, Show)

data Request = Request { method :: Method
                       , uri :: String
                       , headers :: M.Map String [String]
                       }
             | InvalidRequest String
  deriving (Show)

type RequestError = String

data Response = Response { body :: String
                         , code :: Int
                         }

data Store = Store { mimes :: M.Map Mime [Ext]
                   } deriving (Show)

instance Show Response where
  show Response { body, code } = concat [ show code, " : ", body, "\n" ]

listen :: Store -> IO ()
listen store = withSocketsDo $ bracket
  (listenOn (PortNumber 8080)) sClose (loop store)

loop :: Store -> Socket -> IO ()
loop store sock = do
  (h,_,_) <- accept sock
  forkFinally (handleRequest store h) (const $ hClose h)
  loop store sock

handleRequest :: Store -> Handle -> IO ()
handleRequest store h =
  hPutStr h . httpRequest =<< reqToRes =<< req
  where req = do
          headersOrError <- getHeaders h
          return $ case headersOrError of
            Left headers -> parseRequest headers
            Right err -> InvalidRequest err
        reqToRes req = return $ case req of
          Request { method, uri } -> Response
            { code = 200
            , body = show method ++ " " ++ uri }
          InvalidRequest msg -> Response { code = 500, body = "Woupinnaiz! " ++ msg }
        writeRes = hPutStr h

getHeaders :: Handle -> IO (Either [String] RequestError)
getHeaders h = getHeaders' []
  where getHeaders' :: [String] -> IO (Either [String] RequestError)
        getHeaders' acc =
          if length acc > maxHeaders then failure
          else do
            line <- timeout 10000 (hGetLine h)
            case line of
              Just "\r" -> return (Left (reverse acc))
              Just line -> getHeaders' (line:acc)
              _ -> failure
        failure = return $ Right "Could not finish to read the headers properly"
        maxHeaders = 20
        readTimeout = 10e6

getMimeDictionnary :: IO (M.Map Mime [Ext])
getMimeDictionnary = M.fromList . mapMimeToExt <$> readFile "/etc/mime.types"
  where mapMimeToExt contents = [(mime, exts) | l <- lines contents , not (null l) , head l /= '#'
                                              , let wds = words l , length wds > 1
                                              , let mime:exts = wds ]

httpRequest :: Response -> String
httpRequest res = concat
  [ "HTTP/1.0 200 OK\r\n"
  , "Content-Length: ", (show.length) content, "\r\n"
  , "\r\n"
  , content
  ]
  where content = show res

makeStore :: IO Store
makeStore = do
  mimes <- getMimeDictionnary
  return Store { mimes }

parseRequest :: [String] -> Request
parseRequest reqHeaders =
  if not (null reqHeaders) && length hh == 3 && isJust mMaybe
  then Request { method, headers, uri }
  else InvalidRequest "Top header isn't parseable"
  where hh = words (head reqHeaders)
        [m, uri, _] = hh
        mMaybe = readMaybe m :: Maybe Method
        Just method = mMaybe
        headers = M.fromListWith (++) [(h, [v]) | l <- map (splitOn ": ") (tail reqHeaders)
                                                , length l == 2 , let h:v:_ = l ]
