{-# LANGUAGE NamedFieldPuns #-}

module Network.SimpleServe (listen, makeStore)
where

import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import qualified Data.Map as M
import Network
import System.IO
import Text.Read (readMaybe)

type Ext = String
type Mime = String

data Method = DELETE | GET | HEAD | POST | PUT deriving (Read, Show)

data Request = Request { method :: Method
                       , uri :: String
                       , headers :: M.Map String [String]
                       } deriving (Show)

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
handleRequest store h = do
  req <- parseRequest <$> getHeaders h
  let res = case req of
          Just Request { method, uri } -> Response
            { code = 200
            , body = show method ++ " " ++ uri }
          _ -> Response { code = 500, body = "Woupinnaiz!" }

  hPutStr h $ httpRequest res

getHeaders :: Handle -> IO [String]
getHeaders h = reverse <$> getHeaders' []
  where getHeaders' :: [String] -> IO [String]
        getHeaders' acc = do
          line <- hGetLine h
          if line == "\r"
            then return acc
            else getHeaders' (line:acc)

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

parseRequest :: [String] -> Maybe Request
parseRequest reqHeaders =
  if not (null reqHeaders) && length hh == 3 && isJust mMaybe
  then Just Request { method, headers, uri }
  else Nothing
  where hh = words (head reqHeaders)
        [m, uri, _] = hh
        mMaybe = readMaybe m :: Maybe Method
        Just method = mMaybe
        headers = M.fromListWith (++) [(h, [v]) | l <- map (splitOn ": ") (tail reqHeaders)
                                                , length l == 2 , let h:v:_ = l ]
