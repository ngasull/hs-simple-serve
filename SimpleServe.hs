{-# LANGUAGE NamedFieldPuns #-}

import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import qualified Data.Map as M
import Network
import System.IO
import Text.Read (readMaybe)

data Method = DELETE | GET | HEAD | POST | PUT deriving (Read, Show)

data Request = Request { method :: Method
                       , uri :: String
                       , headers :: M.Map String String
                       } deriving (Show)

data Response = Response { body :: String
                         , code :: Int
                         }

instance Show Response where
  show Response { body, code } = concat [ show code, " : ", body, "\n" ]

main = withSocketsDo $
  bracket (listenOn (PortNumber 8080)) sClose loop

loop :: Socket -> IO ()
loop sock = do
  (h,_,_) <- accept sock
  forkFinally (handleRequest h) (const $ hClose h)
  loop sock

handleRequest :: Handle -> IO ()
handleRequest h = do
  -- body <- withFile "Setup.hs" ReadMode hGetContents
  req <- parseRequest <$> getHeaders h
  let res = case req of
          Just Request { method, uri } -> Response
            { code = 200
            , body = show method ++ " " ++ uri }
          _ -> Response { code = 500, body = "Woupinnaiz!" }

  hPutStr h $ httpRequest res

getHeaders :: Handle -> IO [String]
getHeaders h = reverse <$> getHeaders' [] h

getHeaders' :: [String] -> Handle -> IO [String]
getHeaders' acc h = do
  line <- hGetLine h
  if line == "\r"
    then return acc
    else getHeaders' (line:acc) h

httpRequest :: Response -> String
httpRequest res = concat
  [ "HTTP/1.0 200 OK\r\n"
  , "Content-Length: ", (show.length) content, "\r\n"
  , "\r\n"
  , content
  ]
  where content = show res

parseRequest :: [String] -> Maybe Request
parseRequest reqHeaders =
  if (not.null) reqHeaders && length hh == 3 && isJust mMaybe
  then Just Request { method, headers, uri }
  else Nothing
  where hh = words . head $ reqHeaders
        [m, uri, _] = hh
        mMaybe = readMaybe m :: Maybe Method
        Just method = mMaybe
        headers = M.fromList [ (h, v) | l <- map (splitOn ": ") (tail reqHeaders)
                                      , length l >= 2 , let h:v:_ = l ]
