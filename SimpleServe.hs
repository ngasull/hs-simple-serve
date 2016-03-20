import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket)
import Network
import System.IO

main = withSocketsDo $
  bracket (listenOn (PortNumber 8080)) sClose loop

loop :: Socket -> IO ()
loop sock = do
  (h,_,_) <- accept sock
  forkFinally (handleRequest h) (const $ hClose h)
  loop sock

handleRequest :: Handle -> IO ()
handleRequest h = do
  body <- getHeaders h
  -- body <- withFile "Setup.hs" ReadMode hGetContents
  hPutStr h $ httpRequest body

getHeaders :: Handle -> IO String
getHeaders h = unlines <$> getHeaders' [] h

getHeaders' :: [String] -> Handle -> IO [String]
getHeaders' acc h = do
  line <- hGetLine h
  if line == "\r"
    then return acc
    else getHeaders' (line:acc) h

httpRequest :: String -> String
httpRequest body = concat
  [ "HTTP/1.0 200 OK\r\n"
  , "Content-Length: ", (show.length) body, "\r\n"
  , "\r\n"
  , body
  ]
