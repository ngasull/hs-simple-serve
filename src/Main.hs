import Network.SimpleServe (listen, makeStore)

main :: IO ()
main = do
  store <- makeStore
  listen store
