{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveGeneric #-} 

{- |
   Module      : Main.Wreq.Tutorial 
   Description : This is the Wreq tutorial from the BOS. 
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com 

   See 
   http://www.serpentine.com/wreq/tutorial.html
   
 -}

import Network.Wreq (get, getWith, asJSON, responseBody, responseHeader, header, 
                     defaults, post, partText, partFile, responseCookie, 
                     cookieValue, auth, basicAuth, awsAuth, statusCode, 
                     AWSAuthVersion( AWSv4 ), FormParam( (:=) ) ) 

-- get :: String -> IO (Response ByteString)
-- getWith :: Options -> String -> IO (Response ByteString) 
-- asJSON :: (MonadThrow m, FromJSON a) => Response ByteString -> m (Response a)
-- responseBody :: Lens (Response body0) (Response body1) body0 body1
-- responseHeader :: HeaderName  -> Traversal' (Response body) ByteString
-- header :: HeaderName -> Lens' Options [ByteString] 
-- defaults :: Options
-- post :: Postable a => String -> a -> IO (Response ByteString) 
-- partText :: Text -> Text -> Part  
-- partFile :: Text -> FilePath -> Part
-- responseCookie :: ByteString -> Fold (Response body) Cookie   
-- cookieValue :: Lens' Cookie ByteString
-- auth :: Lens' Options (Maybe Auth) 
-- basicAuth :: ByteString -> ByteString -> Auth  
-- awsAuth :: AWSAuthVersion -> ByteString -> ByteString -> Auth 
-- statusCode :: Lens' Status Int 

-- (:=) :: FormValue v => ByteString -> v -> FormParam 

import qualified Network.Wreq.Session as S 
-- withSession :: (Session -> IO a) -> IO a 
-- newSession :: IO Session

import Control.Lens 
import Data.Map 
import Data.Text 
import GHC.Generics (Generic) 
import Data.Aeson (FromJSON)
import Data.Aeson.Lens (_String, key, members) 
import Data.ByteString 

import Network.HTTP.Client ( HttpExceptionContent( StatusCodeException ) ) 
import qualified Control.Exception as E 
import qualified Data.List as L 

data GetBody = GetBody {
    headers :: Map Text Text
  , args :: Map Text Text
  , origin :: Text
  , url :: Text
  } deriving (Show, Generic)

instance FromJSON GetBody 

-- getAuth url myauth = get url `E.catch` handler 
--   where 
--     handler e@(StatusCodeException s _) 
--       | s ^. statusCode == 401 = getWith authopts authurl 
--       | otherwise              = E.throwIO e 
--       where authopts = defaults & auth .~ myauth 
            -- switch to TLS when we use auth 
--             authurl = "https" ++ L.dropWhile (/=':') url 

main :: IO ()
main = do
  r <- asJSON =<< get "http://httpbin.org/get"
  print (headers (r ^. responseBody)) 
-- -> fromList [("Accept-Encoding","gzip"),("Host","httpbin.org"),("User-Agent","haskell wreq-0.5.3.3"),("X-Amzn-Trace-Id","Root=1-60e65f63-2d1a81016188b8ab081f2e3d")]

  s <- get "http://httpbin.org/get" 
  print (s ^? responseBody . key "url") 
  print (s ^. responseBody . key "url" . _String) 
  print (s ^. responseBody . key "foobar" . _String) 
  print (s ^? responseBody . key "foobar" . _String) 

-- ->   Just (String "http://httpbin.org/get")  "http://httpbin.org/get" "" Nothing

  let opts = defaults & header "Accept" .~ ["application/json"]
  t <- getWith opts "http://httpbin.org/get"
  print (t ^. responseHeader "content-type") 

  u <- post "http://httpbin.org/post"  
             [("num" :: ByteString) := ("3" :: ByteString), 
              ("str" :: ByteString) := ("wat" :: ByteString)] 
-- u <- post "http://httpbin.org/post" ["num" := 3, "str" := "wat"] 
-- this works in ghci 
  print (u ^? responseBody . key "form")  
-- ->  Just (Object (fromList [("num",String "3"),("str",String "wat")])) 
  print (u ^? responseBody . key "headers" . key "Content-Type" . _String)   
-- ->  Just "application/x-www-form-urlencoded" 

  v <- post "http://httpbin.org/post" [partText "button" "o hai"] 
  print (v ^? responseBody . key "headers" . key "Content-Type" . _String) 
-- ->  Just "multipart/form-data; boundary=----WebKitFormBoundaryf5AneEBODvqg35xI"
  print (v ^.. responseBody . key "form" ) 
-- ->  [Object (fromList [("button",String "o hai")])] 

  w <- post "http://httpbin.org/post" (partFile "file" "hello.hs") 
  print (w ^.. responseBody . key "files" . members . _String) 
-- ->  ["main = putStrLn \"hello\" \r\n"] 

  x <- get "http://httpbin.org/cookies/set?foo=bar"   
  print (x ^. responseCookie "foo" . cookieValue ) 
-- ->  "bar" 

--  y <- get "http://httpbin.org/basic-auth/user/pass"   
--  print (y ) 
-- ->  401 . This failure means that the next get is ignored.  

  let opts2 = defaults & auth ?~ basicAuth "user" "pass" 
  z <- getWith opts2 "https://httpbin.org/basic-auth/user/pass"   
  print (z ^. responseBody ) 
-- -> "{\n  \"authenticated\": true, \n  \"user\": \"user\"\n}\n" 

--  let opts3 = defaults & auth ?~ awsAuth AWSv4 "key" "secret" 
--                       & header "Accept" .~ ["application/json"] 
--  a <- getWith opts3 "https://sqs.us-east-1.amazonaws.com/?Action=ListQueues"   
--  print (a ^. responseBody ) 
-- -> "The security token included in the request is invalid. 

  S.withSession $ \sess -> do 
--  S.newSession $ \sess -> do 
-- first request 
  S.get sess "http://httpbin.org/cookies/set?name=hi"  
-- second request 
  b <- S.post sess "http://httpbin.org/post" ["a" := (3 :: Int)] 
  print $ b ^. responseCookie "name" . cookieValue  
-- -> "hi"
