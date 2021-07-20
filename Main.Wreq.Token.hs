{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveGeneric #-} 

{- |
   Module      : Main.Wreq.Token   
   Description : This is an example of using Wreq and token to fetch multiple pages.  
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com 

  -}

import qualified Data.ByteString.Lazy as B 
import Data.Csv (encodeDefaultOrderedByName)
import Data.List (nub)
import Network.Wreq (auth, oauth2Token, Options ) 
import Control.Lens ( (?~), (&) ) 
import qualified Data.ByteString.Internal as I 

import CassavaUtils 
import AristotleCommon
import AristotleObjectClass  

-- read the Aristotle provided token 
getToken :: IO String 
getToken = readFile "token.txt" 

main :: IO ()
main = do

-- convert token to correct type and add to Options 
 tokenIn <- getToken 
 let opts1 = opts2 & auth ?~ oauth2Token (s2bs tokenIn) :: Options 

 let page1 = "http://dss.aristotlecloud.io/api/v4/metadata/objectclass?page=1" 

-- only gets 3 pages; actual is just under 100 pages 
 fetched <- crawl opts1 3 (Just page1) [] 

 let parsed = map (rb2aoc.fetchedBody) fetched  

 print (nub (map checkParse parsed))  

 writeFile "Z fetched.out" (show fetched)  

 let aocList = (concat (map (rb2aocOut.fetchedBody) fetched))  
 let aocOut = encodeDefaultOrderedByName aocList 

 B.writeFile "fullAristotleObjectClass.csv" aocOut  
