{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : Main.Cassava   
   Description : This tests some Cassava utility functions. 
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com 
   
 -}

import Data.Csv as C 
import Data.Maybe 
import GHC.Generics
import Data.Time 
import Text.Regex.Applicative 
import qualified Data.ByteString.Lazy as B 
import Data.Word8  

import CassavaUtils 

data CassavaIn  = 
  CassavaIn { 
            csvIn_id :: String 
          , csvIn_created :: Day 
           } deriving (Show,Generic) 

data CassavaOut  = 
  CassavaOut { 
            csvOut_id :: String 
          , csvOut_created :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord CassavaOut
instance ToNamedRecord CassavaOut
instance DefaultOrdered CassavaOut 

dateIn :: String  
dateIn = "2019-10-02x" 

-- d2 = "2020-12-25x" 

cIn :: CassavaIn 
--cIn = CassavaIn "xyz" (fromJust (parseTimeM True defaultTimeLocale "%Y-%m-%d" dateIn))   
cIn = CassavaIn "xyz" (fromJust (parseTimeM True defaultTimeLocale "%Y-%m-%d" (take 10 dateIn)))   
--   [,"2019-10-02T14:21:01.572991+10:00"] 

-- convert to out  
c2c :: CassavaIn -> CassavaOut
c2c c = CassavaOut (csvIn_id c)  (showGregorian (csvIn_created c)) 

-- | Location of the local copy, in case you have it, of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "dataElement.page001.json" 

testName :: [Word8] 
testName = map c2w "PIA_Person—date of birth (abbreviated), MMYYYY"

-- Read the local copy of the JSON file.
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom

main :: IO ()
main = do

 let cOut = c2c cIn 

-- Get JSON data from file 1  decode it
 f1 <- getJSONFrom 
-- let f2 = removeCtlCharsW8 (B.unpack f1)  
-- let f3 = removeCtlChars (B.unpack f1)  
 let f3 = ctlChar2SpacesW8 (B.unpack f1)  
-- let f4 = fixCtlChars (B.unpack f1)  [] 

-- B.writeFile "dataElement.page001.NoCtl.json" (B.pack f2) 
 B.writeFile "dataElement.page001.NoCtl.2.json" (B.pack f3) 
-- B.writeFile "dataElement.page001.NoCtl.3.json" (B.pack f4)  
 -- print f2 

-- print (d2 =~ dateISO) 
-- print ((take 10 d2) =~ dateISO)  
 print (isHyphen (c2w '\226')  (c2w '\128') (c2w '\148')) 
 print (isHyphen (c2w '\226')  (c2w '\128') (c2w '\149'))  
-- print (map w2c (fixCtlChars testName [])) 
 print cIn
 print cOut         

