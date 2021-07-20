{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

{- |
   Module      : CassavaDate 
   Description : Utilities for Cassava Date processing- UNUSED  
   Copyright   : ( c ) Matthew Lawler 2021   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Utilities for Cassava CSV library 

   Day parsing left in but not needed as cassava parses Day now. 
   
   Best site: https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/ 
   
 -}
module CassavaDate 
    (  


     ) where

import Data.Word8   
import Data.Time 
import Data.Char  (ord) 
import Text.Regex


-- ----------------- 
-- Aeson parses Day well, so no need for this code. 
-- Here just in case. 
--  parse 29/07/2020 23:32:55
--  default = epoch date 
-- 2019-10-02T14:21:01.572991+10:00 
-- ^(19|20)\d\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$
-- matches 1900-01-01 through 2099-12-31

--type YYYY = String  
--type MM = String 
--type DOM = String  

type YYYY = [Word8]  
type MM = [Word8] 
type DOM = [Word8]  

-- d :: Char -> Bool
d = isDigit 

epochDay :: Day 
epochDay = fromGregorian 1970 1 1 

-- yyyy :: RE Char YYYY 
yyyy :: RE Word8  YYYY 
yyyy = many $ psym $ d 

-- mm :: RE Char MM  
mm :: RE Word8 MM  
mm = many $ psym $ d 

-- dayOfMM :: RE Char DOM  
dayOfMM :: RE Word8 DOM 
dayOfMM = many $ psym $ d 

data Date = Date YYYY MM DOM deriving Show 

-- add date functions dependeding on format 
-- sep must be Char 
-- sep = "-/. " :: String 
-- sepDash = '-' :: Char 
-- sepSlash = '/' :: Char  

sepDash = fromIntegral (ord '\n') :: Word8 

--dateISO :: RE Char Date 
dateISO :: RE Word8 Date 
dateISO = Date <$> yyyy <* sym sepDash <*> mm <* sym sepDash <*> many anySym  

-- instance FromField Data.Time.Day where 
--    parseField bs 
--       | isJust (s =~ dateISO) = pure (fromJust (parseTimeM True defaultTimeLocale "%Y-%m-%d" s)) 
--       | isJust (s =~ date) = pure (fromJust (parseTimeM True defaultTimeLocale "%Y/%m/%d" s))        
--       | otherwise = pure epochDay 
--       where s = (B.unpack (B.take 10 bs)) 

-- instance ToField Data.Time.Day where  
--    toField = B.pack showGregorian 

