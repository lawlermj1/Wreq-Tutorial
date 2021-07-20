								
# Using Wreq library in Haskell 								
								
								
## Purpose 								
								
These modules use Haskell to read the Aristole API, parse the JSON and export the information into CSVs. 								
								
## Background 								
								
This shows the reaults of a tutorial, and an example of a bearer token. 								
								
There are still some unknowns, so this will be added to. 								
								
								
The JSON parser was built in Haskell 9 using the wreq, lens, words8, aeson and cassava packages. This software has only been used by myself. It is only run intermittently when a new export was needed. Therefore, the IO is very simple. There is also no Cabal install.  								
								
Any errors or misapplication of Haskell are my own. 								
								
## Design - Custom Modules 								
								
The following includes hs modules, as well as input json files, sample output csv files and text files. 								
								
File	| 	Ord	| 	Lines	| 	FT	| 	For
-----------------------	| 	--	| 	-----	| 	----	| 	-----------------------
Aristotle.Wreq.oauth2Bearer.json	| 	1	| 	1	| 	json	| 	Sample json output created from use of oauth2Bearer function.
Aristotle.Wreq.oauth2Bearer.Mod.json	| 	2	| 	150	| 	json	| 	Sample json output created from use of oauth2Bearer function, with returns. 
Aristotle.Wreq.oauth2Token.json	| 	3	| 	1	| 	json	| 	Sample json output created from use of oauth2Token function.
Aristotle.Wreq.oauth2Token.Mod.json	| 	4	| 	230	| 	json	| 	Sample json output created from use of oauth2Token function, with returns. 
AristotleCommon.hs	| 	5	| 	380	| 	hs	| 	This module defines common Wreq functions, and common or reused Aristotle JSON objects. Crawl is a fold function that gathers together all Aristotle object pages until next page is null.  Checkparse exposes JSON parser errors. 
AristotleObjectClass.hs	| 	6	| 	190	| 	hs	| 	This module contains specific JSON and CSV parsing functions for Aristotle Object Class objects. 
CassavaDate.hs	| 	7	| 	90	| 	hs	| 	This module defines unused Date CSV parsing functions. 
CassavaUtils.hs	| 	8	| 	170	| 	hs	| 	This module defines common CSV, String, Word8 and Bytestring functions. 
fullAristotleObjectClass.csv	| 	9	| 	76	| 	csv	| 	Sample csv output 
hello.hs	| 	10	| 	1	| 	hs	| 	Dummy hello program used in tutorial.
Main.Cassava.hs	| 	11	| 	90	| 	hs	| 	This is the main module for a sample Cassava functions. 
Main.Wreq.Token.hs	| 	12	| 	50	| 	hs	| 	This is the main module for a sample Wreq token call. 
Main.Wreq.Tutorial.hs	| 	13	| 	140	| 	hs	| 	This is the main module for the Wreq tutorial. 
Z AristotleObjectClass.urls.txt	| 	14	| 	4	| 	txt	| 	Sample output from Main.Wreq.Token
Z fetched.out	| 	15	| 	1	| 	out	| 	Sample output from Main.Wreq.Token for the Fetched type. 
Z httpbin.org get.txt	| 	16	| 	14	| 	txt	| 	Sample output from Main.Wreq.Tutorial.
Z Install.txt	| 	17	| 	1840	| 	txt	| 	Results of all install steps
Z Main.Wreq.ghci.txt	| 	18	| 	240	| 	txt	| 	Record of ghci usage.
token.txt	| 	19	| 	1	| 	txt	| 	Contains Aristotle supplied API token. This is not checked in for obvious security reasons. 
