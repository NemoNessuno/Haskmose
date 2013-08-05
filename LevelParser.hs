module LevelParser(createFromFile)  where

import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

import Types
import Helper

levelFile:: ParsecT String () Identity [[String]]
levelFile = endBy objects eol

objects:: ParsecT String () Identity [String]
objects = sepBy vals (char ';')

vals:: ParsecT String () Identity String
vals = many (noneOf ";\n")

eol:: ParsecT String () Identity Char
eol = char '\n'

parseLevel :: String -> IO (Either ParseError [[String]])
parseLevel input = parseFromFile levelFile input

value::Either a b -> b
value (Left x) = error "fehler"
value (Right x) = x

--creates a Tuple (playerobject,[foreignobjects])
createFromFile :: String -> IO (Object, [Object])
createFromFile path = do
			extr <- parseLevel path
			return (ent (value extr))
				where ent x = let list=crCsvObj x in
						((head list), (tail list))

--turns List to objects, only lists of length 5 are considered
crCsvObj::[[String]]->[Object]
crCsvObj xs = map str2Obj (filter (\x->length x==5) xs)
		--str2Obj x : crCsvObj xs
		where
			str2Obj::[String]->Object
			str2Obj xs = let f=(\x->read (xs !! x)) in
					mkObject (f 0) (f 1) (f 2) (f 3) (f 4)
