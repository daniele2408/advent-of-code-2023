{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module FuncsDay15 where
import Data.Char (ord)
import CommonFuncs
import Data.List.Split

hashAlgo :: String -> Int
hashAlgo s = hashAlgoString s 0

hashAlgoString :: String -> Int -> Int
hashAlgoString [] acc = acc
hashAlgoString (x:xs) acc = hashAlgoString xs (hashAlgoChar x acc)

hashAlgoChar :: Char -> Int -> Int
hashAlgoChar c acc = (\x -> mod x 256) $ (* 17) $ (+ curVal) $ acc
  where curVal = ord c

answerQuestionDay15 :: String -> Int
answerQuestionDay15 inputText = sum $ map (\w -> hashAlgo w) $ splitOn "," inputText

data Lens = Lens { name :: String, focalLength :: Int } deriving (Show, Eq)
type Box = [Lens]
type Boxes = [Box]

type Command = Either Lens Lens

createCommand :: String -> Command
createCommand s
  | (== '-') $ last s = Left Lens { name = takeStringButLastChar s, focalLength = -1 }
  | elem '=' s = Right $ parseLensFromString s
  | otherwise = error "Unknown command"

parseLensFromString :: String -> Lens
parseLensFromString s = Lens { name = n, focalLength = (convertStrToInt fn) }
  where (n:fn:_) = splitStringByAndStripWhiteSpaces "=" s

applyCommandIterative :: Boxes -> [Command] -> Boxes
applyCommandIterative bs [] = bs
applyCommandIterative bs (c:cs) = applyCommandIterative (applyCommand bs c) cs

applyCommand :: Boxes -> Command -> Boxes
applyCommand boxes (Left lens) = removeLensFromBox boxes lens
applyCommand boxes (Right lens) = replaceLensInBox boxes lens

removeLensFromBox :: Boxes -> Lens -> Boxes
removeLensFromBox bs l = replaceAtIndex i (removeLens b l) bs
  where i = hashAlgo $ name l
        b = bs !! i

replaceLensInBox :: Boxes -> Lens -> Boxes
replaceLensInBox bs l = replaceAtIndex i (replaceLens b l) bs
  where i = hashAlgo $ name l
        b = bs !! i

removeLens :: Box -> Lens -> Box
removeLens b l = filter (\lens -> (name lens) /= (name l)) b

containsLensByName :: Box -> Lens -> Bool
containsLensByName b l = any (\lens -> (name lens) == (name l)) b

getPosLensByName :: Box -> Lens -> Maybe Int
getPosLensByName b l
    | res == [] = Nothing
    | otherwise = Just $ head res
    where res = map (\p -> fst p) $ filter (\p -> (name $ snd p) == (name l)) $ listWithIndex b

replaceLens :: Box -> Lens -> Box
replaceLens b l =
    case pos of
        Nothing -> b ++ [l]
        Just pos -> replaceAtIndex pos l b
    where pos = getPosLensByName b l

computeFocusingPower :: Boxes -> Int
computeFocusingPower bs = sum $ map (\p -> computeBoxPower (snd p) (fst p)) $ listWithIndex bs

computeBoxPower :: Box -> Int -> Int
computeBoxPower b boxPos = sum $ map (\p -> computeLensPower (snd p) boxPos (fst p)) $ listWithIndex b

computeLensPower :: Lens -> Int -> Int -> Int
computeLensPower l boxPos lensPos = (focalLength l) * (boxPos+1) * (lensPos+1)

answerQuestionDay15' :: String -> Int
answerQuestionDay15' inputText = computeFocusingPower $ applyCommandIterative boxes commands
  where boxes = take 256 $ repeat []
        commands = map (\s -> createCommand s) $ splitOn "," inputText