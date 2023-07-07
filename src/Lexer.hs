module Lexer(Token (..),
            TokenWord (..),
            Command(..),
            lexInput,
            isKeyword) where

    import Data.Char ( toLower )

    -- start types definitions

    data Token = Verb String [String] | Noun String [String] | Preposition String [String] | Keyword String [String] deriving (Show, Eq)

    data TokenWord = TokenWord String [Token] deriving (Show, Eq)

    data Command = Command [String] deriving (Show)
    instance Eq Command where
        (Command xs) == (Command ys) = xs == ys
    
    -- end definitions

    --identify a command as keyword 
    isKeyword :: Command -> [Token] -> Bool
    isKeyword (Command words) availableKeywords = length words == 1 && ((head words) `elem` (map (\(Keyword a _ )-> a) availableKeywords))

    -- two tokenwords can be joined if the lead word is the same
    join :: Maybe TokenWord -> Maybe TokenWord -> Maybe TokenWord
    join Nothing Nothing = Nothing
    join (Just a) Nothing = Just a
    join Nothing (Just b) = Just b
    join (Just (TokenWord wordA tokensA)) (Just (TokenWord wordB tokensB))
        | wordA == wordB = Just (TokenWord wordA (tokensA ++ tokensB))
        | otherwise = Nothing

    -- if a word is significant -> return its token and if it is not -> return Nothing
    lexInputAux :: [Token] -> [String] -> [Maybe TokenWord]
    lexInputAux possibleTokens [] = []
    lexInputAux possibleTokens (word : words) = [foldl (\acc token -> tokenize word token `join` acc) Nothing possibleTokens] ++ lexInputAux possibleTokens words

    -- filter tokens to remove Nothings
    le :: [Maybe TokenWord] -> [TokenWord]
    le tokenList = [t | Just t <- tokenList]

    lexInput :: [Token] -> [String] -> [TokenWord]
    lexInput possibleTokens words = le (lexInputAux possibleTokens words)

    -- get token from word
    tokenize :: String -> Token -> Maybe TokenWord
    tokenize "" _  = Nothing
    tokenize word token@(Verb _ synonyms)
        | wordLower `elem` synonyms = Just (TokenWord word [token])
        | otherwise = Nothing
            where wordLower = (map toLower word)
    tokenize word token@(Noun _ synonyms)
        | word `elem` synonyms = Just (TokenWord word [token])
        | wordLower `elem` synonyms = Just (TokenWord word [token])
        | otherwise = Nothing
            where wordLower = (map toLower word)
    tokenize word token@(Preposition _ synonyms)
        | wordLower `elem` synonyms = Just (TokenWord word [token])
        | otherwise = Nothing
            where wordLower = (map toLower word)
    tokenize word token@(Keyword _ synonyms)
        | wordLower `elem` synonyms = Just (TokenWord word [token])
        | otherwise = Nothing
            where wordLower = (map toLower word)