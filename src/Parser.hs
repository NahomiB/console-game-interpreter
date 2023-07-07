module Parser(
    Sentence(..),
    parseSentence,
    parseToSingleCommand,
    parseToCommands
) where

    import Lexer ( Token(..), TokenWord(..), Command(..))

    data Sentence = NullSentence |
                SingleWord Token |
                SimpleSentence Token Token |
                SimpleSentencePreposition Token Token Token |
                ComplexSentence Token Token Token Token |
                ComplexSentencePrepositions Token Token Token Token Token deriving (Show, Eq)

    parseSentence :: [TokenWord] -> [Sentence]
    parseSentence [TokenWord _ tokensA, TokenWord _ tokensB, TokenWord _ tokensC, TokenWord _ tokensD, TokenWord _ tokensE]
        = buildSentence [verbsInTokens tokensA,
                            prepositionsInTokens tokensB,
                            nounsInTokens tokensC,
                            prepositionsInTokens tokensD,
                            nounsInTokens tokensE]
    parseSentence [TokenWord _ tokensA, TokenWord _ tokensB, TokenWord _ tokensC, TokenWord _ tokensD]
        = buildSentence [verbsInTokens tokensA,
                            nounsInTokens tokensB,
                            prepositionsInTokens tokensC,
                            nounsInTokens tokensD]
        ++ buildSentence [nounsInTokens tokensA,
                            nounsInTokens tokensB,
                            nounsInTokens tokensC,
                            nounsInTokens tokensD] 
    parseSentence [TokenWord _ tokensA, TokenWord _ tokensB, TokenWord _ tokensC]
        = buildSentence [verbsInTokens tokensA,
                            prepositionsInTokens tokensB,
                            nounsInTokens tokensC]
    parseSentence [TokenWord _ tokensA, TokenWord _ tokensB]
        = buildSentence [verbsInTokens tokensA,
                            nounsInTokens tokensB]
    parseSentence [TokenWord _ tokensA]
        = buildSentence [verbsInTokens tokensA] 
        ++ buildSentence [keywordsInTokens tokensA]
    parseSentence _ = []

    verbsInTokens :: [Token] -> [Token]
    verbsInTokens [] = []
    verbsInTokens (verb@(Verb _ _) : ts) = verb : verbsInTokens ts
    verbsInTokens (_ : ts) = verbsInTokens ts

    nounsInTokens :: [Token] -> [Token]
    nounsInTokens [] = []
    nounsInTokens (noun@(Noun _ _) : ts) = noun : nounsInTokens ts
    nounsInTokens (_ : ts) = nounsInTokens ts

    prepositionsInTokens :: [Token] -> [Token]
    prepositionsInTokens [] = []
    prepositionsInTokens (preposition@(Preposition _ _) : ts) = preposition : prepositionsInTokens ts
    prepositionsInTokens (_ : ts) = prepositionsInTokens ts

    keywordsInTokens :: [Token] -> [Token]
    keywordsInTokens [] = []
    keywordsInTokens (keywords@(Keyword _ _) : ts) = keywords : keywordsInTokens ts
    keywordsInTokens (_ : ts) = keywordsInTokens ts

    buildSentence :: [[Token]] -> [Sentence]
    buildSentence [] = []
    buildSentence [words]
        = fmap SingleWord words
    buildSentence [verbs, nouns]
        = fmap (\[verb, noun] -> SimpleSentence verb noun)
            ((:) <$> verbs <*>
            ((:) <$> nouns <*> [[]]))
    buildSentence [verbs, prepositions, nouns]
        = fmap (\[verb, preposition, noun] -> SimpleSentencePreposition verb preposition noun)
            ((:) <$> verbs <*>
            ((:) <$> prepositions <*>
            ((:) <$> nouns <*> [[]])))
    buildSentence [verbs, nounsA, prepositions, nounsB]
        = fmap (\[verb, nounA, preposition, nounB] -> ComplexSentence verb nounA preposition nounB)
            ((:) <$> verbs <*>
            ((:) <$> nounsA <*>
            ((:) <$> prepositions <*>
            ((:) <$> nounsB <*> [[]]))))
    buildSentence [verbs, prepositionsA, nounsA, prepositionsB, nounsB]
        = fmap (\[verb, prepositionA, nounA, prepositionB, nounB] -> ComplexSentencePrepositions verb prepositionA nounA prepositionB nounB)
            ((:) <$> verbs <*>
            ((:) <$> prepositionsA <*>
            ((:) <$> nounsA <*>
            ((:) <$> prepositionsB <*>
            ((:) <$> nounsB <*> [[]])))))
    buildSentence _ = []

    parseToSingleCommand :: Sentence -> Command
    parseToSingleCommand NullSentence = Command []
    parseToSingleCommand (SingleWord (Verb a _)) = Command [a]
    parseToSingleCommand (SingleWord (Keyword a _)) = Command [a]
    parseToSingleCommand (SimpleSentence (Verb a _) (Noun b _)) = Command [a, b]
    parseToSingleCommand (SimpleSentencePreposition (Verb a _) (Preposition b _) (Noun c _)) = Command [a, b, c]
    parseToSingleCommand (ComplexSentence (Noun a _) (Noun b _) (Noun c _) (Noun d _)) = Command [a, b, c, d]
    parseToSingleCommand (ComplexSentence (Verb a _) (Noun b _) (Preposition c _) (Noun d _)) = Command [a, b, c, d]
    parseToSingleCommand (ComplexSentencePrepositions (Verb a _) (Preposition b _) (Noun c _) (Preposition d _) (Noun e _)) = Command [a, b, c, d, e]

    parseToCommands :: Sentence -> [Command]
    parseToCommands NullSentence = convertToCommands []
    parseToCommands (SingleWord (Verb _ a)) = convertToCommands [a]
    parseToCommands (SingleWord (Keyword _ a)) = convertToCommands [a]
    parseToCommands (SimpleSentence (Verb _ a) (Noun _ b)) = convertToCommands (sequence [a, b])
    parseToCommands (SimpleSentencePreposition (Verb _ a) (Preposition _ b) (Noun _ c)) = convertToCommands (sequence [a, b, c])
    parseToCommands (ComplexSentence (Noun _ a) (Noun _ b) (Noun _ c) (Noun _ d)) = convertToCommands (sequence [a, b, c, d])
    parseToCommands (ComplexSentence (Verb _ a) (Noun _ b) (Preposition _ c) (Noun _ d)) = convertToCommands (sequence [a, b, c, d])
    parseToCommands (ComplexSentencePrepositions (Verb _ a) (Preposition _ b) (Noun _ c) (Preposition _ d) (Noun _ e)) = convertToCommands (sequence [a, b, c, d, e])

    convertToCommands :: [[String]] -> [Command]
    convertToCommands = map (\list -> Command list)