module SpaceManager(
    Condition(..), 
    State(..), 
    ConditionalAction(..), 
    Interaction(..),  
    Scene(..),
    Inventory(..),
    Flag(..),
    Settings(..),
    chooseStartScene,
    performInteraction,
    filterInteraction,
    getOptions,
    performActions,
    filterActions
) where

    import Data.Char (toLower)
    import Data.List (find, delete)
    import qualified Data.Map as Map
    import Data.Maybe (fromMaybe)

    import Lexer (Command(..))

    newtype Flag = Flag (Map.Map String [String]) deriving (Show, Eq)
    newtype Inventory = Inventory [String] deriving (Show, Eq)

    data Condition = InInventory String |
                    ActiveFlag String |
                    Always |
                    Or Condition Condition|
                    And Condition Condition |
                    No Condition deriving (Show, Eq)

    data State = AddToInventory String |
                WithdrawFromInventory String |
                ActivateFlag String |
                DeactivateFlag String |
                ChangeScene String deriving (Show, Eq)

    data ConditionalAction = ConditionalAction { 
        condition :: Condition, 
        description :: String, 
        states :: [State]
    } deriving (Show, Eq)

    data Interaction = Interaction { 
        command :: Command, 
        actions :: [ConditionalAction]
    } deriving (Show, Eq)

    data Scene = Scene {
        name :: String,
        canBeStartPoint :: Bool,
        start :: [ConditionalAction],
        interactions :: [Interaction]
    } deriving (Show, Eq)

    data Settings = Settings {
        gameName :: String,
        defaultPhrase :: String,
        defaultErrorPhrase :: String,
        dynamic :: Bool,
        help :: String,
        quitMessage :: String,
        finishFlag :: String
    }

    {- Start of Scenes Management -}

    -- Handling

    chooseStartScene :: [Scene] -> String -> IO Scene     
    chooseStartScene possibleStarts quitMessage = do
            putStr ("\nEstas en el lobby. Las habitaciones disponibles son " ++ (getOptions (map (\scene -> (name scene)) possibleStarts)) ++ ". ¿En que habitacion te gustaria entrar?\n\n>")
            chosenName <- getLine
            if chosenName == "salir"
                then 
                    putStrLn ("\n" ++ quitMessage) >>
                    return (Scene { name = "salir", canBeStartPoint = False, start = [], interactions = []})
                else
                    case findSceneByName chosenName possibleStarts of
                        Just scene -> return scene
                        Nothing -> chooseStartScene possibleStarts quitMessage

    -- Helpers

    findSceneByName :: String -> [Scene] -> Maybe Scene
    findSceneByName chosenName scenes = find (\scene -> (map toLower chosenName) == (map toLower (name scene))) scenes

    {- End of Scenes Management -}

    {- Start of Actions Management -}

    filterActions :: [ConditionalAction] -> Flag -> Inventory -> Scene -> (String, [State])
    filterActions [] _ _ _ = ("La ejecución de dicha acción no está permitida en este momento.", [])
    filterActions (first : t) flags inventory currentScene | (evaluateCondition (condition first) flags inventory currentScene) = (description first, states first)
                              | otherwise = filterActions t flags inventory currentScene

    performAction :: State -> Flag -> Inventory -> Scene -> [Scene] -> (Flag, Inventory, Scene)
    performAction (AddToInventory object) flags (Inventory inventory) currentScene _ | object `elem` inventory = (flags, Inventory inventory, currentScene)
                                                                    | otherwise = (flags, Inventory (object : inventory), currentScene)
    performAction (WithdrawFromInventory object) flags (Inventory inventory) currentScene _ | object `elem` inventory = (flags, Inventory (delete object inventory), currentScene)
                                                                    | otherwise = (flags, Inventory inventory, currentScene)
    performAction (ActivateFlag flag) (Flag flags) inventory currentScene _ 
        | Just sceneFlags <- Map.lookup (name currentScene) flags,
            flag `elem` sceneFlags = (Flag flags, inventory, currentScene)
        | otherwise = let newSceneFlags = flag : fromMaybe [] (Map.lookup (name currentScene) flags)
            in (Flag (Map.adjust (\_ -> newSceneFlags) (name currentScene) flags), inventory, currentScene)
                                                         
    performAction (DeactivateFlag flag) (Flag flags) inventory currentScene _ 
        | Just sceneFlags <- Map.lookup (name currentScene) flags,
            flag `elem` sceneFlags = let newSceneFlags = delete flag (fromMaybe [] (Map.lookup (name currentScene) flags)) 
                in (Flag (Map.adjust (\_ -> newSceneFlags) (name currentScene) flags), inventory, currentScene)
        | otherwise = (Flag flags, inventory, currentScene)
    
    performAction (ChangeScene sceneName) flags inventory currentScene allScenes = (flags, inventory, fromMaybe currentScene (findSceneByName sceneName allScenes))

    performActions :: [State] -> Flag -> Inventory -> Scene -> [Scene] -> (Flag, Inventory, Scene)
    performActions [] flags inventory currentScene _ = (flags, inventory, currentScene)
    performActions (h : t) flags inventory currentScene allScenes = performActions t newFlags newInventory newCurrentScene allScenes
        where (newFlags, newInventory, newCurrentScene) = performAction h flags inventory currentScene allScenes

    {- End of Actions Management -}

    {- Start of Interaction Management -}

    filterInteraction :: Scene -> [Command] -> Interaction
    filterInteraction scene commands = if length matchingInteractions == 0
                                            then Interaction {actions = [], command = Command []}
                                            else head matchingInteractions
                        where
                            matchingInteractions = filter (\interaction -> (command interaction) `elem` commands ) (interactions scene) 

    performInteraction :: Interaction -> Flag -> Inventory -> Scene -> [Scene] -> (String, Flag, Inventory, Scene)
    performInteraction interaction flags inventory currentScene allScenes = (description, newFlags, newInventory, newCurrentScene)
                                            where 
                                                (description, states) = filterActions (actions interaction) flags inventory currentScene
                                                (newFlags, newInventory, newCurrentScene) = performActions states flags inventory currentScene allScenes
    
    {- End of Interaction Management -}

    {- Start of Conditions Management -}

    evaluateCondition :: Condition -> Flag -> Inventory -> Scene -> Bool

    evaluateCondition Always _ _ _ = True
    evaluateCondition (ActiveFlag flag) (Flag flags) _ currentScene =
        case Map.lookup (name currentScene) flags of
            Just sceneFlags -> flag `elem` sceneFlags
            Nothing -> False
    evaluateCondition (InInventory object) _ (Inventory inventory) _ = object `elem` inventory
    evaluateCondition (No condition) flags inventory currentScene = not (evaluateCondition condition flags inventory currentScene)
    evaluateCondition (And conditionA conditionB) flags inventory currentScene = (evaluateCondition conditionA flags inventory currentScene) && (evaluateCondition conditionB flags inventory currentScene)
    evaluateCondition (Or conditionA conditionB) flags inventory currentScene = (evaluateCondition conditionA flags inventory currentScene) || (evaluateCondition conditionB flags inventory currentScene)

    {- End of Conditions Management -}

    -- Helpers

    getOptions :: [String] ->  String
    getOptions [] = ""
    getOptions (s : []) = s
    getOptions (h : t) | length t == 1 = h ++ " y " ++ getOptions t
                       | otherwise = h ++ ", " ++ getOptions t
           