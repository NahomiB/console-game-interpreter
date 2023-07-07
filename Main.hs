import Data.Char (toLower)
import Data.List (intercalate, nub)
import qualified Data.Map as Map
import Text.Printf
import System.Exit (exitFailure)
import System.IO (stdout, hFlush)

import Lexer (lexInput, Token(..), isKeyword, Command(..))
import Parser (parseSentence, parseToCommands, parseToSingleCommand)
import SpaceManager (Flag(..), Inventory(..), Scene(..), Settings(..), chooseStartScene, performInteraction, filterInteraction, getOptions, performActions, filterActions)
import StoryManager (availableTokens, availableKeywords, initialInventory, initialFlags, settings, scenes)


initializeInterpeter :: IO Scene
initializeInterpeter = do 
            putStrLn "Interprete de Juegos de Misterio\nNahomi Bouza - Anet Rodriguez\n"
            putStrLn (printf "Bienvenido a %s!\n" (gameName settings))
            if not (validateGame scenes initialFlags)
                then do
                    putStrLn "The game is not defined properly. Contact the developer.\n"
                    exitFailure
                else do return()
            if length possibleStarts == 0
                then do
                    putStrLn "There are no possible starting scenes. Contact the developer.\n"
                    exitFailure
                else if length possibleStarts == 1
                    then return (head possibleStarts)
                    else (chooseStartScene possibleStarts (quitMessage settings))
            where 
                possibleStarts = filter (\scene -> canBeStartPoint scene) scenes

validateGame :: [Scene] -> Flag -> Bool
validateGame allScenes (Flag flags) = length names == length (nub names) &&  names == (Map.keys flags)
                where
                    names = (map (\scene -> (name scene)) allScenes)

winGame :: Flag -> Scene -> Bool
winGame (Flag flags) currentScene = case Map.lookup (name currentScene) flags of
  Just sceneFlags -> (finishFlag settings) `elem` sceneFlags
  Nothing -> False

initializeGame :: Scene -> [Scene] -> Inventory -> Flag -> IO()
initializeGame firstScene scenes initialInventory initialFlags = 
    gameIteration description False newCurrentScene scenes newInventory newFlags >>
    return()
    where 
        (description, states) = filterActions (start firstScene) initialFlags initialInventory firstScene
        (newFlags, newInventory, newCurrentScene) = performActions states initialFlags initialInventory firstScene scenes

gameIteration :: String -> Bool -> Scene -> [Scene] -> Inventory -> Flag -> IO (String, Bool)
gameIteration line exit scene allScenes inventory flags =
    putStrLn ("\n" ++ line) >>
    if exit 
        then return("exit", True)
        else return() >>
    if winGame flags scene
        then return("exit", True)
        else return() >>
    putStr ("\n" ++ (defaultPhrase settings)) >>
    getLine >>= \input -> 
        let commands = (processInputLine input)
        in if length commands == 1 && isKeyword (head commands) availableKeywords
                then 
                    let (newDescription, newExit) = (executeKeyword (head commands) inventory)
                    in (gameIteration newDescription newExit scene allScenes inventory flags)
                else 
                    let (description, newFlags, newInventory, newScene) = performInteraction (filterInteraction scene commands) flags inventory scene allScenes
                    in (gameIteration description False newScene allScenes newInventory newFlags)

processInputLine :: String -> [Command]
processInputLine line | (dynamic settings) = concatMap (\sentence -> parseToCommands sentence) (parseSentence (lexInput availableTokens (words (filter (/= ',') line))))
                      | otherwise = map (\sentence -> parseToSingleCommand sentence) (parseSentence (lexInput availableTokens (words (filter (/= ',') line)))) 

executeKeyword :: Command -> Inventory -> (String, Bool)
executeKeyword (Command (h:[])) (Inventory inventory) = 
    let keyword = (map toLower h)
    in case keyword of
        "inventario" -> if length inventory == 0
                            then ("Aun no tienes articulos. A medida que exploras tu entorno podras encontrar algunos articulos. Seran utiles mas adelante.", False)
                            else ("Dispones de los siguientes elementos en tu inventario: " ++ (getOptions inventory), False)
        "ayuda" -> ((help settings), False)
        "salir" -> ((quitMessage settings), True)
        _ -> ((defaultErrorPhrase settings), False)

play :: IO()
play =  do
    firstScene <- initializeInterpeter
    if (name firstScene) == "salir"
        then return()
        else initializeGame firstScene scenes initialInventory initialFlags

