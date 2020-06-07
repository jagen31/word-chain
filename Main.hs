{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import Control.Lens
import Control.Monad.State.Lazy
import Control.Monad.Except
import Data.Set as Set
import Data.Maybe
import Data.Either

nOT_A_WORD = "That's not a word!"
aLREADY_USED = "You've already used that word!"
wRONG_LETTER = "That doesn't start with the right letter!"

data WordState = WordState { _dictionary :: Set String, 
                             _usedWords :: Set String, 
                             _previousWord :: Maybe String }
  deriving (Show, Eq)
makeLenses ''WordState

data PlayerState = PlayerState { _players :: [String] }

makeLenses ''PlayerState

data GlobalState = GlobalState {
  _wordState :: WordState,
  _playerState :: PlayerState
}
makeLenses ''GlobalState

data GlobalError = NotAWord
               | AlreadyUsed
               | WrongLetter String String
               | NotYourTurn
  deriving (Show, Eq)

canGo :: String -> State PlayerState Bool
canGo player = uses players head >>= (return . (== player))

afterTurn :: State PlayerState ()
afterTurn = players %= tail

takeTurn :: String -> StateT WordState (Except GlobalError) ()
takeTurn word = do 
  usedWords' <- use usedWords
  dictionary' <- use dictionary
  previousWord' <- use previousWord
  if | notMatches previousWord' -> throwError $ WrongLetter (fromJust previousWord') word
     | Set.member word usedWords' -> throwError AlreadyUsed
     | not $ Set.member word $ dictionary' -> throwError NotAWord
     | otherwise -> do previousWord .= (Just word)
                       usedWords .= Set.insert word usedWords'
  where
    notMatches (Nothing) = False
    notMatches (Just prev) = not $ last prev == head word

dict = Set.fromList ["hello", "open", "nice"]
initial = GlobalState (WordState dict empty Nothing) (PlayerState players)
  where players = "Me":"Myself":"I":players

tryTurn :: String -> String -> StateT GlobalState (Except GlobalError) ()
tryTurn player word = do 
  go <- zoom playerState $ mapStateT lift $ canGo player
  zoom wordState $ if go then takeTurn word else throwError NotYourTurn
  zoom playerState $ mapStateT lift afterTurn

main :: IO ()
main = putStrLn $ show $ _wordState . snd . (fromRight ((), initial)) . runExcept . (`runStateT` initial) $ 
  tryTurn "Me" "hello" *> tryTurn "Myself" "open" *> tryTurn "I" "nice"
