module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.RWS
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text.Lazy.IO qualified as TIO
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy (Text)
import Options.Applicative
import System.IO
import Lens.Micro.TH
import Lens.Micro.Platform
import Safe
import Data.Maybe
import System.Exit

newtype Pos = Pos Int
              deriving newtype (Eq,Ord,Num,Enum,Show)

data Chunk =   Tag (Maybe Pos) [Char]
             | Symbol (Maybe Pos) Char
             deriving stock (Eq,Show)


data PState = PState { _psPos  :: Maybe Pos
                     , _psMax  :: Maybe Pos
                     , _psTags :: Map (Maybe Char) Pos
                     }


makeLenses ''PState

instance Monoid PState where
  mempty = PState Nothing Nothing mempty

instance Semigroup PState where
  (<>) a b = PState (max (view psPos a) (view psPos b))
                    (max (view psPos a) (view psPos b))
                    (view psTags a <> view psTags b)


delay :: Int
delay = 0

textMax = 4096

window = 4

readChunks :: [Char] -> [Chunk]
readChunks s =
  case s of
    [] -> []
    ('\r' : rest) -> Symbol Nothing ' ' : readChunks rest
    ('\n' : rest) -> Symbol Nothing ' ' : readChunks rest
    ('%' : '{' : rest) -> readTag rest
    (c    : rest) -> Symbol Nothing c   : readChunks rest


  where
    readTag :: [Char] -> [Chunk]
    readTag txt =
      let (tag,rest) = L.span (/='}') txt
      in Tag Nothing tag : readChunks (drop 1 rest)


updatePositions :: [Chunk] -> [Chunk]
updatePositions x = updateTags updateSymbols
    where

    updateTags :: [Chunk] -> [Chunk]
    updateTags chunks = res
      where
        res = let (_,w) = evalRWS (go (reverse chunks)) () mempty in reverse w

        go :: [Chunk] -> RWS () [Chunk] PState ()
        go  = \case
          [] -> pure ()

          (sy@(Symbol p _) : rest) -> do
            modify (set psPos p)
            pmax <- gets (view psMax)
            modify (set psMax (max p pmax))
            tell [sy]
            go rest

          ( Tag _ t : rest ) -> do
            pmax <- gets (view psMax)
            pcurr <- gets (view psPos)
            pmy   <- gets (Map.lookup (headMay t) . view psTags)

            case pcurr of
              Nothing -> pure ()
              Just p  -> modify $ over psTags (Map.insert (headMay t) (pred p))

            let tagPos0 = pmy <|> pmax
            tell [Tag tagPos0 t]
            go rest

    updateSymbols = let (_,w) = evalRWS (go x) () 0 in w
      where
        go :: [Chunk] -> RWS () [Chunk] Pos ()
        go chunks = case chunks of
          [] -> pure ()
          ( Symbol _ c  : rest ) -> do
            pos <- get
            modify succ
            tell [Symbol (Just pos) c]
            go rest

          ( t : rest ) -> do
            tell [t]
            go rest


getPos :: Chunk -> Maybe Pos
getPos = \case
  Symbol p _ -> p
  Tag p _    -> p


putChunks :: [Chunk] -> Char -> IO ()
putChunks xs end = mapM_ putChunk xs >> putChar end
  where
    putChunk (Symbol _ c) = putChar c >> threadDelay delay
    putChunk  (Tag _ x) = do
      putStr "%{" >> putStr x >> putStr "}"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  txt <- Text.take textMax <$> TIO.hGetContents stdin
  let chunks = updatePositions $ readChunks (Text.unpack txt)

  let chuchu = [ (fromJust (getPos x), [x]) | x <- chunks, isJust (getPos x) ]

  let chuMap = Map.fromListWith (<>) chuchu

  when (Map.null chuMap) exitSuccess

  forM_ [0 .. fst (Map.findMax chuMap)] $ \i -> do
    print (i, i+window)
    -- mapM_ (putVisible (i,i+window)) chuchu
    -- let visible = [head c | (p,c) <- chuchu, p >= i, p <= i+window]
    -- putChunks visible '\n'

