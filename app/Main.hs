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
import Data.Kind
import Data.List.Split
import Data.Int
import Data.Vector qualified as Vector
import Data.Vector (Vector, (!), (!?))

newtype Pos = Pos Int
              deriving newtype (Eq,Ord,Num,Enum,Integral,Real,Read,Show)

data Chunk =   Tag [Char]
             | Symbol Char
             deriving stock (Eq,Show)


data PState = PState { _psMax  :: !Pos
                     , _psTags :: !(Map (Maybe Char) Pos)
                     }

makeLenses ''PState

data L
data LR

data family WithPos a :: Type

data instance WithPos L  =
  PosL Pos Chunk
  deriving stock (Eq,Show)

data instance WithPos LR =
  PosLR (Pos,Pos) Chunk
  deriving stock (Eq,Show)

data Opts = Opts { _optWinLen  :: !Pos
                 , _optDelay   :: !Int
                 , _optLf      :: !Bool
                 , _optTextMax :: !Int64
                 , _optTimes   :: !(Maybe Int)
                 }

makeLenses ''Opts

instance Monoid PState where
  mempty = PState 0 mempty

instance Semigroup PState where
  (<>) a b = PState (max (view psMax a) (view psMax b))
                    (view psTags a <> view psTags b)


window = 4

readChunks :: [Char] -> [WithPos LR]
readChunks = withRight . withLeft . readChunks'
  where
    readChunks' s = case s of
      [] -> []
      ('\r' : rest) -> Symbol ' ' : readChunks' rest
      ('\n' : rest) -> Symbol ' ' : readChunks' rest
      ('%' : '{' : rest) -> readTag rest
      (c    : rest) -> Symbol c   : readChunks' rest

    readTag :: [Char] -> [Chunk]
    readTag txt =
      let (tag,rest) = L.span (/='}') txt
      in Tag tag : readChunks' (drop 1 rest)

    withLeft :: [Chunk] -> [WithPos L]
    withLeft = zipWith PosL [0..]

    withRight :: [WithPos L] -> [WithPos LR]
    withRight chunks = res
      where
        res = let (_,w) = evalRWS (go (reverse chunks)) () mempty in reverse w

        go :: [WithPos L] -> RWS () [WithPos LR] PState ()
        go = \case
          [] -> pure ()
          ((PosL p sy@(Symbol{})) : rest) -> do
            pmax <- gets (view psMax)
            modify (set psMax (max p pmax))
            tell [PosLR (p,p) sy]
            go rest

          ((PosL p tag@(Tag t)) : rest) -> do
            pmy   <- gets (Map.lookup (headMay t) . view psTags)
            pmax <- gets (view psMax)
            modify $ over psTags (Map.insert (headMay t) (pred p))

            let pe = fromMaybe pmax pmy

            tell [PosLR (p,pe) tag]
            go rest


putChunk :: Opts -> Chunk -> IO ()
putChunk o (Symbol c) = putChar c -- >> threadDelay (view optDelay o)
putChunk _ (Tag x) = do
  putStr "%{" >> putStr x >> putStr "}"


putEnd :: Opts -> IO ()
putEnd o = if view optLf o then
             putChar '\n'
           else
             putChar '\r'

chunksMapL :: [WithPos LR] -> Map Pos (WithPos LR)
chunksMapL chunks = Map.fromList [ (l, e) | e@(PosLR (l,_) _) <- chunks ]


findSymbolL :: Pos -> Map Pos (WithPos LR) -> Maybe (WithPos LR)
findSymbolL p m = snd <$> Map.lookupGE p m

findSymbolLV :: Pos -> Vector (WithPos LR) -> WithPos LR
findSymbolLV p v = v ! fromIntegral p

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "Text scrolling tool for panels"
  <> progDesc "Scrolls text. Respects lemonbar tags for formatting"
  )
  where
    parser :: Parser (IO ())
    parser = do
     win <- option auto (  long "window"
                        <> short 'w'
                        <> metavar "NUMBER"
                        <> help "scroll window size"
                        <> value 16
                        <> showDefault
                        )
     delay <- ceiling . (*1000000) <$> option auto (  long "delay"
                                                   <> short 'd'
                                                   <> metavar "FLOAT"
                                                   <> help "animation delay"
                                                   <> value 0.1
                                                   <> showDefault
                                                   )
     lf <- flag False True (  long "newline"
                           <> short 'n'
                           <> help "print newline"
                           <> showDefault
                           )

     mtext <- option auto (  long "max-text"
                          <> short 'm'
                          <> metavar "NUMBER"
                          <> help "max data size"
                          <> value 4096
                          <> showDefault
                         )

     times <- optional $ option auto (  long "times"
                                     <> short 'k'
                                     <> metavar "NUMBER"
                                     <> help "number of repetitions"
                                     )

     pure $ run ( Opts win delay lf mtext times )


genIndexes :: Int -> Pos -> (Pos,Pos) -> [[Pos]]
genIndexes n size (a,b) = take n $ go positions
  where
    positions = cycle [a..b]

    go [] = []
    go es = x : go (drop 1 es)
      where
        (x,xs) = L.splitAt (fromIntegral size) es


run :: Opts -> IO ()
run o = do
  hSetBuffering stdout NoBuffering
  txt <- Text.take textMax <$> TIO.hGetContents stdin
  let chunks = readChunks (Text.unpack txt)
  let chunksL = chunksMapL chunks
  let chunksTL = chunksMapL [ x | x@(PosLR _ (Tag{}))  <- chunks ]
  let chunksV = Vector.fromList chunks

  when (null chunks) exitSuccess

  -- mapM_ print (Vector.toList chunksV)
  -- error "oops"

  let total = Map.size chunksL
  let cmin = 0
  let cmax = fromIntegral $ max 0 (pred $ Vector.length chunksV)
  let batches = genIndexes total winLen (cmin,cmax)

  let cycles = maybe forever replicateM_ (view optTimes o)

  cycles $ do
    forM_ batches $ \batch -> do
      let start = headDef 0 batch

      let sy = map (`findSymbolLV` chunksV) batch

      let (lt,_,_) = Map.splitLookup start chunksTL
      let otags = [ x | e@(PosLR (_,r) x) <- Map.elems lt, r >= start  ]

      -- forM_ otags $ \e -> do
      --   print (start, e)

      mapM_ (putChunk o) otags
      mapM_ (putChunk o) [ x | PosLR _ x <- sy ]
      putEnd o
      threadDelay delay

  replicateM_  (fromIntegral winLen) (putChar ' ')

  where
    delay = view optDelay o
    winLen = view optWinLen o
    textMax = view optTextMax o


