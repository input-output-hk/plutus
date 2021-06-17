{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations=1 #-}

{-% Last piece puzzle, adapted from nofib/spectral/last-piece.
    This is a solver for a jigsaw problem:
        see https://www.nicklevine.org/contest/2003/index.html.

  I've removed prettyprinting code for solutions and replaced Map.Map with an
  association list.  The original version collected the entire search tree,
  including paths which led to failure, and the PLC version quickly ran out of
  memory. This version prunes the search tree to keep only successful paths.  It
  still doesn't work on the CEK machine (I don't know about the CK machine: that
  took forever).
%-}

module Plutus.Benchmark.LastPiece where

import           Data.Char          (isSpace)
import           PlutusCore.Default
import qualified PlutusCore.Pretty  as PLC
import           PlutusTx           as PlutusTx
import           PlutusTx.Builtins  as Tx
import           PlutusTx.Prelude   as PLC hiding (Semigroup (..), check, foldMap)
import qualified Prelude            as Haskell
import           UntypedPlutusCore

-------------------------------------
--      Pieces
type Offset  = (Integer, Integer)
type Square  = (Integer, Integer)
     -- (1,1) is bottom LH corner

type PieceId = Haskell.Char

type Board = [(Square, PieceId)]  -- Was Map.Map Square PieceId

data Piece = P PieceId
    [[Offset]]  -- Male in bottom LH
    [[Offset]]  -- Female in bottom LH
        -- In both cases, the list of offset is all the
        -- squares except the bottom LH one

data Solution = Soln Board
              | Choose [Solution]       -- Non-empty
              | Fail  -- Board Square
                deriving (Haskell.Show)

data Sex = Male | Female


{-# INLINABLE sumList #-}
sumList :: [Integer] -> Integer
sumList []    = 0
sumList (h:t) = h + sumList t

{-# INLINABLE numSolutions #-}
numSolutions :: Solution -> Integer
numSolutions (Soln _)   = 1
numSolutions (Choose l) = sumList . map numSolutions $ l
numSolutions Fail       = 0

sizeOfSolution :: Solution -> Integer
sizeOfSolution (Soln _)   = 1
sizeOfSolution (Choose l) = sumList . map sizeOfSolution $ l
sizeOfSolution Fail       = 1

{-# INLINABLE flipSex #-}
flipSex :: Sex -> Sex
flipSex Male   = Female
flipSex Female = Male

--      The main search

{-# INLINABLE search #-}
search :: Square -> Sex         -- Square we are up to
       -> Board                 -- Current board
       -> [Piece]               -- Remaining pieces
       -> Solution
search _ _ board []
  = Soln board     -- Finished
search (row,col) sex board ps      -- Next row
  | col == (maxCol+1) = search (row+1, 1) (flipSex sex) board ps
search square sex board ps     -- Occupied square
  | isJust (check board square) = search (next square) (flipSex sex) board ps
search square sex board ps
  = case mapMaybe (try square sex board) choices of
        [] -> Fail     -- board square
        ss -> prune ss -- discard failed paths
    where
      choices = [(pid, os, ps') |
                 (P pid ms fs, ps') <- pickOne ps,
                 let oss = case sex of
                             Male   -> ms
                             Female -> fs,
                 os <- oss]

{-# INLINABLE prune #-}
-- % An attempt to cut down on the size of the result (not in the original program)
prune :: [Solution] -> Solution
prune ss =
    case filter nonFailure ss of
      []       -> Fail
      [Soln s] -> Soln s
      l        -> Choose l
      where nonFailure Fail = False
            nonFailure _    = True

{-# INLINABLE try #-}
try :: Square -> Sex -> Board -> (PieceId,[Offset],[Piece]) -> Maybe Solution
try square sex board (pid,os,ps)
  = case (fit board square pid os) of
        Just board' -> Just (search (next square) (flipSex sex) board' ps)
        Nothing     -> Nothing


{-# INLINABLE fit #-}
fit :: Board -> Square -> PieceId -> [Offset] -> Maybe Board
fit board square pid []     = Just (extend board square pid)
fit board square pid (o:os) =
    case extend_maybe board (square `add` o) pid of
      Just board' -> fit board' square pid os
      Nothing     -> Nothing


--------------------------
--      Offsets and squares

{-# INLINABLE add #-}
add :: Square -> Offset -> Square
add (row,col) (orow, ocol) = (row + orow, col + ocol)

{-# INLINABLE next #-}
next :: Square -> Square
next (row,col) = (row,col+1)

{-# INLINABLE maxRow #-}
{-# INLINABLE maxCol #-}
maxRow,maxCol :: Integer
maxRow = 8
maxCol = 8


------------------------
--      Boards

{-# INLINABLE emptyBoard #-}
emptyBoard :: Board
emptyBoard = [] -- Map.empty

{-# INLINABLE check #-}
check :: Board -> Square -> Maybe PieceId
check board square = -- Map.lookup square board
    case board of
      []                   -> Nothing
      (square',pid):board' -> if square == square' then Just pid else check board' square

{-# INLINABLE extend #-}
extend :: Board -> Square -> PieceId -> Board
extend board square pid = (square, pid): board -- Map.insert square pid board

{-# INLINABLE extend_maybe #-}
extend_maybe :: Board -> Square -> PieceId -> Maybe Board
extend_maybe board square@(row,col) pid
  | row > maxRow || col < 1 || col > maxCol
  = Nothing
  | otherwise
  = case check board square of
        Just _  -> Nothing
        Nothing -> Just (extend board square pid)


--------------------------
--      Utility

{-# INLINABLE pickOne #-}
pickOne :: [a] -> [(a,[a])]
pickOne xs0 = go id xs0
  where
    go _ []     = []
    go f (x:xs) = (x, f xs) : go ((x :) . f) xs


-----------------------------------
--      The initial setup

{-# INLINABLE fromJust #-}
-- % Library functions is not inlinable
fromJust :: Maybe a -> a
fromJust Nothing  = Tx.error ()
fromJust (Just x) = x

{-# INLINABLE initialBoard #-}
initialBoard :: Board
initialBoard = fromJust (fit emptyBoard (1,1) 'a' [(1,0),(1,1)])

{-# INLINABLE initialPieces #-}
initialPieces :: [Piece]
initialPieces = [bPiece, cPiece, dPiece, ePiece, fPiece,
                 gPiece, hPiece, iPiece, jPiece, kPiece,
                 lPiece, mPiece, nPiece]

{-# INLINABLE nPiece #-}
nPiece :: Piece
nPiece = P 'n' [ [(0,1),(1,1),(2,1),(2,2)],
                 [(1,0),(1,-1),(1,-2),(2,-2)] ]
               []

{-# INLINABLE mPiece #-}
mPiece :: Piece
mPiece = P 'm' [ [(0,1),(1,0),(2,0),(3,0)] ]
               [ [(0,1),(0,2),(0,3),(1,3)],
                 [(1,0),(2,0),(3,0),(3,-1)] ]

{-# INLINABLE lPiece #-}
lPiece :: Piece
lPiece = P 'l' [ [(0,1),(0,2),(0,3),(1,2)],
                 [(1,0),(2,0),(3,0),(2,-1)] ]
               [ [(1,-1),(1,0),(1,1),(1,2)],
                 [(1,0),(2,0),(3,0),(1,1)] ]

{-# INLINABLE kPiece #-}
kPiece :: Piece
kPiece = P 'k' [ [(0,1),(1,0),(2,0),(2,-1)] ]
               [ [(1,0),(1,1),(1,2),(2,2)] ]


{-# INLINABLE jPiece #-}
jPiece :: Piece
jPiece = P 'j' [ [(0,1),(0,2),(0,3),(1,1)],
                 [(1,0),(2,0),(3,0),(1,-1)],
                 [(1,-2),(1,-1),(1,0),(1,1)] ]
               [ [(1,0),(2,0),(3,0),(2,2)] ]

{-# INLINABLE iPiece #-}
iPiece :: Piece
iPiece = P 'i' [ [(1,0),(2,0),(2,1),(3,1)],
                 [(0,1),(0,2),(1,0),(1,-1)],
                 [(1,0),(1,1),(2,1),(3,1)] ]
               [ [(0,1),(1,0),(1,-1),(1,-2)] ]

{-# INLINABLE hPiece #-}
hPiece :: Piece
hPiece = P 'h' [ [(0,1),(1,1),(1,2),(2,2)],
                 [(1,0),(1,-1),(2,-1),(2,-2)],
                 [(1,0),(1,1),(2,1),(2,2)] ]
               [ [(0,1),(1,0),(1,-1),(2,-1)] ]


{-# INLINABLE gPiece #-}
gPiece :: Piece
gPiece = P 'g' [ ]
               [ [(0,1),(1,1),(1,2),(1,3)],
                 [(1,0),(1,-1),(2,-1),(3,-1)],
                 [(0,1),(0,2),(1,2),(1,3)],
                 [(1,0),(2,0),(2,-1),(3,-1)] ]

{-# INLINABLE fPiece #-}
fPiece :: Piece
fPiece = P 'f' [ [(0,1),(1,1),(2,1),(3,1)],
                 [(1,0),(1,-1),(1,-2),(1,-3)],
                 [(1,0),(2,0),(3,0),(3,1)] ]
               [ [(0,1),(0,2),(0,3),(1,0)] ]


{-# INLINABLE ePiece #-}
ePiece :: Piece
ePiece = P 'e' [ [(0,1),(1,1),(1,2)],
                 [(1,0),(1,-1),(2,-1)] ]
               [ [(0,1),(1,1),(1,2)],
                 [(1,0),(1,-1),(2,-1)] ]

{-# INLINABLE dPiece #-}
dPiece :: Piece
dPiece = P 'd' [ [(0,1),(1,1),(2,1)],
                 [(1,0),(1,-1),(1,-2)] ]
               [ [(1,0),(2,0),(2,1)] ]


{-# INLINABLE cPiece #-}
cPiece :: Piece
cPiece = P 'c' [ ]
               [ [(0,1),(0,2),(1,1)],
                 [(1,0),(1,-1),(2,0)],
                 [(1,-1),(1,0),(1,1)],
                 [(1,0),(1,1),(2,0)] ]

{-# INLINABLE bPiece #-}
bPiece :: Piece
bPiece = P 'b'  [ [(0,1),(0,2),(1,2)],
                  [(1,0),(2,0),(2,-1)],
                  [(0,1),(1,0),(2,0)] ]
                [ [(1,0),(1,1),(1,2)] ]

unindent :: PLC.Doc ann -> [Haskell.String]
unindent d = map (Haskell.dropWhile isSpace) $ (Haskell.lines . Haskell.show $ d)

runLastPiece :: Solution
runLastPiece = search (1,2) Female initialBoard initialPieces

mkLastPieceTerm :: Term NamedDeBruijn DefaultUni DefaultFun ()
mkLastPieceTerm =
  let (Program _ _ code) = getPlc $ $$(compile [|| runLastPiece ||])
  in code

-- -- Number of correct solutions: 3
-- -- Number including failures: 59491
