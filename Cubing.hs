{----------------------------------------------------------------

  A Rubik's Cube Solver

  Copyright (C) 2018 Hideyuki Kawabata

  This solver is based on the CFOP method without F2L.

  Usage:

  - using GHCI:
   (a) input: a scramble
    *Cubing> solve_check "R  L'  U'  D2  F2  U'  F'  R  D'  U'  B2"
    ...

   (b) input: a scrambled pattern
    *Cubing> solve_check_pat "OBOWBWYW, BRWYBOWW, ROROWRYG, BBYBGORY, GROBGGOY, RRYGGYWG"
    ...

    Note: the above input string corresponds to the following pattern:
      (based on the Western color scheme)

           YWB                            765
           WWW                            8W4
           OBO                            123
       OGG WOB YRW ROG                765 765 765 765
       YGB WRY GBO YOB       <--->    8G4 8R4 8B4 8O4
       GRO BRW ROR BBY                123 123 123 123
           WYG                            765
           GYG                            8Y4
           RRY                            123

  - stand alone execution:
   (a)
    $ ./Cubing
    Input a scramble:
    R  L'  U'  D2  F2  U'  F'  R  D'  U'  B2
    ...

   or

   (b)
    $ ./Cubing 
    Input a scrambled pattern:
    OBOWBWYW, BRWYBOWW, ROROWRYG, BBYBGORY, GROBGGOY, RRYGGYWG
    ...

-----------------------------------------------------------------}

{-# LANGUAGE ScopedTypeVariables #-}

--module Cubing where

import Data.List.Split
import Control.Exception
data CouldNotSolve = CouldNotSolve String
  deriving (Show)
instance Exception CouldNotSolve


data Color = Red | White | Green | Blue | Orange | Yellow deriving (Eq)
instance Show Color where
  show Red = "R"
  show White = "W"
  show Green = "G"
  show Blue = "B"
  show Orange = "O"
  show Yellow = "Y"

data Op = R | R' | U | U' | B | B' | L | L' | F | F' | D | D' 
        | Y | Y' | Z | Z' | N | M | M'
        | Y2 | Y'2 | Z2 | Z'2 | R2 | R'2 | U2 | U'2 | B2 | B'2
        | L2 | L'2 | F2 | F'2 | D2 | D'2
        --
        | FstLayer | SndLayer | PLL1p 
        | PLL21p | PLL22p | PLL23p | PLL24p | PLL25p | PLL26p
        | A2p | A1p | Tp | U1p | U2p | Yp | R2p | Zp | R1p
        | Hp | G2p | J2p | G4p | J1p | G1p | G3p | Fp | Vp | N2p | N1p | Ep
        deriving (Show, Eq, Read)

type Surface = (Color, Color, Color, Color, Color, Color, Color, Color)
data Q = Q { w :: Surface
           , r :: Surface
           , b :: Surface
           , o :: Surface
           , g :: Surface
           , y :: Surface
           } deriving (Show, Eq)
{-
     WWW                            765
     WWW                            8W4
     WWW                            123
 GGG RRR BBB OOO                765 765 765 765
 GGG RRR BBB OOO       <--->    8G4 8R4 8B4 8O4
 GGG RRR BBB OOO                123 123 123 123
     YYY                            765
     YYY                            8Y4
     YYY                            123
-}


-- goal configuration
goal :: Q
goal = Q { w = (White, White, White, White, White, White, White, White)
         , r = (Red, Red, Red, Red, Red, Red, Red, Red)
         , b = (Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue)
         , o = (Orange, Orange, Orange, Orange, Orange, Orange, Orange, Orange)
         , g = (Green, Green, Green, Green, Green, Green, Green, Green)
         , y = (Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow)
         }

-- simple parser
fromString :: String -> [Op] -> [Op]
fromString [] acc = reverse acc
fromString (x:xs) acc
  | x == ' ' = fromString xs acc
  | x == ',' = fromString xs acc
  | elem x "RUBLFDYZM" = 
    if length xs >= 1 then
      if xs0 == '\'' && length xs >= 2 && xs1 == '2' then -- eg. U'2
        fromString (drop 2 xs) (read [x, xs0, xs1] : acc)
      else if xs0 == '\'' || xs0 == '2' then -- eg. U', U2
        fromString (drop 1 xs) (read [x, xs0] : acc)
      else fromString xs (read [x] : acc) -- eg. U
    else reverse (read [x] : acc) -- length xs == 0
  | otherwise = error "fromString"
  where xs0 = xs!!0
        xs1 = xs!!1

-- parser for configuration data
-- eg. "WWWWWWWW, RRRRRRRR, BBBBBBBB, OOOOOOOO, GGGGGGGG, YYYYYYYY"
fromStringPos :: String -> Q
fromStringPos str = Q { w = l2t sw
                      , r = l2t sr
                      , b = l2t sb
                      , o = l2t so
                      , g = l2t sg
                      , y = l2t sy
                      }
  where surlist = splitOn "," $ filter (\c -> c /= ' ') str
        collist = map (map charToColor) surlist
        sw = collist!!0
        sr = collist!!1
        sb = collist!!2
        so = collist!!3
        sg = collist!!4
        sy = collist!!5
        l2t l = (l!!0, l!!1, l!!2, l!!3, l!!4, l!!5, l!!6, l!!7)

charToColor :: Char -> Color
charToColor ch
  | ch == 'W' = White
  | ch == 'R' = Red
  | ch == 'B' = Blue
  | ch == 'O' = Orange
  | ch == 'G' = Green
  | ch == 'Y' = Yellow
  | otherwise = error $ "charToColor : " ++ [ch]


-- pretty printer
pr :: Q -> IO ()
pr q = do
  let cwq = sel $ w q
  let cgq = sel $ g q
  let crq = sel $ r q
  let cbq = sel $ b q
  let coq = sel $ o q
  let cyq = sel $ y q
  putStrLn $ "    " ++ show (cwq 7) ++ show (cwq 6) ++ show (cwq 5)
  putStrLn $ "    " ++ show (cwq 8) ++ show White ++ show (cwq 4)
  putStrLn $ "    " ++ show (cwq 1) ++ show (cwq 2) ++ show (cwq 3)
  putStrLn $ show (cgq 7) ++ show (cgq 6) ++ show (cgq 5) ++ " "
    ++ show (crq 7) ++ show (crq 6) ++ show (crq 5) ++ " "
    ++ show (cbq 7) ++ show (cbq 6) ++ show (cbq 5) ++ " "
    ++ show (coq 7) ++ show (coq 6) ++ show (coq 5)
  putStrLn $ show (cgq 8) ++ show Green ++ show (cgq 4) ++ " "
    ++ show (crq 8) ++ show Red ++ show (crq 4) ++ " "
    ++ show (cbq 8) ++ show Blue ++ show (cbq 4) ++ " "
    ++ show (coq 8) ++ show Orange ++ show (coq 4)
  putStrLn $ show (cgq 1) ++ show (cgq 2) ++ show (cgq 3) ++ " "
    ++ show (crq 1) ++ show (crq 2) ++ show (crq 3) ++ " "
    ++ show (cbq 1) ++ show (cbq 2) ++ show (cbq 3) ++ " "
    ++ show (coq 1) ++ show (coq 2) ++ show (coq 3)
  putStrLn $ "    " ++ show (cyq 7) ++ show (cyq 6) ++ show (cyq 5)
  putStrLn $ "    " ++ show (cyq 8) ++ show Yellow ++ show (cyq 4)
  putStrLn $ "    " ++ show (cyq 1) ++ show (cyq 2) ++ show (cyq 3)

prSeq :: [Op] -> String
prSeq [] = ""
prSeq (x:xs)
  | x == FstLayer = "First Layer -----\n " ++ prSeq xs
  | x == SndLayer = "\nSecond Layer -----\n " ++ prSeq xs
  | x == PLL1p
  || x == PLL21p 
  || x == PLL22p
  || x == PLL23p 
  || x == PLL24p
  || x == PLL25p 
  || x == PLL26p = "\nPLL -----\n " ++ prSeq xs
  | x == A2p 
  || x == A1p 
  || x == Tp 
  || x == U1p 
  || x == U2p
  || x == Yp
  || x == R2p
  || x == Zp 
  || x == R1p
  || x == Hp 
  || x == G2p
  || x == J2p
  || x == G4p
  || x == J1p
  || x == G1p
  || x == G3p
  || x == Fp
  || x == Vp 
  || x == N2p
  || x == N1p
  || x == Ep = "\nOLL -----\n " ++ prSeq xs
  | otherwise = show x ++ " " ++ prSeq xs


-- helper functions
sel :: Surface -> Int -> Color
sel (c1, c2, c3, c4, c5, c6, c7, c8) 1 = c1
sel (c1, c2, c3, c4, c5, c6, c7, c8) 2 = c2
sel (c1, c2, c3, c4, c5, c6, c7, c8) 3 = c3
sel (c1, c2, c3, c4, c5, c6, c7, c8) 4 = c4
sel (c1, c2, c3, c4, c5, c6, c7, c8) 5 = c5
sel (c1, c2, c3, c4, c5, c6, c7, c8) 6 = c6
sel (c1, c2, c3, c4, c5, c6, c7, c8) 7 = c7
sel (c1, c2, c3, c4, c5, c6, c7, c8) 8 = c8

rot :: Bool -> Surface ->  Surface
rot True (c1, c2, c3, c4, c5, c6, c7, c8) = (c7, c8, c1, c2, c3, c4, c5, c6)
rot False (c1, c2, c3, c4, c5, c6, c7, c8) = (c3, c4, c5, c6, c7, c8, c1, c2)

-- operations
turn :: Op -> Q -> Q
turn R q = Q { w = (cwq 1, cwq 2, crq 3, crq 4, crq 5, cwq 6, cwq 7, cwq 8)
             , r = (crq 1, crq 2, cyq 3, cyq 4, cyq 5, crq 6, crq 7, crq 8)
             , b = (cbq 3, cbq 4, cbq 5, cbq 6, cbq 7, cbq 8, cbq 1, cbq 2)
             , o = (cwq 5, coq 2, coq 3, coq 4, coq 5, coq 6, cwq 3, cwq 4)
             , g = g q
             , y = (cyq 1, cyq 2, coq 7, coq 8, coq 1, cyq 6, cyq 7, cyq 8)
             }
  where cwq = sel $ w q
        crq = sel $ r q
        cbq = sel $ b q
        coq = sel $ o q
        cgq = sel $ g q
        cyq = sel $ y q
turn Y q = Q { w = rot False $ w q
             , r = b q
             , b = o q
             , o = g q
             , g = r q
             , y = rot True $ y q
             }
turn Z q = Q { w = rot False $ g q
             , r = rot False $ r q
             , b = rot False $ w q
             , o = rot True $ o q
             , g = rot False $ y q
             , y = rot False $ b q
             }
turn op q = case op of
  R' -> applySeq [R, R, R] q
  R2 -> applySeq [R, R] q
  Y' -> applySeq [Y, Y, Y] q
  Z2 -> applySeq [Z, Z] q
  Z' -> applySeq [Z, Z, Z] q
  L -> applySeq [Y, Y, R, Y, Y] q
  L2 -> applySeq [L, L] q
  L' -> applySeq [Y, Y, R', Y, Y] q
  U -> applySeq [Z, R, Z'] q
  U2 -> applySeq [U, U] q
  U' -> applySeq [Z, R', Z'] q
  D -> applySeq [Z', R, Z] q
  D2 -> applySeq [D, D] q
  D' -> applySeq [Z', R', Z] q
  F -> applySeq [Y', R, Y] q
  F2 -> applySeq [F, F] q
  F' -> applySeq [Y', R', Y] q
  B -> applySeq [Y, R, Y'] q
  B2 -> applySeq [B, B] q
  B' -> applySeq [Y, R', Y'] q
  Y2 -> applySeq [Y, Y] q
  Y'2 -> applySeq [Y', Y'] q
  Z'2 -> applySeq [Z', Z'] q
  U'2 -> applySeq [U', U'] q
  B'2 -> applySeq [B', B'] q
  D'2 -> applySeq [D', D'] q
  L'2 -> applySeq [L', L'] q
  R'2 -> applySeq [R', R'] q
  F'2 -> applySeq [F', F'] q
  M -> applySeq [Z, D', U, Y', Z'] q
  _ -> q

applySeq :: [Op] -> Q -> Q
applySeq l q = foldl (\r op -> turn op r) q l


-- simple optimizations
expandOp :: [Op] -> [Op]
expandOp [] = []
expandOp (x:xs) = case x of
  R2 -> R:R:(expandOp xs)
  L2 -> L:L:(expandOp xs)
  D2 -> D:D:(expandOp xs)
  U2 -> U:U:(expandOp xs)
  B2 -> B:B:(expandOp xs)
  F2 -> F:F:(expandOp xs)
  Y2 -> Y:Y:(expandOp xs)
  Z2 -> Z:Z:(expandOp xs)
  R'2 -> R:R:(expandOp xs)
  L'2 -> L:L:(expandOp xs)
  D'2 -> D:D:(expandOp xs)
  U'2 -> U:U:(expandOp xs)
  B'2 -> B:B:(expandOp xs)
  F'2 -> F:F:(expandOp xs)
  Y'2 -> Y:Y:(expandOp xs)
  Z'2 -> Z:Z:(expandOp xs)
  _ -> x:(expandOp xs)

exchangeOp :: [Op] -> [Op]
exchangeOp [] = []
exchangeOp l@(x:[]) = l
-- Y
exchangeOp (Y:U:xs) = exchangeOp (U:Y:xs)
exchangeOp (Y:U':xs) = exchangeOp (U':Y:xs)
exchangeOp (Y':U:xs) = exchangeOp (U:Y':xs)
exchangeOp (Y':U':xs) = exchangeOp (U':Y':xs)
exchangeOp (Y:D:xs) = exchangeOp (D:Y:xs)
exchangeOp (Y:D':xs) = exchangeOp (D':Y:xs)
exchangeOp (Y':D:xs) = exchangeOp (D:Y':xs)
exchangeOp (Y':D':xs) = exchangeOp (D':Y':xs)
-- D
exchangeOp (D:U:xs) = exchangeOp (U:D:xs)
exchangeOp (D:U':xs) = exchangeOp (U':D:xs)
exchangeOp (D':U:xs) = exchangeOp (U:D':xs)
exchangeOp (D':U':xs) = exchangeOp (U':D':xs)
-- B
exchangeOp (B:F:xs) = exchangeOp (F:B:xs)
exchangeOp (B:F':xs) = exchangeOp (F':B:xs)
exchangeOp (B':F:xs) = exchangeOp (F:B':xs)
exchangeOp (B':F':xs) = exchangeOp (F':B':xs)
-- L
exchangeOp (L:R:xs) = exchangeOp (R:L:xs)
exchangeOp (L:R':xs) = exchangeOp (R':L:xs)
exchangeOp (L':R:xs) = exchangeOp (R:L':xs)
exchangeOp (L':R':xs) = exchangeOp (R':L':xs)
--
exchangeOp (x:xs) = x:exchangeOp xs

revOp :: Op -> Op
revOp U = U'
revOp U' = U
revOp L = L'
revOp L' = L
revOp B = B'
revOp B' = B
revOp D = D'
revOp D' = D
revOp F = F'
revOp F' = F
revOp R = R'
revOp R' = R
revOp Y = Y'
revOp Y' = Y

reduceOp :: [Op] -> [Op]
reduceOp [] = []
reduceOp l@(x:[]) = l
reduceOp (U:U':ys) = reduceOp ys
reduceOp (L:L':ys) = reduceOp ys
reduceOp (R:R':ys) = reduceOp ys
reduceOp (B:B':ys) = reduceOp ys
reduceOp (D:D':ys) = reduceOp ys
reduceOp (F:F':ys) = reduceOp ys
reduceOp (Y:Y':ys) = reduceOp ys
reduceOp (Z:Z':ys) = reduceOp ys
reduceOp (U':U:ys) = reduceOp ys
reduceOp (L':L:ys) = reduceOp ys
reduceOp (R':R:ys) = reduceOp ys
reduceOp (B':B:ys) = reduceOp ys
reduceOp (D':D:ys) = reduceOp ys
reduceOp (F':F:ys) = reduceOp ys
reduceOp (Y':Y:ys) = reduceOp ys
reduceOp (Z':Z:ys) = reduceOp ys
reduceOp (a:b:c:d:ys) 
  | a == b && b == c && c == d = reduceOp ys
  | a == b && b == c = reduceOp ((revOp a):(d:ys))
  | otherwise = a:(reduceOp (b:c:d:ys))
reduceOp (a:b:c:[]) 
  | a == b && b == c = [revOp a]
  | otherwise = a:(reduceOp (b:c:[]))
reduceOp l = l

iterOpt :: ([Op] -> [Op]) -> [Op] -> [Op]
iterOpt f l
  | l == l' = l'
  | otherwise = f l'
  where l' = f l

mergeOp :: [Op] -> [Op]
mergeOp [] = []
mergeOp l@(x:[]) = l
mergeOp (U:U:ys) = U2:mergeOp ys
mergeOp (B:B:ys) = B2:mergeOp ys
mergeOp (F:F:ys) = F2:mergeOp ys
mergeOp (D:D:ys) = D2:mergeOp ys
mergeOp (L:L:ys) = L2:mergeOp ys
mergeOp (R:R:ys) = R2:mergeOp ys
mergeOp (Y:Y:ys) = Y2:mergeOp ys
mergeOp (Z:Z:ys) = Z2:mergeOp ys
mergeOp (U':U':ys) = U2:mergeOp ys
mergeOp (B':B':ys) = B2:mergeOp ys
mergeOp (F':F':ys) = F2:mergeOp ys
mergeOp (D':D':ys) = D2:mergeOp ys
mergeOp (L':L':ys) = L2:mergeOp ys
mergeOp (R':R':ys) = R2:mergeOp ys
mergeOp (Y':Y':ys) = Y2:mergeOp ys
mergeOp (Z':Z':ys) = Z2:mergeOp ys
mergeOp (y:ys) = y:mergeOp ys

optimizeOp :: [Op] -> [Op]
optimizeOp l = mergeOp $ iterOpt reduceOp $ iterOpt exchangeOp $ expandOp l


-- solver functions
solveQ :: Q -> [Op]
solveQ q = optimizeOp $ snd $
           (q, [FstLayer]) `step`
           (setRY N) `step`
           (\q -> [Y] ++ setRY Y (turn Y q) ++ [Y']) `step`
           (\q -> [Y2] ++ setRY Y2 (turn Y2 q) ++ [Y2]) `step`
           (\q -> [Y'] ++ setRY Y' (turn Y' q) ++ [Y]) `step`
           (\q -> [SndLayer]) `step`
           (\q -> setYGR N q) `step`
           (\q -> [Y] ++ setYGR Y (turn Y q) ++ [Y']) `step`
           (\q -> [Y2] ++ setYGR Y2 (turn Y2 q) ++ [Y2]) `step`
           (\q -> [Y'] ++ setYGR Y' (turn Y' q) ++ [Y]) `step`
           (\q -> setGR N q) `step`
           (\q -> [Y] ++ setGR Y (turn Y q) ++ [Y']) `step`
           (\q -> [Y2] ++ setGR Y2 (turn Y2 q) ++ [Y2]) `step`
           (\q -> [Y'] ++ setGR Y' (turn Y' q) ++ [Y]) `step`
           (\q -> oneToThree q) `step`
           (\q -> threeToFive q) `step`
           (\q -> fiveToNine q) `step`
           (\q -> [Y] ++ fiveToNine (turn Y q) ++ [Y']) `step`
           (\q -> [Y2] ++ fiveToNine (turn Y2 q) ++ [Y2]) `step`
           (\q -> [Y'] ++ fiveToNine (turn Y' q) ++ [Y]) `step`
           (\q -> nineToFinish q) `step`
           (\q -> [Y] ++ nineToFinish (turn Y q) ++ [Y']) `step`
           (\q -> [Y2] ++ nineToFinish (turn Y2 q) ++ [Y2]) `step`
           (\q -> [Y'] ++ nineToFinish (turn Y' q) ++ [Y]) `step`
           (\q -> finishQ q)
--           `step` (\q -> [])

step :: (Q, [Op]) -> (Q -> [Op]) -> (Q, [Op])
step (q, ops) slvr = 
  let ops' = slvr q in
    let q' = applySeq ops' q in
      (q', ops ++ ops')

finishQ :: Q -> [Op]
finishQ q
  | q == goal = []
  | turn U q == goal = [U]
  | turn U2 q == goal = [U2]
  | turn U' q == goal = [U']
  | otherwise = throw $ CouldNotSolve "finishQ"


rotc :: Op -> Color -> Color
rotc N c = c
rotc Y Red = Blue
rotc Y Blue = Orange
rotc Y Orange = Green
rotc Y Green = Red
rotc Y c = c
rotc Y2 c = rotc Y $ rotc Y c
rotc Y' c = rotc Y $ rotc Y2 c

setRY :: Op -> Q -> [Op]
setRY tc q
  | sr 2 == red && sy 6 == yellow = []
--
  | sr 2 == yellow && sy 6 == red = [F', D, R', D']
  | sr 4 == yellow && sb 8 == red = [D, R', D']
  | sr 4 == red && sb 8 == yellow = [F]
  | sr 6 == red && sw 2 == yellow = [F, F]
  | sr 6 == yellow && sw 2 == red = [U', R', F, R]
  | sr 8 == yellow && sg 4 == red = [D', L, D]
  | sr 8 == red && sg 4 == yellow = [F']
--
  | sb 2 == red && sy 4 == yellow = [R, D, R', D']
  | sb 2 == yellow && sy 4 == red = [R, F]
  | sb 4 == yellow && so 8 == red = [B, U, U, B', F, F]
  | sb 4 == red && so 8 == yellow = [R', U, R, F, F]
  | sb 6 == red && sw 4 == yellow = [U, F, F]
  | sb 6 == yellow && sw 4 == red = [R', F, R]
--
  | so 2 == red && sy 2 == yellow = [B, B, U, U, F, F]
  | so 2 == yellow && sy 2 == red = [B, B, U, R', F, R]
  | so 4 == yellow && sg 8 == red = [L, U', L', F, F]
  | so 4 == red && sg 8 == yellow = [B', U', B, U', F, F]
  | so 6 == red && sw 6 == yellow = [U, U, F, F]
  | so 6 == yellow && sw 6 == red = [U', L, F', L']
--
  | sg 2 == red && sy 8 == yellow = [L', D', L, D]
  | sg 2 == yellow && sy 8 == red = [L', F']
  | sg 6 == red && sw 8 == yellow = [U', F, F]
  | sg 6 == yellow && sw 8 == red = [L, F', L']
  | otherwise = throw $ CouldNotSolve "setRY"
  where sr = sel (r q)
        sy = sel (y q)
        sb = sel (b q)
        sw = sel (w q)
        sg = sel (g q)
        so = sel (o q)
        yellow = rotc tc Yellow
        red = rotc tc Red


setYGR :: Op -> Q -> [Op]
setYGR tc q
  | sr 1 == red && sg 3 == green = []
--
  | sr 1 == yellow && sg 3 == red = [F, U, F', U', F, U, F']
  | sr 1 == green && sg 3 == yellow = [L', U', L, U, L', U', L]
--
  | sr 3 == yellow && sb 1 == green = [F', U', F, U, U, L', U', L]
  | sr 3 == red && sb 1 == yellow = [R, U, R', F, U, F']
  | sr 3 == green && sb 1 == red = [R, U, R', L', U', L]
--
  | sr 5 == green && sb 7 == yellow = [L', U, L]
  | sr 5 == yellow && sb 7 == red = [U, L', U', L]
  | sr 5 == red && sb 7 == green = [U, L', U, L, U', U', L', U', L]
--
  | sr 7 == red && sg 5 == yellow = [L', U', L]
  | sr 7 == green && sg 5 == red = [F, U, U, F', U', F, U, F']
  | sr 7 == yellow && sg 5 == green = [F, U, F']
--
  | sb 3 == green && so 1 == red = [R', U', U', R, F, U, F']
  | sb 3 == yellow && so 1 == green = [R', U', R, U', L', U', L]
  | sb 3 == red && so 1 == yellow = [B, U, B', U, F, U, F']
--
  | sb 5 == green && so 7 == yellow = [U, U, F, U, F']
  | sb 5 == red && so 7 == green = [U, U, L', U, L, U, U, L', U', L]
  | sb 5 == yellow && so 7 == red = [U, U, L', U', L]
--
  | so 3 == red && sg 1 == yellow = [L, U, L', U, U, F, U, F']
  | so 3 == yellow && sg 1 == green = [B', U', B, L', U', L]
  | so 3 == green && sg 1 == red = [L, U', L', U', F, U, F']
--
  | so 5 == yellow && sg 7 == red = [F, U', F']
  | so 5 == green && sg 7 == yellow = [U', F, U, F']
  | so 5 == red && sg 7 == green = [F, U, U, F', L', U', L]
  | otherwise = throw $ CouldNotSolve "setYGR"
  where sr = sel (r q)
        sb = sel (b q)
        sg = sel (g q)
        so = sel (o q)
        yellow = rotc tc Yellow
        green = rotc tc Green
        red = rotc tc Red

setGR :: Op -> Q -> [Op]
setGR tc q
  | sr 8 == red && sg 4 == green = []
--
  | sr 8 == green && sg 4 == red = 
      [F, U', F', U', L', U, L, U', F, U', F', U', L', U, L]
  | sr 6 == red && sw 2 == green = [U', L', U, L, U, F, U', F']
  | sr 6 == green && sw 2 == red = [U, U, F, U', F', U', L', U, L]
  | sr 4 == green && sb 8 == red =
      [R, U', R', U', F', U, F, U, L', U, L, U, F, U', F']
  | sr 4 == red && sb 8 == green =
      [R, U', R', U', F', U, F, F, U', F', U', L', U, L]
--
  | sb 4 == green && so 8 == red =
           [Y, R, U', R', U', F', U, F, Y', U, U, L', U, L, U, F, U', F']
  | sb 4 == red && so 8 == green =
      [Y, R, U', R', U', F', U, F, Y', U, F, U', F', U', L', U, L]
  | sb 6 == red && sw 4 == green = [L', U, L, U, F, U', F']
  | sb 6 == green && sw 4 == red = [U', F, U', F', U', L', U, L]
--
  | so 6 == red && sw 6 == green = [U, L', U, L, U, F, U', F']
  | so 6 == green && sw 6 == red = [F, U', F', U', L', U, L]
  | so 4 == red && sg 8 == green =
      [Y, Y, R, U', R', U', F', U, F, Y, Y, U, U, F, U', F', U', L', U, L]
  | so 4 == green && sg 8 == red =
      [Y, Y, R, U', R', U', F', U, F, Y, Y, U', L', U, L, U, F, U', F']
--
  | sg 6 == red && sw 8 == green = [U, U, L', U, L, U, F, U', F']
  | sg 6 == green && sw 8 == red = [U, F, U', F', U', L', U, L]
  | otherwise = throw $ CouldNotSolve "setGR"
  where sr = sel (r q)
        sg = sel (g q)
        sw = sel (w q)
        sb = sel (b q)
        so = sel (o q)
        green = rotc tc Green
        red = rotc tc Red

-- PLL1
oneToThree :: Q -> [Op]
oneToThree q
  | swq 2 /= White && swq 4 /= White &&
    swq 6 /= White && swq 8 /= White = PLL1p : [F, R, U, R', U', F']
  | otherwise = []
  where swq = sel (w q)

-- PLL2
threeToFive :: Q -> [Op]
threeToFive q
  | w2 == w4 && w2 /= w6 && w2 /= w8 = PLL21p : [B, U, L, U', L', B']
  | w4 == w6 && w4 /= w8 && w4 /= w2 = PLL22p : [U, B, U, L, U', L', B']
  | w6 == w8 && w6 /= w2 && w6 /= w4 = PLL23p : [U, U, B, U, L, U', L', B']
  | w8 == w2 && w8 /= w4 && w8 /= w6 = PLL24p : [U', B, U, L, U', L', B']
  | w4 == w8 && w4 /= w2 && w4 /= w6 = PLL25p : [F, R, U, R', U', F']
  | w2 == w6 && w2 /= w4 && w2 /= w8 = PLL26p : [U, F, R, U, R', U', F']
  | otherwise = []
  where (w2, w4, w6, w8) = let sq = sel (w q) in (sq 2, sq 4, sq 6, sq 8)

-- PLL3
fiveToNine :: Q -> [Op]
fiveToNine q
  | r5 == o7 && o7 == g5 && g5 == g7 =
      [R, U, U, R', R', U', R, R, U', R', R', U, U, R]
  | w1 == w3 && w3 == o5 && o5 == o7 =
      [R, R, D', R, U, U, R', D, R, U, U, R]
  | r5 == b5 && b5 == o5 && o5 == w1 = [R, U, R', U, R, U', U', R']
  | r7 == b7 && b7 == g7 && g7 == w5 = [R, U', U', R', U', R, U', R']
  | r7 == r5 && r5 == o7 && o7 == o5 = 
      [R, U', U', R', U', R, U, R', U', R, U', R']
  | r7 == b5 && b5 == w3 && w3 == w7 = 
      [L, F', F', R', R', D, R, D', R, F', F', L']
  | r7 == w3 && w3 == w5 && w5 == o5 = [L, F, R', F', L', F, R, F']
  | otherwise = []
  where (r5, r7) = let sq = sel (r q) in (sq 5, sq 7)
        (o5, o7) = let sq = sel (o q) in (sq 5, sq 7)
        (g5, g7) = let sq = sel (g q) in (sq 5, sq 7)
        (b5, b7) = let sq = sel (b q) in (sq 5, sq 7)
        (w1, w3, w5, w7) = let sq = sel (w q) 
                           in (sq 1, sq 3, sq 5, sq 7)

-- OLL
nineToFinish :: Q -> [Op]
nineToFinish q
  | g6 == g7 && o5 == o6 && b5 == b7 && b7 == r6 = -- A1
      A1p : [R, R, F, F, R', B', R, F, F, R', B, R']
  | g6 == g7 && o5 == o6 && r5 == r7 && r7 == b6 = -- A2
      A2p : [R, B', R, F, F, R', B, R, F, F, R', R']
  | r6 == r7 && g5 == g7 && g5 /= g6 && o5 == o6 = -- T
      Tp : [R, U, R', U', R', F, R, R, U', R', U', R, U, R', F']
  | o5 == o6 && o5 == o7 && r5 /= r6 && r5 == r7 = 
      if r5 == b6 
      then U1p : [R, R, U, R, U, R', U', R', U', R', U, R'] -- U1
      else U2p : [R, U', R, U, R, U, R, U', R', U', R', R'] -- U2
  | r6 == r7 && b5 == b6 && r7 == o5 && b6 /= b7 = -- Y
      Yp : [F, R, U', R', U', R, U, R', F', R, U, R', U', R', F, R, F']
  | r5 == r7 && r5 == b6 && r6 == b7 && g5 == g6 = -- R2
      R2p : [R', U', U', R, U', U', R', F, R, U, R', U', R', F', R, R, U']
  | r5 == r7 && r5 == b6 && r6 == b7 && b5 == b7 = -- Z
      Zp : [R', L, F', R, R, L', L', B', R, R, L', L', F', R', L, D, D, R, R, L', L', U]
  | r5 == r7 && r5 == g6 && r6 == g5 && g7 /= g5 = -- R1
      R1p : [L, U, U, L', U, U, L, F', L', U', L, U, L, F, L', L', U]
  | r5 == r7 && b5 == b7 && r5 == o6 && b5 == g6 = -- H
      Hp : [M, M, U', M, M, U', U', M, M, U', M, M]
  | r5 == r6 && g5 == g7 && g7 == b6 && b7 == g6 = -- G1
      G1p : [R, R, D, Y, R', U, R', U', R, D', Y', R', R', F', U, F]
  | r5 == r6 && r6 == o7 && g5 == g6 && g6 == g7 = -- J2
      J2p : [R, U, R', F', R, U, R', U', R', F, R, R, U', R', U']
  | r7 == r6 && r6 == o5 && b5 == b7 && b7 == g6 = -- G3
      G3p : [L', L', D', Y', L, U', L, U, L', D, Y, L, L, F, U', F']
  | r7 == r6 && r6 == o5 && b5 == b6 && b6 == b7 = -- J1
      J1p : [L', U', L, F, L', U', L, U, L, F', L', L', U, L, U]
  | r5 == r6 && o5 == o7 && o7 == b6 && r7 /= r6 = -- G2
      G2p : [F', U', F, R, R, D, Y, R', U, R, U', R, D', Y', R, R]
  | r7 == r6 && o5 == o7 && o5 == g6 && r5 == b6 = -- G4
      G4p : [F, U, F', L', L', D', Y', L, U', L', U, L', D, Y, L', L']
  | r7 == b5 && r5 == b6 && r6 == b7 && g6 == g5 = -- F
      Fp : [R', U', F', R, U, R', U', R', F, R, R, U', R', U', R, U, R', U, R]
  | r6 == r7 && g5 == g6 && g5 == b7 && r7 == o5 = -- V
      Vp : [R', U, R', U', Y, R', F', R, R, U', R', U, R', F, R, F, Y']
  | r7 == r6 && b7 == b6 && o7 == o6 && o6 == r5 && b6 /= b5 && g5 /= g6 = -- N2
      N2p : [R', U, R, U', R', F', U', F, R, U, R', F, R', F', R, U', R]
  | r5 == r6 && b5 == b6 && o5 == o6 && o6 == r7 && b6 /= b7 = -- N1
      N1p : [L, U', L', U, L, F, U, F', L', U', L, F', L, F, L', U, L']
  | r5 == b6 && b6 == o7 && r7 == g6 && g6 == o5 = -- E
      Ep : [R, B', R', F, R, B, R', F', R, B, R', F, R, B', R', F']
  | otherwise = []
  where (r5, r6, r7) = let sq = sel (r q) in (sq 5, sq 6, sq 7)
        (g5, g6, g7) = let sq = sel (g q) in (sq 5, sq 6, sq 7)
        (o5, o6, o7) = let sq = sel (o q) in (sq 5, sq 6, sq 7)
        (b5, b6, b7) = let sq = sel (b q) in (sq 5, sq 6, sq 7)


-- solver interfaces
solve :: String -> [Op]
solve str = solveQ $ applySeq (fromString str []) goal

solve_check, solve_check' :: String -> IO ()
solve_check' str = do
  let ins = fromString str []
  let outs = solve str
  let q_start = applySeq ins goal
  putStrLn "Scramble:"
  putStrLn $ show ins
  putStrLn "Scrambled:"
  pr $ q_start
  putStrLn "Solution:"
  putStrLn $ show outs
  putStrLn $ prSeq outs
  putStrLn "Solved:"
  pr $ applySeq outs q_start
solve_check str =
  catch (solve_check' str) $
  \(msg::CouldNotSolve) -> putStrLn $ show msg

solve_check_pat, solve_check_pat' :: String -> IO ()
solve_check_pat' str = do
  let q_start = fromStringPos str
  let outs = solveQ q_start
  putStrLn "Scrambled:"
  pr $ q_start
  putStrLn "Solution:"
  putStrLn $ show outs
  putStrLn $ prSeq outs
  putStrLn "Solved:"
  pr $ applySeq outs q_start
solve_check_pat str =
  catch (solve_check_pat' str) $
  \(msg::CouldNotSolve) -> putStrLn $ show msg ++ 
                           "\n... Illegal configuration ?"

main :: IO ()
main = do

  putStrLn $ "Input a scramble:"
  ins <- getLine
  solve_check ins
{-
  putStrLn $ "Input a scrambled pattern:"
  pat <- getLine
  solve_check_pat pat
-}
