{-
  Rubik's Cube Solver

  Copyright (C) 2018 Hideyuki Kawabata

  Example

  - using GHCI:
    *Cubing> solve_check "R  L'  U'  D2  F2  U'  F'  R  D'  U'  B2"
    ...

  - stand alone execution:
    $ ./Cubing
    Input a scramble:
    R  L'  U'  D2  F2  U'  F'  R  D'  U'  B2
    ...
-}

--module Cubing where

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
  | Y2 | Y'2 | Z2 | Z'2 | R2 | R'2 | U2 | U'2 | B2 | B'2 | L2 | L'2 | F2 | F'2 | D2 | D'2
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
goal =  Q { w = (White, White, White, White, White, White, White, White)
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
  M -> applySeq [Z, D', U, Y', Z'] q

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

optimizeOp :: [Op] -> [Op]
optimizeOp [] = []
optimizeOp l@(x:[]) = l
optimizeOp l@(a:b:c:d:ys) 
  | a == b && b == c && c == d && d == a = optimizeOp ys
  | a == b && b == c && c == d = optimizeOp ((revOp a):(d:ys))
  | otherwise = a:(optimizeOp (b:c:d:ys))
optimizeOp (a:b:ys) = case (a,b) of
  (U,U') -> optimizeOp ys
  (L,L') -> optimizeOp ys
  (R,R') -> optimizeOp ys
  (B,B') -> optimizeOp ys
  (D,D') -> optimizeOp ys
  (F,F') -> optimizeOp ys
  (Y,Y') -> optimizeOp ys
  (Z,Z') -> optimizeOp ys
  (U',U) -> optimizeOp ys
  (L',L) -> optimizeOp ys
  (R',R) -> optimizeOp ys
  (B',B) -> optimizeOp ys
  (D',D) -> optimizeOp ys
  (F',F) -> optimizeOp ys
  (Y',Y) -> optimizeOp ys
  (Z',Z) -> optimizeOp ys
  _ -> a:(optimizeOp (b:ys))


-- checker functions
checkRY :: Op -> Q -> Bool
checkRY tc q = sel (r q) 2 == rotc tc Red && sel (y q) 6 == rotc tc Yellow

checkYGR :: Op -> Q -> Bool
checkYGR tc q = sel (r q) 1 == rotc tc Red &&
                sel (g q) 3 == rotc tc Green &&
                sel (y q) 7 == rotc tc Yellow

checkGR :: Op -> Q -> Bool
checkGR tc q = sel (g q) 4 == rotc tc Green && sel (r q) 8 == rotc tc Red

checkOne :: Q -> Bool
checkOne q = swq 2 /= White && swq 4 /= White &&
             swq 6 /= White && swq 8 /= White
  where swq = sel (w q)

checkFive :: Q -> Bool
checkFive q = swq 4 == c && swq 6 == c && swq 8 == c
  where swq = sel (w q)
        c = swq 2

checkPat :: Q -> Bool
checkPat q
  | r5 == o7 && o7 == g5 && g5 == g7 = True
  | w1 == w3 && w3 == o5 && o5 == o7 = True
  | r5 == b5 && b5 == o5 && o5 == w1 = True
  | r7 == b7 && b7 == g7 && g7 == w5 = True
  | r7 == r5 && r5 == o7 && o7 == o5 = True
  | r7 == b5 && b5 == w3 && w3 == w7 = True
  | r7 == w3 && w3 == w5 && w5 == o5 = True
  | otherwise = False
  where (r5, r7) = let sq = sel (r q) in (sq 5, sq 7)
        (o5, o7) = let sq = sel (o q) in (sq 5, sq 7)
        (g5, g7) = let sq = sel (g q) in (sq 5, sq 7)
        (b5, b7) = let sq = sel (b q) in (sq 5, sq 7)
        (w1, w3, w5, w7) = let sq = sel (w q) 
                           in (sq 1, sq 3, sq 5, sq 7)

checkNine :: Q -> Bool
checkNine q
  | r6 == r7 && g5 == g7 && g5 /= g6 && o5 == o6 = True
  | o5 == o6 && o5 == o7 && o5 /= o6 && r5 == r7 = True
  | r6 == r7 && b5 == b6 && r7 == o5 = True
  | r5 == r7 && r5 == b6 && r6 == b7 = True
  | r5 == r7 && r5 == g6 && r6 == g5 && g7 /= g5 = True
  | r5 == r7 && b5 == b7 && r5 == o6 && b5 == g6 = True
  | r5 == r6 && r6 /= r7 && g5 == g7 = True
  | r7 == r6 && r6 /= r5 && b5 == b7 = True
  | r5 == r6 && o5 == o7 && o7 == b6 = True
  | r7 == r6 && o5 == o7 && o5 == g6 = True
  | r7 == b5 && r5 == b6 && r6 == b7 && g6 == g5 = True
  | r6 == r7 && g5 == g6 && g5 == b7 && r7 == o5 = True
  | otherwise = False
  where (r5, r6, r7) = let sq = sel (r q) in (sq 5, sq 6, sq 7)
        (g5, g6, g7) = let sq = sel (g q) in (sq 5, sq 6, sq 7)
        (o5, o6, o7) = let sq = sel (o q) in (sq 5, sq 6, sq 7)
        (b5, b6, b7) = let sq = sel (b q) in (sq 5, sq 6, sq 7)

checkU :: Q -> Bool
checkU q = let sq = sel (r q) in sq 4 == sq 5


-- solver functions
nextSeq :: Q -> [Op] -> [Op]
nextSeq q ops
  | not $ checkRY N q = cont $ setRY N q
  | not $ checkRY Y (turn Y q) = cont $ [Y] ++ setRY Y (turn Y q) ++ [Y']
  | not $ checkRY Y2 (turn Y2 q) = cont $ [Y2] ++ setRY Y2 (turn Y2 q) ++ [Y2] 
  | not $ checkRY Y' (turn Y' q) = cont $ [Y'] ++ setRY Y' (turn Y' q) ++ [Y]
--
  | not $ checkYGR N q = cont $ setYGR N q
  | not $ checkYGR Y (turn Y q) = cont $ [Y] ++ setYGR Y (turn Y q) ++ [Y']
  | not $ checkYGR Y2 (turn Y2 q) = cont $ [Y2] ++ setYGR Y2 (turn Y2 q) ++ [Y2]
  | not $ checkYGR Y' (turn Y' q) = cont $ [Y'] ++ setYGR Y' (turn Y' q) ++ [Y]
--
  | not $ checkGR N q = cont $ setGR N q
  | not $ checkGR Y (turn Y q) = cont $ [Y] ++ setGR Y (turn Y q) ++ [Y']
  | not $ checkGR Y2 (turn Y2 q) = cont $ [Y2] ++ setGR Y2 (turn Y2 q) ++ [Y2]
  | not $ checkGR Y' (turn Y' q) = cont $ [Y'] ++ setGR Y' (turn Y' q) ++ [Y]
--
  | checkOne q = cont $ oneToThree
  | not $ checkFive q = cont $ threeToFive q
  | checkPat q = cont $ fiveToNine q
  | checkPat (turn Y q) = cont $ [Y] ++ fiveToNine (turn Y q) ++ [Y']
  | checkPat (turn Y2 q) = cont $ [Y2] ++ fiveToNine (turn Y2 q) ++ [Y2]
  | checkPat (turn Y' q) = cont $ [Y'] ++ fiveToNine (turn Y' q) ++ [Y]
--
  | checkNine q = cont $ nineToFinish q
  | checkNine (turn Y q) = cont $ [Y] ++ nineToFinish (turn Y q) ++ [Y']
  | checkNine (turn Y2 q) = cont $ [Y2] ++ nineToFinish (turn Y2 q) ++ [Y2]
  | checkNine (turn Y' q) = cont $ [Y'] ++ nineToFinish (turn Y' q) ++ [Y]
--
  | checkU q = optimizeOp $ expandOp $ ops
  | checkU (turn U q) = optimizeOp $ expandOp $ ops ++ [U]
  | checkU (turn U2 q) = optimizeOp $ expandOp $ ops ++ [U2]
  | checkU (turn U' q) = optimizeOp $ expandOp $ ops ++ [U']
--
  | otherwise = error "nextSeq"
  where cont ops' = nextSeq (applySeq ops' q) (ops ++ ops')

rotc :: Op -> Color -> Color
rotc N c = c
rotc Y Red = Blue
rotc Y Blue = Orange
rotc Y Orange = Green
rotc Y Green = Red
rotc Y c = c
rotc Y' Blue = Red
rotc Y' Orange = Blue
rotc Y' Green = Orange
rotc Y' Red = Green
rotc Y' c = c
rotc Y2 Red = Orange
rotc Y2 Blue = Green
rotc Y2 Orange = Red
rotc Y2 Green = Blue
rotc Y2 c = c

setRY :: Op -> Q -> [Op]
setRY tc q
  | sel (r q) 2 == rotc tc Yellow &&
    sel (y q) 6 == rotc tc Red = [F', D, R', D']
  | sel (r q) 4 == rotc tc Yellow &&
    sel (b q) 8 == rotc tc Red = [D, R', D']
  | sel (r q) 4 == rotc tc Red &&
    sel (b q) 8 == rotc tc Yellow = [F]
  | sel (r q) 6 == rotc tc Red &&
    sel (w q) 2 == rotc tc Yellow = [F, F]
  | sel (r q) 6 == rotc tc Yellow &&
    sel (w q) 2 == rotc tc Red = [U', R', F, R]
  | sel (r q) 8 == rotc tc Yellow &&
    sel (g q) 4 == rotc tc Red = [D', L, D]
  | sel (r q) 8 == rotc tc Red &&
    sel (g q) 4 == rotc tc Yellow = [F']

  | sel (b q) 2 == rotc tc Red &&
    sel (y q) 4 == rotc tc Yellow = [R, D, R', D']
  | sel (b q) 2 == rotc tc Yellow &&
    sel (y q) 4 == rotc tc Red = [R, F]
  | sel (b q) 4 == rotc tc Yellow &&
    sel (o q) 8 == rotc tc Red = [B, U, U, B', F, F]
  | sel (b q) 4 == rotc tc Red &&
    sel (o q) 8 == rotc tc Yellow = [R', U, R, F, F]
  | sel (b q) 6 == rotc tc Red &&
    sel (w q) 4 == rotc tc Yellow = [U, F, F]
  | sel (b q) 6 == rotc tc Yellow &&
    sel (w q) 4 == rotc tc Red = [R', F, R]

  | sel (o q) 2 == rotc tc Red &&
    sel (y q) 2 == rotc tc Yellow = [B, B, U, U, F, F]
  | sel (o q) 2 == rotc tc Yellow &&
    sel (y q) 2 == rotc tc Red = [B, B, U, R', F, R]
  | sel (o q) 4 == rotc tc Yellow &&
    sel (g q) 8 == rotc tc Red = [L, U', L', F, F]
  | sel (o q) 4 == rotc tc Red &&
    sel (g q) 8 == rotc tc Yellow = [B', U', B, U', F, F]
  | sel (o q) 6 == rotc tc Red &&
    sel (w q) 6 == rotc tc Yellow = [U, U, F, F]
  | sel (o q) 6 == rotc tc Yellow &&
    sel (w q) 6 == rotc tc Red = [U', L, F', L']

  | sel (g q) 2 == rotc tc Red &&
    sel (y q) 8 == rotc tc Yellow = [L', D', L, D]
  | sel (g q) 2 == rotc tc Yellow &&
    sel (y q) 8 == rotc tc Red = [L', F']
  | sel (g q) 6 == rotc tc Red &&
    sel (w q) 8 == rotc tc Yellow = [U', F, F]
  | sel (g q) 6 == rotc tc Yellow &&
    sel (w q) 8 == rotc tc Red = [L, F', L']

  | otherwise = error "setRY"

setYGR :: Op -> Q -> [Op]
setYGR tc q
  | sel (r q) 1 == rotc tc Yellow &&
    sel (g q) 3 == rotc tc Red = [Y', R, U, R', U', R, U, R', Y]
  | sel (r q) 1 == rotc tc Green &&
    sel (g q) 3 == rotc tc Yellow = [Y', U, R, U', R', U, R, U', R', Y]
--
  | sel (r q) 3 == rotc tc Yellow &&
    sel (b q) 1 == rotc tc Green = [R, U, R', Y', R, U, R', U', R, U, R', U', R, U, R', Y]
  | sel (r q) 3 == rotc tc Red &&
    sel (b q) 1 == rotc tc Yellow = [R, U, R', Y', R, U, R', Y]
  | sel (r q) 3 == rotc tc Green &&
    sel (b q) 1 == rotc tc Red = [R, U, R', Y', U, R, U', R', Y]
--
  | sel (r q) 5 == rotc tc Green &&
    sel (b q) 7 == rotc tc Yellow = [U, Y', R, U, R', U', Y]  
  | sel (r q) 5 == rotc tc Yellow &&
    sel (b q) 7 == rotc tc Red = [U, Y', U, R, U', R', Y]
  | sel (r q) 5 == rotc tc Red &&
    sel (b q) 7 == rotc tc Green = [U, Y', R, U, R', U', R, U, R', U', R, U, R', Y]
--
  | sel (r q) 7 == rotc tc Red &&
    sel (g q) 5 == rotc tc Yellow = [Y', U, R, U', R', Y]
  | sel (r q) 7 == rotc tc Green &&
    sel (g q) 5 == rotc tc Red = [Y', R, U, R', U', R, U, R', U', R, U, R', Y]
  | sel (r q) 7 == rotc tc Yellow &&
    sel (g q) 5 == rotc tc Green = [Y', R, U, R', Y]
--
  | sel (b q) 3 == rotc tc Green &&
    sel (o q) 1 == rotc tc Red = [Y, R, U, R', U, Y2, U, R, U', R', Y]
  | sel (b q) 3 == rotc tc Yellow &&
    sel (o q) 1 == rotc tc Green = [Y, R, U, R', U, Y2, R, U, R', U', R, U, R', U', R, U, R', Y]
  | sel (b q) 3 == rotc tc Red &&
    sel (o q) 1 == rotc tc Yellow = [Y, R, U, R', U, Y2, R, U, R', Y]
--
  | sel (b q) 5 == rotc tc Green &&
    sel (o q) 7 == rotc tc Yellow = [U, U, Y', R, U, R', Y]
  | sel (b q) 5 == rotc tc Red &&
    sel (o q) 7 == rotc tc Green = [U, U, Y', R, U, R', U', R, U, R', U', R, U, R', Y]
  | sel (b q) 5 == rotc tc Yellow &&
    sel (o q) 7 == rotc tc Red = [U, U, Y', U, R, U', R', Y]
--
  | sel (o q) 3 == rotc tc Red &&
    sel (g q) 1 == rotc tc Yellow = [Y, Y, R, U', R', U', Y, R, U, R', U', R, U, R', U', R, U, R', Y]
  | sel (o q) 3 == rotc tc Yellow &&
    sel (g q) 1 == rotc tc Green = [Y, Y, R, U', R', Y, R, U, R', Y]
  | sel (o q) 3 == rotc tc Green &&
    sel (g q) 1 == rotc tc Red = [Y, Y, R, U', R', U', Y, R, U, R', Y]
--
  | sel (o q) 5 == rotc tc Yellow &&
    sel (g q) 7 == rotc tc Red = [Y', R, U, R', Y]
  | sel (o q) 5 == rotc tc Green &&
    sel (g q) 7 == rotc tc Yellow = [Y', U', R, U, R', Y]
  | sel (o q) 5 == rotc tc Red &&
    sel (g q) 7 == rotc tc Green = [Y', U', R, U, R', U', R, U, R', U', R, U, R', Y]

  | otherwise = error "setYGR"

setGR :: Op -> Q -> [Op]
setGR tc q
  | sel (r q) 8 == rotc tc Green &&
    sel (g q) 4 == rotc tc Red = [U, F, U', F', U', L, U, L', U', F, U', F', U', L', U, L]

  | sel (r q) 6 == rotc tc Red &&
    sel (w q) 2 == rotc tc Green = [Y', U', F', U, F, U, R, U', R', Y]
  | sel (r q) 6 == rotc tc Green &&
    sel (w q) 2 == rotc tc Red = [Y', U, U, R, U', R', U', F', U, F, Y]

  | sel (r q) 4 == rotc tc Green &&
    sel (b q) 8 == rotc tc Red = [U, R, U', R', U', F', U, F, U, L', U, L, U, F, U', F']
  | sel (r q) 4 == rotc tc Red &&
    sel (b q) 8 == rotc tc Green = [U, R, U', R', U', F', U, F, F, U', F', U', L', U, L]
--
  | sel (b q) 4 == rotc tc Green &&
    sel (o q) 8 == rotc tc Red = [Y, U, R, U', R', U', R, U, R', Y', U, U, L', U, L, U, F, U', F']
  | sel (b q) 4 == rotc tc Red &&
    sel (o q) 8 == rotc tc Green = [Y, U, R, U', R', U', R, U, R', Y', U, F, U', F', U', L', U, L]
  
  | sel (b q) 6 == rotc tc Red &&
    sel (w q) 4 == rotc tc Green = [L', U, L, U, F, U', F']
  | sel (b q) 6 == rotc tc Green &&
    sel (w q) 4 == rotc tc Red = [U', F, U', F', U', L', U, L]
--
  | sel (o q) 6 == rotc tc Red &&
    sel (w q) 6 == rotc tc Green = [U, L', U, L, U, F, U', F']
  | sel (o q) 6 == rotc tc Green &&
    sel (w q) 6 == rotc tc Red = [F, U', F', U', L', U, L]

  | sel (o q) 4 == rotc tc Red &&
    sel (g q) 8 == rotc tc Green = [Y, Y, U, R, U', R', U', F', U, F, Y, Y, U, U, F, U', F', U', L', U, L]
  | sel (o q) 4 == rotc tc Green &&
    sel (g q) 8 == rotc tc Red = [Y, Y, U, R, U', R', U', F', U, F, Y, Y, U', L', U, L, U, F, U', F']
--
  | sel (g q) 6 == rotc tc Red &&
    sel (w q) 8 == rotc tc Green = [U, U, L', U, L, U, F, U', F']
  | sel (g q) 6 == rotc tc Green &&
    sel (w q) 8 == rotc tc Red = [U, F, U', F', U', L', U, L]

  | otherwise = error "setGR"


oneToThree :: [Op]
oneToThree  = [F, R, U, R', U', F']

threeToFive :: Q -> [Op]
threeToFive q
  | w2 == w4 && w2 /= w6 && w2 /= w8 = [B, U, L, U', L', B']
  | w4 == w6 && w4 /= w8 && w4 /= w2 = [U, B, U, L, U', L', B']
  | w6 == w8 && w6 /= w2 && w6 /= w4 = [U, U, B, U, L, U', L', B']
  | w8 == w2 && w8 /= w4 && w8 /= w6 = [U', B, U, L, U', L', B']
  | w4 == w8 && w4 /= w2 && w4 /= w6 = [F, R, U, R', U', F']
  | w2 == w6 && w2 /= w4 && w2 /= w8 = [U, F, R, U, R', U', F']
  | otherwise = error "threeToFive"
  where (w2, w4, w6, w8) = let sq = sel (w q) in (sq 2, sq 4, sq 6, sq 8)

fiveToNine :: Q -> [Op]
fiveToNine q
  | r5 == o7 && o7 == g5 && g5 == g7 =
      [R, U, U, R', R', U', R, R, U', R', R', U, U, R]
  | w1 == w3 && w3 == o5 && o5 == o7 =
      [R, R, D', R, U, U, R', D, R, U, U, R]
  | r5 == b5 && b5 == o5 && o5 == w1 = [R, U, R', U, R, U', U', R']
  | r7 == b7 && b7 == g7 && g7 == w5 = [R, U', U', R', U', R, U', R]
  | r7 == r5 && r5 == o7 && o7 == o5 = 
      [R, U', U', R', U', R, U, R', U', R, U', R']
  | r7 == b5 && b5 == w3 && w3 == w7 = 
      [L, F', F', R', R', D, R, D', R, F', F', L']
  | r7 == w3 && w3 == w5 && w5 == o5 = [L, F, R', F', L', F, R, F']
  | otherwise = error "fiveToNine"
  where (r5, r7) = let sq = sel (r q) in (sq 5, sq 7)
        (o5, o7) = let sq = sel (o q) in (sq 5, sq 7)
        (g5, g7) = let sq = sel (g q) in (sq 5, sq 7)
        (b5, b7) = let sq = sel (b q) in (sq 5, sq 7)
        (w1, w3, w5, w7) = let sq = sel (w q) 
                           in (sq 1, sq 3, sq 5, sq 7)


nineToFinish :: Q -> [Op]
nineToFinish q
  | r6 == r7 && g5 == g7 && g5 /= g6 && o5 == o6 = 
      [R, U, R', U', R', F, R, R, U', R', U', R, U, R', F] -- T-perm
  | o5 == o6 && o5 == o7 && o5 /= o6 && r5 == r7 = 
      if sel (r q) 5 == sel (b q) 6 
      then [R, R, U, R, U, R', U', R', U', R', U, R']
      else [R, U', R, U, R, U, R, U', R', U', R', R']
  | r6 == r7 && b5 == b6 && r7 == o5 = 
      [F, R, U', R', U', R, U, R', F', R, U, R', U', R', F, R, F']
  | r5 == r7 && r5 == b6 && r6 == b7 = 
      if sel (b q) 7 /= sel (b q) 5 
      then [R', U', U', R, U', U', R', F, R, U, R', U', R', F', R, R, U']
      else [R', L, F', R, R, L', L', B', R, R, L', L', F', R, L', D, D, R, R, L', L', U]
  | r5 == r7 && r5 == g6 && r6 == g5 && g7 /= g5 =
      [L, U, U, L', U, U, L, F', L', U', L, U, L, F, L', L', U]
  | r5 == r7 && b5 == b7 && r5 == o6 && b5 == g6 = 
      [M, M, U', M, M, U', U', M, M, U', M, M]
  | r5 == r6 && r6 /= r7 && g5 == g7 = 
      if sel (g q) 5 /= sel (g q) 6
      then [R, R, D, Y, R', U, R', U', R, D', Y', R', R', F', U, F]
      else [R, U, R', F', R, U, R', U', R', F, R, R, U', R', U']
  | r7 == r6 && r6 /= r5 && b5 == b7 = 
      if sel (b q) 5 /= sel (b q) 6 
      then [L', L', D', Y', L, U', L, U, L', D, Y, L, L, F, U', F']
      else [L', U', L, F, L', U', L, U, L, F', L', L', U, L, U]
  | r5 == r6 && o5 == o7 && o7 == b6 = 
      [F', U', F, R, R, D, Y, R', U, R, U', R, D', Y', R, R]
  | r7 == r6 && o5 == o7 && o5 == g6 = 
      [F, U, F', L', L', D', Y', L, U', L', U, L', D, Y, L', L']
  | r7 == b5 && r5 == b6 && r6 == b7 && g6 == g5 = 
      [R', U', F', R, U, R', U', R', F, R, R, U', R', U', R, U, R', U, R]
  | r6 == r7 && g5 == g6 && g5 == b7 && r7 == o5 = 
      [R', U, R', U', Y, R', F', R, R, U', R', U, R', F, R, F]
  | otherwise = error "nineToFinish"
  where (r5, r6, r7) = let sq = sel (r q) in (sq 5, sq 6, sq 7)
        (g5, g6, g7) = let sq = sel (g q) in (sq 5, sq 6, sq 7)
        (o5, o6, o7) = let sq = sel (o q) in (sq 5, sq 6, sq 7)
        (b5, b6, b7) = let sq = sel (b q) in (sq 5, sq 6, sq 7)



-- solver interfaces
solve :: String -> [Op]
solve str = nextSeq (applySeq (fromString str []) goal) []

solve_check :: String -> IO ()
solve_check str = do
  let ins = fromString str []
  let outs = solve str
  let q_start = applySeq ins goal
  putStrLn "Scramble:"
  putStrLn $ show ins
  putStrLn "Scrambled:"
  pr $ q_start
  putStrLn "Solution:"
  putStrLn $ show outs
  putStrLn "Solved:"
  pr $ applySeq outs q_start

main :: IO ()
main = do
  putStrLn $ "Input a scramble:"
  ins <- getLine
  solve_check ins
  
