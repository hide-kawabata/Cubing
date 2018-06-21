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

member :: Char -> String -> Bool
member _ [] = False
member c (x:xs)
  | c == x = True
  | otherwise = member c xs

fromString :: String -> [Op] -> [Op]
fromString [] acc = reverse acc
fromString (x:xs) acc
  | x == ' ' = fromString xs acc
  | member x "RUBLFDYZM" = 
    if length xs >= 1 then
      if xs !! 0 == '\'' then
        if length xs >= 2 then
          if xs !! 1 == '2' then fromString (drop 2 xs) ((read (x:(xs!!0):(xs!!1):[]) :: Op) : acc)
          else fromString (drop 1 xs) ((read (x:(xs!!0):[]) :: Op) : acc)
        else fromString (drop 1 xs) ((read (x:(xs!!0):[]) :: Op) : acc)
      else if xs !! 0 == '2' then fromString (drop 1 xs) ((read (x:(xs!!0):[]) :: Op) : acc)
      else fromString xs ((read (x:[]) :: Op) : acc)
    else fromString xs ((read (x:[]) :: Op) : acc)
  | x == ',' = fromString xs acc
  | otherwise = error "fromString"

-- pritty printer
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
turn R' q = turn R $ turn R $ turn R q
turn Y q = Q { w = rot False $ w q
             , r = b q
             , b = o q
             , o = g q
             , g = r q
             , y = rot True $ y q
             }
turn Y' q = turn Y $ turn Y $ turn Y q
turn Z q = Q { w = rot False $ g q
             , r = rot False $ r q
             , b = rot False $ w q
             , o = rot True $ o q
             , g = rot False $ y q
             , y = rot False $ b q
             }
turn Z' q = turn Z $ turn Z $ turn Z q
turn L q = turn Y $ turn Y $ turn R $ turn Y $ turn Y q
turn L' q = turn Y $ turn Y $ turn R' $ turn Y $ turn Y q
turn U q = turn Z' $ turn R $ turn Z q
turn U' q = turn Z' $ turn R' $ turn Z q
turn D q = turn Z $ turn R $ turn Z' q
turn D' q = turn Z $ turn R' $ turn Z' q
turn F q = turn Y $ turn R $ turn Y' q
turn F' q = turn Y $ turn R' $ turn Y' q
turn B q = turn Y' $ turn R $ turn Y q
turn B' q = turn Y' $ turn R' $ turn Y q
turn Y2 q = turn Y $ turn Y q
turn M q = applySeq [Z, D', U, Y', Z'] q

applySeq :: [Op] -> Q -> Q
applySeq l q = foldl (\r op -> turn op r) q l'
  where l' = optimizeOp $ expandOp l


-- simple optimizations
expandOp :: [Op] -> [Op]
expandOp [] = []
expandOp (x:xs)
  | x == R2 = R:R:(expandOp xs)
  | x == L2 = L:L:(expandOp xs)
  | x == D2 = D:D:(expandOp xs)
  | x == U2 = U:U:(expandOp xs)
  | x == B2 = B:B:(expandOp xs)
  | x == F2 = F:F:(expandOp xs)
  | x == Y2 = Y:Y:(expandOp xs)
  | x == Z2 = Z:Z:(expandOp xs)
  | x == R'2 = R:R:(expandOp xs)
  | x == L'2 = L:L:(expandOp xs)
  | x == D'2 = D:D:(expandOp xs)
  | x == U'2 = U:U:(expandOp xs)
  | x == B'2 = B:B:(expandOp xs)
  | x == F'2 = F:F:(expandOp xs)
  | x == Y'2 = Y:Y:(expandOp xs)
  | x == Z'2 = Z:Z:(expandOp xs)
  | otherwise = x:(expandOp xs)

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
optimizeOp (U:U':ys) = optimizeOp ys
optimizeOp (L:L':ys) = optimizeOp ys
optimizeOp (R:R':ys) = optimizeOp ys
optimizeOp (B:B':ys) = optimizeOp ys
optimizeOp (D:D':ys) = optimizeOp ys
optimizeOp (F:F':ys) = optimizeOp ys
optimizeOp (Y:Y':ys) = optimizeOp ys
optimizeOp (Z:Z':ys) = optimizeOp ys
optimizeOp (U':U:ys) = optimizeOp ys
optimizeOp (L':L:ys) = optimizeOp ys
optimizeOp (R':R:ys) = optimizeOp ys
optimizeOp (B':B:ys) = optimizeOp ys
optimizeOp (D':D:ys) = optimizeOp ys
optimizeOp (F':F:ys) = optimizeOp ys
optimizeOp (Y':Y:ys) = optimizeOp ys
optimizeOp (Z':Z:ys) = optimizeOp ys
optimizeOp l@(a:b:c:d:ys) 
  | a == b && b == c && c == d && d == a = optimizeOp ys
  | a == b && b == c && c == d = optimizeOp ((revOp a):(d:ys))
  | otherwise = a:(optimizeOp (b:c:d:ys))
optimizeOp (x:xs) = x:(optimizeOp xs)


-- checker functions
checkRY q = sel (r q) 2 == Red && sel (y q) 6 == Yellow
checkBY q = sel (b q) 2 == Blue && sel (y q) 4 == Yellow
checkOY q = sel (o q) 2 == Orange && sel (y q) 2 == Yellow
checkGY q = sel (g q) 2 == Green && sel (y q) 8 == Yellow
checkYGR q = sel (r q) 1 == Red && sel (g q) 3 == Green && sel (y q) 7 == Yellow
checkYRB q = sel (r q) 3 == Red && sel (b q) 1 == Blue && sel (y q) 5 == Yellow
checkYBO q = sel (b q) 3 == Blue && sel (o q) 1 == Orange && sel (y q) 3 == Yellow
checkYOG q = sel (o q) 3 == Orange && sel (g q) 1 == Green && sel (y q) 1 == Yellow
checkGR q = sel (g q) 4 == Green && sel (r q) 8 == Red
checkRB q = sel (r q) 4 == Red && sel (b q) 8 == Blue
checkBO q = sel (b q) 4 == Blue && sel (o q) 8 == Orange
checkOG q = sel (o q) 4 == Orange && sel (g q) 8 == Green
--checkWR q = sel (w q) 2 == White && sel (r q) 6 == Red
--checkWB q = sel (w q) 4 == White && sel (b q) 6 == Blue
--checkWO q = sel (w q) 6 == White && sel (o q) 6 == Orange
--checkWG q = sel (w q) 8 == White && sel (g q) 6 == Green
--checkWGR q = sel (w q) 1 == White && sel (g q) 5 == Green && sel (r q) 7 == Red
--checkWRB q = sel (w q) 3 == White && sel (r q) 5 == Red && sel (b q) 7 == Blue
--checkWBO q = sel (w q) 5 == White && sel (b q) 5 == Blue && sel (o q) 7 == Orange
--checkWOG q = sel (w q) 7 == White && sel (o q) 5 == Orange && sel (g q) 7 == Green
--check1 q = checkRY q && checkBY q && checkOY q && checkGY q
--check2 q = check1 q && checkYGR q && checkYRB q && checkYBO q && checkYOG q
--check3 q = check2 q && checkGR q && checkRB q && checkBO q && checkOG q
--check4 q = check3 q && checkWR q && checkWB q && checkWO q && checkWG q
--finished q = check4 q && checkWGR q && checkWRB q && checkWBO q && checkWOG q

checkZero q = sel (w q) 2 /= White && sel (w q) 4 /= White &&
              sel (w q) 6 /= White && sel (w q) 8 /= White

checkFive q = sel (w q) 2 == White && sel (w q) 4 == White &&
              sel (w q) 6 == White && sel (w q) 8 == White

checkPat tc q
  | sel (r q) 5 == turnedColor tc White &&
    sel (o q) 7 == turnedColor tc White &&
    sel (g q) 5 == turnedColor tc White &&
    sel (g q) 7 == turnedColor tc White = True
  | sel (w q) 1 == turnedColor tc White &&
    sel (w q) 3 == turnedColor tc White &&
    sel (o q) 5 == turnedColor tc White &&
    sel (o q) 7 == turnedColor tc White = True
  | sel (r q) 5 == turnedColor tc White &&
    sel (b q) 5 == turnedColor tc White &&
    sel (o q) 5 == turnedColor tc White &&
    sel (w q) 1 == turnedColor tc White = True
  | sel (r q) 7 == turnedColor tc White &&
    sel (b q) 7 == turnedColor tc White &&
    sel (g q) 7 == turnedColor tc White &&
    sel (w q) 5 == turnedColor tc White = True
  | sel (r q) 7 == turnedColor tc White &&
    sel (r q) 5 == turnedColor tc White &&
    sel (o q) 7 == turnedColor tc White &&
    sel (o q) 5 == turnedColor tc White = True
  | sel (r q) 7 == turnedColor tc White &&
    sel (b q) 5 == turnedColor tc White &&
    sel (w q) 3 == turnedColor tc White &&
    sel (w q) 7 == turnedColor tc White = True
  | sel (r q) 7 == turnedColor tc White &&
    sel (w q) 3 == turnedColor tc White &&
    sel (w q) 5 == turnedColor tc White &&
    sel (o q) 5 == turnedColor tc White = True
  | otherwise = False

checkNine q
  | sel (r q) 6 == sel (r q) 7 &&
    sel (g q) 5 == sel (g q) 7 &&
    sel (g q) 5 /= sel (g q) 6 &&
    sel (o q) 5 == sel (o q) 6 = True

  | sel (o q) 5 == sel (o q) 6 &&
    sel (o q) 5 == sel (o q) 7 &&
    sel (o q) 5 /= sel (o q) 6 &&
    sel (r q) 5 == sel (r q) 7 = True

  | sel (r q) 6 == sel (r q) 7 &&
    sel (b q) 5 == sel (b q) 6 &&
    sel (r q) 7 == sel (o q) 5 = True

  | sel (r q) 5 == sel (r q) 7 &&
    sel (r q) 5 == sel (b q) 6 &&
    sel (r q) 6 == sel (b q) 7 = True

  | sel (r q) 5 == sel (r q) 7 &&
    sel (r q) 5 == sel (g q) 6 &&
    sel (r q) 6 == sel (g q) 5 &&
    sel (g q) 7 /= sel (g q) 5 = True

  | sel (r q) 5 == sel (r q) 7 &&
    sel (b q) 5 == sel (b q) 7 &&
    sel (r q) 5 == sel (o q) 6 &&
    sel (b q) 5 == sel (g q) 6 = True

  | sel (r q) 5 == sel (r q) 6 &&
    sel (r q) 6 /= sel (r q) 7 &&
    sel (g q) 5 == sel (g q) 7 = True

  | sel (r q) 7 == sel (r q) 6 &&
    sel (r q) 6 /= sel (r q) 5 &&
    sel (b q) 5 == sel (b q) 7 = True

  | sel (r q) 5 == sel (r q) 6 &&
    sel (o q) 5 == sel (o q) 7 &&
    sel (o q) 7 == sel (b q) 6 = True
  | sel (r q) 7 == sel (r q) 6 &&
    sel (o q) 5 == sel (o q) 7 &&
    sel (o q) 5 == sel (g q) 6 = True

  | sel (r q) 7 == sel (b q) 5 &&
    sel (r q) 5 == sel (b q) 6 &&
    sel (r q) 6 == sel (b q) 7 &&
    sel (g q) 6 == sel (g q) 5 = True

  | otherwise = False

checkU q
  | sel (r q) 6 == Red = True
  | otherwise = False


-- solver functions
nextSeq :: Q -> [Op] -> [Op]
nextSeq q ops
  | not $ checkRY q = let ops' = setRY N q in nextSeq (applySeq ops' q) (ops ++ ops')
  | not $ checkBY q = let ops' = [Y] ++ setRY Y (applySeq [Y] q) ++ [Y']
                      in nextSeq (applySeq ops' q) (ops ++ ops')
  | not $ checkOY q = let ops' = [Y2] ++ setRY Y2 (applySeq [Y2] q) ++ [Y2] 
                      in nextSeq (applySeq ops' q) (ops ++ ops')
  | not $ checkGY q = let ops' = [Y'] ++ setRY Y' (applySeq [Y'] q) ++ [Y]
                      in nextSeq (applySeq ops' q) (ops ++ ops')
--
  | not $ checkYGR q = let ops' = setYGR N q in nextSeq (applySeq ops' q) (ops ++ ops')
  | not $ checkYRB q = let ops' = [Y] ++ setYGR Y (applySeq [Y] q) ++ [Y']
                       in nextSeq (applySeq ops' q) (ops ++ ops')
  | not $ checkYBO q = let ops' = [Y2] ++ setYGR Y2 (applySeq [Y2] q) ++ [Y2]
                       in nextSeq (applySeq ops' q) (ops ++ ops')
  | not $ checkYOG q = let ops' = [Y'] ++ setYGR Y' (applySeq [Y'] q) ++ [Y]
                       in nextSeq (applySeq ops' q) (ops ++ ops')
--
  | not $ checkGR q = let ops' = setGR N q in nextSeq (applySeq ops' q) (ops ++ ops')
  | not $ checkRB q = let ops' = [Y] ++ setGR Y (applySeq [Y] q) ++ [Y']
                      in nextSeq (applySeq ops' q) (ops ++ ops')
  | not $ checkBO q = let ops' = [Y2] ++ setGR Y2 (applySeq [Y2] q) ++ [Y2]
                      in nextSeq (applySeq ops' q) (ops ++ ops')
  | not $ checkOG q = let ops' = [Y'] ++ setGR Y' (applySeq [Y'] q) ++ [Y]
                      in nextSeq (applySeq ops' q) (ops ++ ops')
--
  | checkZero q = let ops' = zeroToThree N q in nextSeq (applySeq ops' q) (ops ++ ops')
  | not $ checkFive q = let ops' = threeToFive N q in nextSeq (applySeq ops' q) (ops ++ ops')
  | checkPat N q = let ops' = fiveToNine N q in nextSeq (applySeq ops' q) (ops ++ ops')
  | checkPat N (applySeq [Y] q) = let ops' = [Y] ++ fiveToNine Y (applySeq [Y] q) ++ [Y']
                   in nextSeq (applySeq ops' q) (ops ++ ops')
  | checkPat N (applySeq [Y2] q) = let ops' = [Y2] ++ fiveToNine Y2 (applySeq [Y2] q) ++ [Y2]
                   in nextSeq (applySeq ops' q) (ops ++ ops')
  | checkPat N (applySeq [Y'] q) = let ops' = [Y'] ++ fiveToNine Y' (applySeq [Y'] q) ++ [Y]
                   in nextSeq (applySeq ops' q) (ops ++ ops')
--
  | checkNine q = let ops' = nineToFinish q in nextSeq (applySeq ops' q) (ops ++ ops')

  | checkNine (applySeq [Y] q) = let ops' = [Y] ++ nineToFinish (applySeq [Y] q) ++ [Y']
                   in nextSeq (applySeq ops' q) (ops ++ ops')
  | checkNine (applySeq [Y2] q) = let ops' = [Y2] ++ nineToFinish (applySeq [Y2] q) ++ [Y2]
                   in nextSeq (applySeq ops' q) (ops ++ ops')
  | checkNine (applySeq [Y'] q) = let ops' = [Y'] ++ nineToFinish (applySeq [Y'] q) ++ [Y]
                   in nextSeq (applySeq ops' q) (ops ++ ops')
--
  | checkU q = optimizeOp $ expandOp $ ops
  | checkU (applySeq [U] q) = optimizeOp $ expandOp $ ops ++ [U]
  | checkU (applySeq [U2] q) = optimizeOp $ expandOp $ ops ++ [U2]
  | checkU (applySeq [U'] q) = optimizeOp $ expandOp $ ops ++ [U']
--
  | otherwise = error "nextSeq"


turnedColor :: Op -> Color -> Color
turnedColor N c = c
turnedColor Y Red = Blue
turnedColor Y Blue = Orange
turnedColor Y Orange = Green
turnedColor Y Green = Red
turnedColor Y c = c
turnedColor Y' Blue = Red
turnedColor Y' Orange = Blue
turnedColor Y' Green = Orange
turnedColor Y' Red = Green
turnedColor Y' c = c
turnedColor Y2 Red = Orange
turnedColor Y2 Blue = Green
turnedColor Y2 Orange = Red
turnedColor Y2 Green = Blue
turnedColor Y2 c = c

setRY tc q
  | sel (r q) 2 == turnedColor tc Yellow &&
    sel (y q) 6 == turnedColor tc Red = [F', D, R', D']
  | sel (r q) 4 == turnedColor tc Yellow &&
    sel (b q) 8 == turnedColor tc Red = [D, R', D']
  | sel (r q) 4 == turnedColor tc Red &&
    sel (b q) 8 == turnedColor tc Yellow = [F]
  | sel (r q) 6 == turnedColor tc Red &&
    sel (w q) 2 == turnedColor tc Yellow = [F, F]
  | sel (r q) 6 == turnedColor tc Yellow &&
    sel (w q) 2 == turnedColor tc Red = [U', R', F, R]
  | sel (r q) 8 == turnedColor tc Yellow &&
    sel (g q) 4 == turnedColor tc Red = [D', L, D]
  | sel (r q) 8 == turnedColor tc Red &&
    sel (g q) 4 == turnedColor tc Yellow = [F']

  | sel (b q) 2 == turnedColor tc Red &&
    sel (y q) 4 == turnedColor tc Yellow = [R, D, R', D']
  | sel (b q) 2 == turnedColor tc Yellow &&
    sel (y q) 4 == turnedColor tc Red = [R, F]
  | sel (b q) 4 == turnedColor tc Yellow &&
    sel (o q) 8 == turnedColor tc Red = [B, U, U, B', F, F]
  | sel (b q) 4 == turnedColor tc Red &&
    sel (o q) 8 == turnedColor tc Yellow = [R', U, R, F, F]
  | sel (b q) 6 == turnedColor tc Red &&
    sel (w q) 4 == turnedColor tc Yellow = [U, F, F]
  | sel (b q) 6 == turnedColor tc Yellow &&
    sel (w q) 4 == turnedColor tc Red = [R', F, R]

  | sel (o q) 2 == turnedColor tc Red &&
    sel (y q) 2 == turnedColor tc Yellow = [B, B, U, U, F, F]
  | sel (o q) 2 == turnedColor tc Yellow &&
    sel (y q) 2 == turnedColor tc Red = [B, B, U, R', F, R]
  | sel (o q) 4 == turnedColor tc Yellow &&
    sel (g q) 8 == turnedColor tc Red = [L, U', L', F, F]
  | sel (o q) 4 == turnedColor tc Red &&
    sel (g q) 8 == turnedColor tc Yellow = [B', U', B, U', F, F]
  | sel (o q) 6 == turnedColor tc Red &&
    sel (w q) 6 == turnedColor tc Yellow = [U, U, F, F]
  | sel (o q) 6 == turnedColor tc Yellow &&
    sel (w q) 6 == turnedColor tc Red = [U', L, F', L']

  | sel (g q) 2 == turnedColor tc Red &&
    sel (y q) 8 == turnedColor tc Yellow = [L', D', L, D]
  | sel (g q) 2 == turnedColor tc Yellow &&
    sel (y q) 8 == turnedColor tc Red = [L', F']
  | sel (g q) 6 == turnedColor tc Red &&
    sel (w q) 8 == turnedColor tc Yellow = [U', F, F]
  | sel (g q) 6 == turnedColor tc Yellow &&
    sel (w q) 8 == turnedColor tc Red = [L, F', L']

  | otherwise = error "setRY"

setYGR tc q
  | sel (r q) 1 == turnedColor tc Yellow &&
    sel (g q) 3 == turnedColor tc Red = [Y', R, U, R', U', R, U, R', Y]
  | sel (r q) 1 == turnedColor tc Green &&
    sel (g q) 3 == turnedColor tc Yellow = [Y', U, R, U', R', U, R, U', R', Y]
--
  | sel (r q) 3 == turnedColor tc Yellow &&
    sel (b q) 1 == turnedColor tc Green = [R, U, R', Y', R, U, R', U', R, U, R', U', R, U, R', Y]
  | sel (r q) 3 == turnedColor tc Red &&
    sel (b q) 1 == turnedColor tc Yellow = [R, U, R', Y', R, U, R', Y]
  | sel (r q) 3 == turnedColor tc Green &&
    sel (b q) 1 == turnedColor tc Red = [R, U, R', Y', U, R, U', R', Y]
--
  | sel (r q) 5 == turnedColor tc Green &&
    sel (b q) 7 == turnedColor tc Yellow = [U, Y', R, U, R', U', Y]  
  | sel (r q) 5 == turnedColor tc Yellow &&
    sel (b q) 7 == turnedColor tc Red = [U, Y', U, R, U', R', Y]
  | sel (r q) 5 == turnedColor tc Red &&
    sel (b q) 7 == turnedColor tc Green = [U, Y', R, U, R', U', R, U, R', U', R, U, R', Y]
--
  | sel (r q) 7 == turnedColor tc Red &&
    sel (g q) 5 == turnedColor tc Yellow = [Y', U, R, U', R', Y]
  | sel (r q) 7 == turnedColor tc Green &&
    sel (g q) 5 == turnedColor tc Red = [Y', R, U, R', U', R, U, R', U', R, U, R', Y]
  | sel (r q) 7 == turnedColor tc Yellow &&
    sel (g q) 5 == turnedColor tc Green = [Y', R, U, R', Y]
--
  | sel (b q) 3 == turnedColor tc Green &&
    sel (o q) 1 == turnedColor tc Red = [Y, R, U, R', U, Y2, U, R, U', R', Y]
  | sel (b q) 3 == turnedColor tc Yellow &&
    sel (o q) 1 == turnedColor tc Green = [Y, R, U, R', U, Y2, R, U, R', U', R, U, R', U', R, U, R', Y]
  | sel (b q) 3 == turnedColor tc Red &&
    sel (o q) 1 == turnedColor tc Yellow = [Y, R, U, R', U, Y2, R, U, R', Y]
--
  | sel (b q) 5 == turnedColor tc Green &&
    sel (o q) 7 == turnedColor tc Yellow = [U, U, Y', R, U, R', Y]
  | sel (b q) 5 == turnedColor tc Red &&
    sel (o q) 7 == turnedColor tc Green = [U, U, Y', R, U, R', U', R, U, R', U', R, U, R', Y]
  | sel (b q) 5 == turnedColor tc Yellow &&
    sel (o q) 7 == turnedColor tc Red = [U, U, Y', U, R, U', R', Y]
--
  | sel (o q) 3 == turnedColor tc Red &&
    sel (g q) 1 == turnedColor tc Yellow = [Y, Y, R, U', R', U', Y, R, U, R', U', R, U, R', U', R, U, R', Y]
  | sel (o q) 3 == turnedColor tc Yellow &&
    sel (g q) 1 == turnedColor tc Green = [Y, Y, R, U', R', Y, R, U, R', Y]
  | sel (o q) 3 == turnedColor tc Green &&
    sel (g q) 1 == turnedColor tc Red = [Y, Y, R, U', R', U', Y, R, U, R', Y]
--
  | sel (o q) 5 == turnedColor tc Yellow &&
    sel (g q) 7 == turnedColor tc Red = [Y', R, U, R', Y]
  | sel (o q) 5 == turnedColor tc Green &&
    sel (g q) 7 == turnedColor tc Yellow = [Y', U', R, U, R', Y]
  | sel (o q) 5 == turnedColor tc Red &&
    sel (g q) 7 == turnedColor tc Green = [Y', U', R, U, R', U', R, U, R', U', R, U, R', Y]

  | otherwise = error "setYGR"

setGR tc q
  | sel (r q) 8 == turnedColor tc Green &&
    sel (g q) 4 == turnedColor tc Red = [U, F, U', F', U', L, U, L', U', F, U', F', U', L', U, L]

  | sel (r q) 6 == turnedColor tc Red &&
    sel (w q) 2 == turnedColor tc Green = [Y', U', F', U, F, U, R, U', R', Y]
  | sel (r q) 6 == turnedColor tc Green &&
    sel (w q) 2 == turnedColor tc Red = [Y', U, U, R, U', R', U', F', U, F, Y]

  | sel (r q) 4 == turnedColor tc Green &&
    sel (b q) 8 == turnedColor tc Red = [U, R, U', R', U', F', U, F, U, L', U, L, U, F, U', F']
  | sel (r q) 4 == turnedColor tc Red &&
    sel (b q) 8 == turnedColor tc Green = [U, R, U', R', U', F', U, F, F, U', F', U', L', U, L]
--
  | sel (b q) 4 == turnedColor tc Green &&
    sel (o q) 8 == turnedColor tc Red = [Y, U, R, U', R', U', R, U, R', Y', U, U, L', U, L, U, F, U', F']
  | sel (b q) 4 == turnedColor tc Red &&
    sel (o q) 8 == turnedColor tc Green = [Y, U, R, U', R', U', R, U, R', Y', U, F, U', F', U', L', U, L]
  
  | sel (b q) 6 == turnedColor tc Red &&
    sel (w q) 4 == turnedColor tc Green = [L', U, L, U, F, U', F']
  | sel (b q) 6 == turnedColor tc Green &&
    sel (w q) 4 == turnedColor tc Red = [U', F, U', F', U', L', U, L]
--
  | sel (o q) 6 == turnedColor tc Red &&
    sel (w q) 6 == turnedColor tc Green = [U, L', U, L, U, F, U', F']
  | sel (o q) 6 == turnedColor tc Green &&
    sel (w q) 6 == turnedColor tc Red = [F, U', F', U', L', U, L]

  | sel (o q) 4 == turnedColor tc Red &&
    sel (g q) 8 == turnedColor tc Green = [Y, Y, U, R, U', R', U', F', U, F, Y, Y, U, U, F, U', F', U', L', U, L]
  | sel (o q) 4 == turnedColor tc Green &&
    sel (g q) 8 == turnedColor tc Red = [Y, Y, U, R, U', R', U', F', U, F, Y, Y, U', L', U, L, U, F, U', F']
--
  | sel (g q) 6 == turnedColor tc Red &&
    sel (w q) 8 == turnedColor tc Green = [U, U, L', U, L, U, F, U', F']
  | sel (g q) 6 == turnedColor tc Green &&
    sel (w q) 8 == turnedColor tc Red = [U, F, U', F', U', L', U, L]

  | otherwise = error "setGR"


zeroToThree tc q
  | sel (w q) 2 /= turnedColor tc White &&
    sel (w q) 4 /= turnedColor tc White &&
    sel (w q) 6 /= turnedColor tc White &&
    sel (w q) 8 /= turnedColor tc White = [F, R, U, R', U', F']
  | otherwise = []

threeToFive tc q
  | sel (w q) 2 == turnedColor tc White &&
    sel (w q) 4 == turnedColor tc White &&
    sel (w q) 6 /= turnedColor tc White &&
    sel (w q) 8 /= turnedColor tc White = [B, U, L, U', L', B']
  | sel (w q) 2 /= turnedColor tc White &&
    sel (w q) 4 == turnedColor tc White &&
    sel (w q) 6 == turnedColor tc White &&
    sel (w q) 8 /= turnedColor tc White = [U, B, U, L, U', L', B']
  | sel (w q) 2 /= turnedColor tc White &&
    sel (w q) 4 /= turnedColor tc White &&
    sel (w q) 6 == turnedColor tc White &&
    sel (w q) 8 == turnedColor tc White = [U, U, B, U, L, U', L', B']
  | sel (w q) 2 == turnedColor tc White &&
    sel (w q) 4 /= turnedColor tc White &&
    sel (w q) 6 /= turnedColor tc White &&
    sel (w q) 8 == turnedColor tc White = [U', B, U, L, U', L', B']
  | sel (w q) 2 /= turnedColor tc White &&
    sel (w q) 4 == turnedColor tc White &&
    sel (w q) 6 /= turnedColor tc White &&
    sel (w q) 8 == turnedColor tc White = [F, R, U, R', U', F']
  | sel (w q) 2 == turnedColor tc White &&
    sel (w q) 4 /= turnedColor tc White &&
    sel (w q) 6 == turnedColor tc White &&
    sel (w q) 8 /= turnedColor tc White = [U, F, R, U, R', U', F']
  | otherwise = []

fiveToNine tc q
  | sel (r q) 5 == turnedColor tc White &&
    sel (o q) 7 == turnedColor tc White &&
    sel (g q) 5 == turnedColor tc White &&
    sel (g q) 7 == turnedColor tc White = [R, U, U, R', R', U', R, R, U', R', R', U, U, R]
  | sel (w q) 1 == turnedColor tc White &&
    sel (w q) 3 == turnedColor tc White &&
    sel (o q) 5 == turnedColor tc White &&
    sel (o q) 7 == turnedColor tc White = [R, R, D', R, U, U, R', D, R, U, U, R]
  | sel (r q) 5 == turnedColor tc White &&
    sel (b q) 5 == turnedColor tc White &&
    sel (o q) 5 == turnedColor tc White &&
    sel (w q) 1 == turnedColor tc White = [R, U, R', U, R, U', U', R']
  | sel (r q) 7 == turnedColor tc White &&
    sel (b q) 7 == turnedColor tc White &&
    sel (g q) 7 == turnedColor tc White &&
    sel (w q) 5 == turnedColor tc White = [R, U', U', R', U', R, U', R]
  | sel (r q) 7 == turnedColor tc White &&
    sel (r q) 5 == turnedColor tc White &&
    sel (o q) 7 == turnedColor tc White &&
    sel (o q) 5 == turnedColor tc White = [R, U', U', R', U', R, U, R', U', R, U', R']
  | sel (r q) 7 == turnedColor tc White &&
    sel (b q) 5 == turnedColor tc White &&
    sel (w q) 3 == turnedColor tc White &&
    sel (w q) 7 == turnedColor tc White = [L, F', F', R', R', D, R, D', R, F', F', L']
  | sel (r q) 7 == turnedColor tc White &&
    sel (w q) 3 == turnedColor tc White &&
    sel (w q) 5 == turnedColor tc White &&
    sel (o q) 5 == turnedColor tc White = [L, F, R', F', L', F, R, F']

nineToFinish q
  | sel (r q) 6 == sel (r q) 7 &&
    sel (g q) 5 == sel (g q) 7 &&
    sel (g q) 5 /= sel (g q) 6 &&
    sel (o q) 5 == sel (o q) 6 = [R, U, R', U', R', F, R, R, U', R, U', R, U, R', F] -- T-perm

  | sel (o q) 5 == sel (o q) 6 &&
    sel (o q) 5 == sel (o q) 7 &&
    sel (o q) 5 /= sel (o q) 6 &&
    sel (r q) 5 == sel (r q) 7 = if sel (r q) 5 == sel (b q) 6 then [R, R, U, R, U, R', U', R', U', R', U, R']
                                 else [R, U', R, U, R, U, R, U', R', U', R', R']

  | sel (r q) 6 == sel (r q) 7 &&
    sel (b q) 5 == sel (b q) 6 &&
    sel (r q) 7 == sel (o q) 5 = [F, R, U', R', U', R, U, R', F', R, U, R', U', R', F, R, F']

  | sel (r q) 5 == sel (r q) 7 &&
    sel (r q) 5 == sel (b q) 6 &&
    sel (r q) 6 == sel (b q) 7 =
    if sel (b q) 7 /= sel (b q) 5 then [R', U', U', R, U', U', R', F, R, U, R', U', R', F', R, R, U']
    else [R', L, F', R, R, L', L', B', R, R, L', L', F', R, L', D, D, R, R, L', L', U]
  | sel (r q) 5 == sel (r q) 7 &&
    sel (r q) 5 == sel (g q) 6 &&
    sel (r q) 6 == sel (g q) 5 &&
    sel (g q) 7 /= sel (g q) 5 = [L, U, U, L', U, U, L, F', L', U', L, U, L, F, L', L', U]

  | sel (r q) 5 == sel (r q) 7 &&
    sel (b q) 5 == sel (b q) 7 &&
    sel (r q) 5 == sel (o q) 6 &&
    sel (b q) 5 == sel (g q) 6 = [M, M, U', M, M, U', U', M, M, U', M, M]

  | sel (r q) 5 == sel (r q) 6 &&
    sel (r q) 6 /= sel (r q) 7 &&
    sel (g q) 5 == sel (g q) 7 = 
    if sel (g q) 5 /= sel (g q) 6 then [R, R, D, Y, R', U, R', U', R, D', Y', R', R', F', U, F]
    else [R, U, R', F', R, U, R', U', R', F, R, R, U', R', U']
  | sel (r q) 7 == sel (r q) 6 &&
    sel (r q) 6 /= sel (r q) 5 &&
    sel (b q) 5 == sel (b q) 7 = 
    if sel (b q) 5 /= sel (b q) 6 then [L', L', D', Y', L, U', L, U, L', D, Y, L, L, F, U', F']
    else [L', U', L, F, L', U', L, U, L, F', L', L', U, L, U]

  | sel (r q) 5 == sel (r q) 6 &&
    sel (o q) 5 == sel (o q) 7 &&
    sel (o q) 7 == sel (b q) 6 = [F', U', F, R, R, D, Y, R', U, R, U', R, D', Y', R, R]
  | sel (r q) 7 == sel (r q) 6 &&
    sel (o q) 5 == sel (o q) 7 &&
    sel (o q) 5 == sel (g q) 6 = [F, U, F', L', L', D', Y', L, U', L', U, L', D, Y, L', L']

  | sel (r q) 7 == sel (b q) 5 &&
    sel (r q) 5 == sel (b q) 6 &&
    sel (r q) 6 == sel (b q) 7 &&
    sel (g q) 6 == sel (g q) 5 = [R', U', F', R, U, R', U', R', F, R, R, U', R', U', R, U, R', U, R]

  | sel (r q) 6 == sel (r q) 7 &&
    sel (g q) 5 == sel (g q) 6 &&
    sel (g q) 5 == sel (b q) 7 = [R', U, R', U', Y, R', F', R, R, U', R', U, R', F, R, F]

  | otherwise = error "nineToFinish"



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
  
