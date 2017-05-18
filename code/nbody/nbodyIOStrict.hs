{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- import Text.ParserCombinators.Parsec
-- import Data.CSV
-- import System.IO
import Data.List
import Data.List.Split
import Data.Time
import Control.DeepSeq

softening = 1e-9

-------------------------------------------

integrateBody :: Triple -> Triple -> Float -> Triple
integrateBody !(px, py, pz) !(vx, vy, vz) dt = (px + vx * dt, py + vy * dt, pz + vz * dt)

integrate :: [Triple] -> [Triple] -> Float -> [Triple]
integrate poss vels dt = integrate' dt poss vels

integrate' dt = integrate''
  where
    integrate'' [] [] = []
    integrate'' (p:poss) (v:vels) = r1 `deepseq` (r1 : integrate'' poss vels)
       where r1 = integrateBody p v dt

-- vels dt = zipWith (\p v -> integrateBody p v dt) poss vels
-- integrate poss vels dt = zipWith (\p v -> integrateBody p v dt) poss vels

velocitiesBody :: Triple -> Triple -> Float -> Triple
velocitiesBody frc vel dt =
    (x vel + x frc * dt
    ,y vel + y frc * dt
    ,z vel + z frc * dt)

velocities :: [Triple] -> [Triple] -> Float -> [Triple]
velocities frcs vels dt = integrate vels frcs dt
  -- frcs vels dt = zipWith (\f v -> velocitiesBody f v dt) frcs vels

-------------------------------------------

calcForceBody :: Triple -> Triple -> Triple -> Triple
calcForceBody p@(px, py, pz) (ppx, ppy, ppz) (fx, fy, fz) = fi
 where
  dx = ppx - px   -- (x pos) - (x p)
  dy = ppy - py   -- (y pos) - (y p)
  dz = ppz - pz   -- (z pos) - (z p)
  distSqr = dx*dx + dy*dy + dz*dz + softening
  invDist = 1.0 / sqrt(distSqr);
  invDist3 = invDist * invDist * invDist
  fix = fx + (dx * invDist3)
  fiy = fy + (dy * invDist3)
  fiz = fz + (dz * invDist3)
  fi = fix `deepseq` fiy `deepseq` fiz `deepseq` (fix, fiy, fiz)

calcForce :: [Triple] -> Triple -> Triple
calcForce poss p = calcForce' p poss

calcForce' p = foldl' (\a b ->  let i = calcForceBody p a b in a `deepseq` b `deepseq` i `deepseq` i) tripleNull

foldl'' :: NFData b => (b -> a -> b) -> b -> [a] -> b
foldl'' f a []     = a
foldl'' f a (x:xs) = let !a' = f a x
                     in a' `deepseq` foldl'' f a' xs

bodyForce :: [Triple] -> [Triple]
bodyForce poss = map (calcForce poss) poss

-------------------------------------------

bodyLoop :: [Triple] -> [Triple] -> Float -> ([Triple], [Triple])
bodyLoop poss vels dt = frcs `deepseq` vels' `deepseq` poss' `deepseq` (poss', vels')
  where
    frcs  = bodyForce poss
    vels' = velocities frcs vels dt
    poss' = integrate poss vels' dt


loop :: [Triple] -> [Triple] -> Int -> [Triple]
loop  poss vels 0 = poss
loop  poss vels n = poss' `deepseq` vels' `deepseq` loop poss' vels' (n-1)
  where (poss', vels') = bodyLoop poss vels 0.1
-- loop  (poss, vels) n = lastPos
--   where
--     (lastPos, _, _) = iterate bodyLoop (poss, vels, 0.1) !! n

-------------------------------------------


main = do
  x <- readFile "pos.csv"
  y <- readFile "vel.csv"
  let iposs =  splits commaSplit (lines x)
  let ivels =  splits commaSplit (lines y)
  let genp =  generateTList iposs
  let genv =  generateTList ivels
  genp `deepseq` genv `deepseq` print "START"
  stime <- getCurrentTime
  print stime

  let res1 = loop genp genv 2
  etime <- res1 `deepseq` getCurrentTime
  putStrLn $ tripleListShow res1

  print "STOP"
  print stime
  print etime


-- let {generateT :: Float -> Triple; generateT n = Triple (n) (n) (n)}
generateTE :: Float -> Triple
generateTE n =  (n, n, n)

generateT :: [Float] -> Triple
generateT (x:y:z:_) =  (x,y,z)

generateTList :: [[Float]] -> [Triple]
generateTList ns =  map generateT ns

convertSF :: String -> Float
convertSF s =  read s :: Float

commaSplit :: String -> [Float]
commaSplit s =  map convertSF (splitOn "," s)

splits :: (String -> [Float]) -> [String] -> [[Float]]
splits func ss =  map func ss

type Triple = (Float, Float, Float)

x :: (a,b,c) -> a
x (a,_,_) = a

y :: (a,b,c) -> b
y (_,b,_) = b

z :: (a,b,c) -> c
z (_,_,c) = c

tripleNull :: Triple
tripleNull = (0, 0, 0)

tripleShow :: Triple -> String
-- tripleShow t = "(" ++ (show (x t)) ++ "," ++ (show (y t)) ++ "," ++ (show (z t)) ++ ")"
tripleShow t = show (x t, y t, z t)

_ta :: String -> Triple -> String
_ta s t = s ++ " " ++ tripleShow(t)

tripleListShow :: [Triple] -> String
-- tripleListShow l = foldl _ta "" l
tripleListShow l = intercalate " " strings
 where strings :: [String]
       strings = map tripleShow l
-- _ta "" l
