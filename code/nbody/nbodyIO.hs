-- import Text.ParserCombinators.Parsec
-- import Data.CSV
-- import System.IO
import Data.List.Split
import Data.Time
import Control.DeepSeq

softening = 1e-9

data Triple = Triple { x :: Float, y :: Float, z :: Float }

tripleNull = Triple 0 0 0

tripleShow :: Triple -> String
tripleShow t = "(" ++ (show (x t)) ++ "," ++ (show (y t)) ++ "," ++ (show (z t)) ++ ")"

_ta :: String -> Triple -> String
_ta s t = s ++ " " ++ tripleShow(t)

tripleListShow :: [Triple] -> String
tripleListShow l = foldl _ta "" l 


-- let {generateT :: Float -> Triple; generateT n = Triple (n) (n) (n)}
generateTE :: Float -> Triple
generateTE n = Triple (n) (n) (n)

generateT :: [Float] -> Triple
generateT n = Triple (n !! 0) (n !! 1) (n !! 2)

generateTList :: [[Float]] -> [Triple]
generateTList ns = map generateT ns

-------------------------------------------

integrateBody :: Triple -> Triple -> Float -> Triple
integrateBody pos vel dt =
 Triple
    (x pos + x vel * dt)
    (y pos + y vel * dt)
    (z pos + z vel * dt)

integrate :: [Triple] -> [Triple] -> Float -> [Triple]
integrate poss vels dt = zipWith (\p v -> integrateBody p v dt) poss vels


velocitiesBody :: Triple -> Triple -> Float -> Triple
velocitiesBody frc vel dt =
 Triple
    (x vel + x frc * dt)
    (y vel + y frc * dt)
    (z vel + z frc * dt)

velocities :: [Triple] -> [Triple] -> Float -> [Triple]
velocities frcs vels dt = zipWith (\f v -> velocitiesBody f v dt) frcs vels

-------------------------------------------

calcForceBody :: (Triple, Triple) -> Triple -> (Triple, Triple)
calcForceBody (f, p) pos = (fi, p)
 where
  dx = (x pos) - (x p)
  dy = (y pos) - (y p)
  dz = (z pos) - (z p)
  distSqr = dx*dx + dy*dy + dz*dz + softening
  invDist = 1.0 / sqrt(distSqr);
  invDist3 = invDist * invDist * invDist
  fi = Triple (x f + (dx * invDist3)) (y f + (dy * invDist3)) (z f + (dz * invDist3))
 
calcForce :: ([Triple], Triple) -> Triple
calcForce (poss, p) = fst (foldl calcForceBody (tripleNull, p) poss)


bodyForce :: [Triple] -> [Triple]
bodyForce poss = map calcForce eposs
 where
  eposs = [(poss,i) | i <- poss]

-------------------------------------------

bodyLoop :: ([Triple], [Triple], Float) -> ([Triple], [Triple], Float)
bodyLoop (poss, vels, dt) = (poss', vels', dt)
  where
    frcs  = bodyForce poss
    vels' = velocities frcs vels dt
    poss' = integrate poss vels' dt


loop :: ([Triple], [Triple]) -> Int -> [Triple]
loop  (poss, vels) n = lastPos
  where
    (lastPos, _, _) = iterate bodyLoop (poss, vels, 0.1) !! n

-------------------------------------------


convertSF :: String -> Float
convertSF s = read s :: Float

commaSplit :: String -> [Float]
commaSplit s = map convertSF (splitOn "," s)

splits :: (String -> [Float]) -> [String] -> [[Float]]
splits func ss = map func ss

main = do
  x <- readFile "pos.csv"
  y <- readFile "vel.csv"
  let iposs = splits commaSplit (splitOn "\n" x)
  let ivels = splits commaSplit (splitOn "\n" y)
  -- iposs `deepseq` ivels `deepseq` print "START"
  print "START"
  stime <- getCurrentTime
  print stime

  let result = tripleListShow (loop (generateTList(iposs), generateTList(ivels)) 2)
  etime <- result `deepseq` getCurrentTime

  print result
  print "STOP"
  print stime
  print etime
