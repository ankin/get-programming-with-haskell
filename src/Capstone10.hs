module Capstone10 where

cup flOz = \_ -> flOz
cup flOz = \message -> message flOz

coffeeCup = cup 12

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank =
  if ozDiff >= 0
    then cup ozDiff
    else cup 0
  where
    flOz = getOz aCup
    ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0

robot :: (a, b, c) -> ((a, b, c) -> t) -> t
robot (name, attack, hp) = \message -> message (name, attack, hp)

-- robot (name, attack, hp) message = message (name, attack, hp)

robotName (n, _, _) = n

attack (_, a, _) = a

hp (_, _, h) = h

getName :: (((a, b, c) -> a) -> t) -> t
getName aRobot = aRobot robotName

getAttack aRobot = aRobot attack

setAttack aRobot newAttack = aRobot (\(n, _, h) -> robot (n, newAttack, h))

getHp aRobot = aRobot hp

setHp aRobot newHp = aRobot (\(n, a, _) -> robot (n, a, newHp))

printStats aRobot = aRobot (\(name, attack, hp) -> name ++ " attack:" ++ (show attack) ++ " hp:" ++ (show hp))

damage aRobot damage = aRobot (\(name, attack, hp) -> robot (name, attack, hp - damage))

fight :: Num c1 => (((a1, b1, c2) -> b1) -> c1) -> (((a2, b2, c1) -> ((a2, b2, c1) -> t1) -> t1) -> t2) -> t2
fight aRobot defender = damage defender attackPoints
  where
    attackPoints = getAttack aRobot

--threeRoundFight a b = if hp aFinal > hp bFinal then aFinal else bFinal
--  where
--    (aFinal, bFinal) =
--      (\(r1, r2) -> (fight r2 r1, fight r1 r2))
--        ( (\(r1, r2) -> (fight r2 r1, fight r1 r2))
--            ((\(r1, r2) -> (fight r2 r1, fight r1 r2)) (a, b))
--        )

--  let robotBResult = fight robotA (fight robotA (fight robotA robotB))
--                                     robotAResult = fight robotB (fight robotB (fight robotB robotA))
--     in if getHp robotAResult > getHp robotBResult
--          then robotAResult
--          else robotBResult

--
--  if getHp resRobotA > getHp resRobotB
--    then resRobotA
--    else resRobotB
--  where
--    resRobotB = robotA (\t -> fight (robot t) robotB)
--    resRobotA = fight robotB robotA
