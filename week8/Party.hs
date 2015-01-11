module Party where

import Data.Tree
import Data.Monoid
import Debug.Trace
import Employee


-----------------------
-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e gl = GL (e : employees gl) (fun gl + empFun e)

employees (GL emps _) = emps
fun (GL _ f) = f

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 > f2 = gl1
  | otherwise = gl2


----------------------
-- Exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a []) = f a []
treeFold f (Node a forest) = f a (map (treeFold f) forest)

---------------------
-- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gls = (glCons e glWithoutBosses, glWithBosses)
  where unzipped = unzip gls 
        glWithBosses = mconcat (fst unzipped)
        glWithoutBosses = mconcat (snd unzipped)

---------------------
-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun employees = pickMax $ treeFold nextLevel employees

pickMax (g1@(GL _ f1), g2@(GL _ f2))
  | f1 > f2 = g1
  | otherwise = g2



---------------------
-- Exercise 5

main :: IO ()
main = readFile "company.txt" >>= \content ->
  let gl = (maxFun (read content))
      empStrs = makeEmpStrs gl
      funStr = makeFunStr gl
  in putStrLn funStr >> putStrLn (unlines empStrs)

makeEmpStrs :: GuestList -> [String]
makeEmpStrs gl = map empName (employees gl)

makeFunStr :: GuestList -> String
makeFunStr gl = "Total fun: " ++ funStr
  where funStr = show $ fun gl
