module Lib
    ( solve, Person, Table, Class, showSolution
    ) where

import Data.Traversable (for)
import Data.List (intercalate)
import Control.Monad (MonadPlus (..), forM_, guard)
import Debug.Trace (trace)

-- | A Type to represent a person, just for ease of change
newtype Person = Person Int deriving (Eq, Show)
-- | A Type to represent an incomplete table. It can have up to 4 people in it, but this is not enforced.  
type Table = [Person]
-- | A type to represent a class. It requires 7 tables, but this is not enforced. 
type Class = [Table]

addOne :: Person -> Table -> Maybe Table
addOne p table 
  | length table < 4 = Just $ p : table
  | otherwise = Nothing


fromList :: (MonadPlus m) => [a] -> m a
fromList = foldl (flip (mplus . return)) mzero

solve :: Int -> Int -> Int -> Table -> Class -> [Class] -> [[Class]]
solve tableSize classSize numClasses = solve' 
    where
        allPeople :: [Person]
        allPeople = map Person [1..tableSize * classSize]

        solve' :: Table -> Class -> [Class] -> [[Class]]
        solve' incTable incClass madeClasses = {- trace (show (length madeClasses) ++ show incClass ++ show incTable ++ "\n\n") $  -}do
            person <- filter (\p -> p `notElem` incTable && p `notElem` concat incClass) $ allPeople
            trace ("Person: " ++ showPerson person ++ "\nTable: " ++ showTable incTable ++ "\nClass: " ++ showClass incClass ++ "\nMadeClasses: \n" ++ showSolution madeClasses ++ "\n--------\n\n") [()]
            let madeTables = filter (elem person) . concat $ madeClasses -- all Tables that have person in them
            -- trace (show person) [()]
            forM_ incTable (\partner ->
                (guard $ null $ filter (elem partner) madeTables))
            
            if length incTable == tableSize - 1 then 
                if length incClass == classSize - 1 then
                    if length madeClasses == (numClasses - 1) then
                        return (((person:incTable):incClass):madeClasses)
                    else solve' [] [] (((person:incTable):incClass):madeClasses)
                else solve' [] ((person:incTable):incClass) madeClasses
            else solve' (person:incTable) incClass madeClasses
            
            {- 
            let incTable' = person : incTable in if length incTable' == 4 then 
                let incClass' = incTable' : incClass in if length incClass' == 7 then
                    let madeClasses' = incClass' : madeClasses in if length madeClasses' == 9 then return madeClasses'
                    else solve' [] [] madeClasses'
                else solve' [] incClass' madeClasses
            else solve' incTable' incClass madeClasses -}
    
showSolution :: [Class] -> String
showSolution sol = intercalate "\n" $ map showClass sol

showClass :: Class -> String
showClass cl = intercalate " | " $ map showTable cl

showTable :: Table -> String
showTable table = intercalate " " $ map showPerson table

showPerson :: Person -> String
showPerson (Person p) = show p