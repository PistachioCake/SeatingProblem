module Lib
    ( solve, Person, Table, Class, showSolution
    ) where

import Data.Traversable (for)
import Data.List (intercalate)
import Control.Monad (MonadPlus (..), forM_, guard)
import Debug.Trace (trace)

-- * Types
-- | A Type to represent a person, just for ease of change
newtype Person = Person Int deriving (Eq, Show)
-- | A Type to represent an incomplete table. It can have up to 4 people in it, but this is not enforced.  
type Table = [Person]
-- | A type to represent a class. It requires 7 tables, but this is not enforced. 
type Class = [Table]

-- * Unused Utility Functions!
-- | Utility function to add a person to a table
addOne :: Person -> Table -> Maybe Table
addOne p table 
  | length table < 4 = Just $ p : table
  | otherwise = Nothing

-- | Transforming a list (the free MonadPlus) into any other MonadPlus.
--   This could conceivably be opened to any monoid, since a list is the free monoid.
fromList :: (MonadPlus m) => [a] -> m a
fromList = foldl (flip (mplus . return)) mzero

-- * Used Utility Functions!

-- * The Actual Solution

-- Note: the Table, Class, and [Class] arguments should all be initialized as []. They are left for ease of debugging. 
-- | The function for the solution of the problem.
solve :: Int -- ^ the size of each table
      -> Int -- ^ the size of each class (in tables)
      -> Int -- ^ the number of classes to calculate (should be (tableSize * classSize - 1) `div` (tableSize - 1)
      -> Table -- ^ the current, incomplete table
      -> Class -- ^ the current, incomplete class
      -> [Class] -- ^ the current list of complete past classes
      -> [[Class]] -- ^ the final result - a list of possible arrangments of students
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

            -- The next two lines do ALL the work for this problem.
            forM_ incTable (\partner ->
            -- ^for each partner,
                (guard $ null $ filter (elem partner) madeTables))
                -- ^keep only the ones which have NO tables in common with person.
            
            if length incTable == tableSize - 1 then 
                if length incClass == classSize - 1 then
                    if length madeClasses == (numClasses - 1) then
                        return (((person:incTable):incClass):madeClasses)
                        -- ^Just return the full list, we're done here
                    else solve' [] [] (((person:incTable):incClass):madeClasses) 
                         -- ^Adding the class to the list of completed classes
                else solve' [] ((person:incTable):incClass) madeClasses 
                     -- ^Adding the table to the class
            else solve' (person:incTable) incClass madeClasses
                 -- ^Adding the person to the table
            
            {- --This does the same as above and looks a little nicer. Maybe we should switch them? But it creates lots of intermediate lists...
            let incTable' = person : incTable in if length incTable' == 4 then 
                let incClass' = incTable' : incClass in if length incClass' == 7 then
                    let madeClasses' = incClass' : madeClasses in if length madeClasses' == 9 then return madeClasses'
                    else solve' [] [] madeClasses'
                else solve' [] incClass' madeClasses
            else solve' incTable' incClass madeClasses -}
    
-- * Pretty Printing

-- | Pretty print a list of classes
showSolution :: [Class] -> String
showSolution sol = intercalate "\n" $ map showClass sol

-- | Pretty print a single class
showClass :: Class -> String
showClass cl = intercalate " | " $ map showTable cl

-- | Pretty print a table
showTable :: Table -> String
showTable table = intercalate " " $ map showPerson table

-- | Pretty print a person
showPerson :: Person -> String
showPerson (Person p) = show p