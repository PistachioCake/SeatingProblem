module Lib
    ( solve, Person, Table, Class
    ) where

import Data.Traversable (for)
import Control.Monad (MonadPlus (..))
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

allPeople :: (MonadPlus m) => m Person
allPeople = foldl (flip (mplus . return)) mzero $ map Person [1..28]

tableSize = 4
classSize = 7
numClasses = 9

solve :: (MonadPlus m) => Table -> Class -> [Class] -> m [Class]
solve incTable incClass madeClasses = {- trace (show (length madeClasses) ++ show incClass ++ show incTable ++ "\n\n") $  -}do
    person <- fromList . filter (\p -> p `notElem` incTable && p `notElem` concat incClass) $ allPeople
    
    let madeTables = filter (elem person) madeClasses

    
    if length incTable == tableSize - 1 then 
        if length incClass == classSize - 1 then
            if length madeClasses == (numClasses - 1) then
                return (((person:incTable):incClass):madeClasses)
            else solve [] [] (((person:incTable):incClass):madeClasses)
        else solve [] ((person:incTable):incClass) madeClasses
    else solve (person:incTable) incClass madeClasses
    
    {- 
    let incTable' = person : incTable in if length incTable' == 4 then 
        let incClass' = incTable' : incClass in if length incClass' == 7 then
            let madeClasses' = incClass' : madeClasses in if length madeClasses' == 9 then return madeClasses'
            else solve [] [] madeClasses'
        else solve [] incClass' madeClasses
    else solve incTable' incClass madeClasses -}
    
