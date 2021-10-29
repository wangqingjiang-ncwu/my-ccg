import Prelude hiding (lookup)
import Data.Map
import Data.List

ListIntCount :: List -> Map -> Map
ListIntCount (list countMap) = (ret) where
    if length list == 1
       then
         count <- lookup countMap (list !! 0)
         if count == Nothing
            then
              ret = insertWith (++) (list !! 0) 1 countMap
            else
              ret = insertWith (++) (list !! 0) 1 (count + 1)
    else
       for (int index = 0; index < length list; index += 1)
           ret = ListIntCount (list !! index) countMap
