{-# LANGUAGE ScopedTypeVariables #-}
module Main where {
import qualified Data.Set as Set;
import Data.Set(Set);
import qualified Data.Map as Map;
import Data.Map(Map);
-- import Debug.Trace;
import Data.List;

-- type Trie a = Set (Node a);
-- data Node a = Node (a,(Trie a)) deriving (Ord, Eq, Show);
data Trie a = Trie (Map a (Trie a)) deriving (Show);
type Ctrie = Trie Char;


main :: IO();
main = undefined;

test_alphabet :: Set Char;
test_alphabet = Set.fromList "abc";

tsingleton :: a -> Trie a;
tsingleton x = Trie $ Map.singleton x empty_trie;

empty_trie :: Trie a;
empty_trie = Trie $ Map.empty;

unTrie :: Trie a -> Map a (Trie a);
unTrie (Trie m) = m;

initial :: Ctrie;
initial = flat $ Set.toList test_alphabet;

flat :: String -> Ctrie;
flat = Trie . Map.unions . map (unTrie .tsingleton) ;

modifications :: forall a . (Ord a) => Set a -> Trie a -> [Trie a];
modifications alphabet (Trie t) = do {
c :: a <- Set.toList $ Set.difference alphabet $ Map.keysSet $ t;
return $ Trie $ Map.insert c empty_trie t;
} ++ do {
(k::a, v::Trie a) <- Map.assocs t;
new :: Trie a <- modifications alphabet v;
return $ Trie $ Map.insert k new t;
};

all_strings :: Trie a -> [[a]];
all_strings = concatMap all_strings_from . Map.assocs . unTrie;

all_strings_from :: (a, Trie a) -> [[a]];
all_strings_from (x,t) = [[x]]++do {
  more <- all_strings t;
  return $ x:more;
};

-------------------

-- not exactly the best way given all the redundant evaluations
improve_1 :: forall a score . (Ord score) => a -> (a -> [a]) -> (a -> score) -> Maybe a;
improve_1 start nexts eval = let {
s :: a -> a -> Ordering;
s x y = compare (eval x) (eval y);
best :: a;
best = maximumBy s $ nexts start;
} in if eval best > eval start
then Just best else Nothing;

keep_improving :: (Ord score) => a -> (a -> [a]) -> (a -> score) -> a;
keep_improving start nexts eval = case improve_1 start nexts eval of {
Just new -> keep_improving new nexts eval;
Nothing -> start
};

-----------------

longest_prefix :: Trie a -> [a] -> ([a],[a]);
longest_prefix _ [] = ([],[]);
longest_prefix t (h:rest) = undefined;
}
