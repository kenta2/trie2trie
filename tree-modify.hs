{-# LANGUAGE ScopedTypeVariables #-}
module Main where {
import qualified Data.Set as Set;
import Data.Set(Set);
-- import Debug.Trace;
import Data.List;

type Trie a = Set (Node a);
data Node a = Node (a,(Trie a)) deriving (Ord, Eq, Show);
type Ctrie = Trie Char;

test_alphabet :: Set Char;
test_alphabet = Set.fromList "abc";

tsingleton :: a -> Node a;
tsingleton x = Node (x,Set.empty);

initial :: Ctrie;
initial = Set.map tsingleton test_alphabet;

test :: String -> Ctrie;
test s = Set.map tsingleton $ Set.fromList s;

getval :: Node a -> (a,Trie a);
getval (Node x) = x;

modifications :: forall a . (Ord a) => Set a -> Trie a -> [Trie a];
modifications alphabet t = do {
c :: a <- Set.toList $ Set.difference alphabet $ Set.map (fst.getval) t;
return $ Set.insert (tsingleton c) t;
} ++ do {
let {
g1 :: Node a -> (Node a, (Trie a,Trie a));
g1 n = (n, Set.split n t);
};
((Node ((v::a),(children::Trie a))),(l::Trie a,r::Trie a)) <- map g1 $ Set.toList t;
-- traceM $ "\nnow evaluating " ++ show qq;
c1 :: Trie a <- modifications alphabet children;
-- traceM $ "\nand c1 " ++ show c1;
return $ Set.insert (Node (v,c1)) $ Set.union l r;
-- return $ Set.union l r;
}
;

all_strings :: Trie a -> [[a]];
all_strings = concatMap all_strings_from . Set.toList;

all_strings_from :: Node a -> [[a]];
all_strings_from (Node (x,t)) = [[x]]++do {
  more <- all_strings t;
  return $ x:more;
};

-------------------

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


main :: IO();
main = undefined;
}
