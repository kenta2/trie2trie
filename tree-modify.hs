{-# LANGUAGE ScopedTypeVariables #-}
module Main where {
import qualified Data.Set as Set;
import Data.Set(Set);
import qualified Data.Map as Map;
import Data.Map(Map);
-- import Debug.Trace;
import Data.List;
import HuffmanNAry;
import Data.Maybe;

-- cycle in type synonym declarations
-- type Foo a = Map a (Foo a);

newtype Trie a = Trie {unTrie :: (Map a (Trie a))} deriving (Show);
type Ctrie = Trie Char;


main :: IO();
main = undefined;

alphabet :: String;
alphabet = "abc";
set_alphabet :: Set Char;
set_alphabet = Set.fromList alphabet;

tsingleton :: a -> Trie a;
tsingleton x = Trie $ Map.singleton x empty_trie;

empty_trie :: Trie a;
empty_trie = Trie $ Map.empty;

initial :: Ctrie;
initial = flat alphabet;

flat :: String -> Ctrie;
flat = Trie . Map.unions . map (unTrie .tsingleton) ;

modifications :: forall a . (Ord a) => Set a -> Trie a -> [Trie a];
modifications my_alphabet (Trie t) = do {
c :: a <- Set.toList $ Set.difference my_alphabet $ Map.keysSet $ t;
return $ Trie $ Map.insert c empty_trie t;
} ++ do {
(k::a, v::Trie a) <- Map.assocs t;
new :: Trie a <- modifications my_alphabet v;
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

longest_prefix :: forall a. (Ord a) => Trie a -> [a] -> ([a],[a]);
longest_prefix _ [] = ([],[]);
longest_prefix (Trie t) s@(h:rest) = case Map.lookup h t of {
Nothing -> ([],s);
Just subt -> let { (x :: [a],y :: [a]) = longest_prefix subt rest; } in
(h:x,y);
};

break_by_prefix :: forall a. (Ord a) => Trie a -> [a] -> [[a]];
break_by_prefix _ [] = [];
break_by_prefix t s = let {
(x,y) = longest_prefix t s;
} in x:break_by_prefix t y;

-- all_elems :: forall a. (Ord a) => Trie a -> [a] -> [[a]];
-- all_elems t = concatMap (tail . inits) . break_by_prefix t;

counts :: (Ord a) => [a] -> Map a Integer;
counts = Map.unionsWith (+) . map (\x -> Map.singleton x 1);

p1word1 :: (Ord a) => Trie a -> ([a],Integer) -> Map [a] Integer;
p1word1 t (s,n) = Map.map (*n) $ counts $ break_by_prefix t s;

p1words :: (Ord a) => Trie a -> [([a],Integer)] -> Map [a] Integer;
p1words t = Map.unionsWith (+) . map (p1word1 t);

dummies :: [(String,Integer)];
dummies = map (\c -> ([c],1)) alphabet;

phase1 :: Ctrie -> [(String,Integer)] -> Map String Integer;
phase1 t l = p1words t $ dummies ++ l;

hcounts :: (Ord a, Ord w, Num w) => [(a,w)] -> Map a Int;
hcounts l = huffman_depths (HuffmanArity 3) l;

hscore :: forall a . (Ord a) => [(a,Integer)] -> Integer;
hscore l = let {
table :: Map a Int;
table = hcounts l;
gettable :: a -> Integer;
gettable a = fromIntegral $ fromJust $ Map.lookup a table;
eval :: (a,Integer) -> Integer;
eval (s,n) = n*gettable s;
} in sum $ map eval l;

}
