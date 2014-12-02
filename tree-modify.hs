{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Main where {
import qualified Data.Set as Set;
import Data.Set(Set);
import qualified Data.Map as Map;
import Data.Map(Map);
import Debug.Trace;
import Data.List;
import HuffmanNAry;
import Data.Maybe;
import System.Environment;

-- cycle in type synonym declarations
-- type Foo a = Map a (Foo a);

newtype Trie a = Trie {unTrie :: (Map a (Trie a))} deriving (Show);
type Ctrie = Trie Char;


main :: IO();
main = getArgs >>= \case {
["alice",n] -> print $ all_strings $ p1_improve $ take (read n) alice;
_ -> error "unknown args";
};

alphabet :: String;
alphabet = ['a'..'z'];
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
improve_1 :: forall a score . (Ord score, Show score) => a -> (a -> [a]) -> (a -> score) -> Maybe a;
improve_1 start nexts eval = let {
s :: a -> a -> Ordering;
s x y = compare (eval x) (eval y);
best :: a;
best = minimumBy s $ nexts start;
ebest :: score;
ebest = eval best;
} in if ebest < eval start
then trace ("score = " ++ show ebest) $ Just best else Nothing;

keep_improving :: (Show score, Ord score) => a -> (a -> [a]) -> (a -> score) -> a;
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

type Wordcounts = [(String,Integer)];

dummies :: Wordcounts;
dummies = map (\c -> ([c],1)) alphabet;

phase1 :: Ctrie -> Wordcounts -> Map String Integer;
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

eval_trie :: Wordcounts -> Ctrie -> Integer;
eval_trie l t = hscore $ Map.assocs $ phase1 t l;

p1_improve :: Wordcounts -> Ctrie;
p1_improve l = keep_improving initial (modifications set_alphabet) (eval_trie l);

alice :: Wordcounts;
alice = [("the", 9399 ),
("and", 5163 ),
("a", 4558 ),
("to", 4334 ),
("of", 4299 ),
("i", 2813 ),
("in", 2735 ),
("was", 2328 ),
("it", 2178 ),
("that", 1944 ),
("you", 1912 ),
("she", 1872 ),
("her", 1728 ),
("he", 1590 ),
("as", 1452 ),
("said", 1451 ),
("had", 1337 ),
("s", 1304 ),
("his", 1241 ),
("at", 1159 ),
("with", 1156 ),
("for", 1145 ),
("not", 998 ),
("be", 959 ),
("on", 932 ),
("is", 875 ),
("t", 871 ),
("but", 839 ),
("by", 789 ),
("this", 776 ),
("have", 759 ),
("all", 710 ),
("which", 668 ),
("from", 660 ),
("so", 652 ),
("were", 617 ),
("no", 595 ),
("him", 583 ),
("me", 582 ),
("bathsheba", 556 ),
("they", 553 ),
("there", 548 ),
("what", 533 ),
("now", 511 ),
("if", 510 ),
("been", 500 ),
("one", 480 ),
("an", 476 ),
("my", 462 ),
("or", 444 ),
("when", 442 ),
("upon", 431 ),
("do", 428 ),
("up", 425 ),
("man", 417 ),
("would", 407 ),
("then", 403 ),
("time", 401 ),
("alice", 398 ),
("oak", 395 ),
("like", 391 ),
("into", 382 ),
("out", 382 ),
("very", 379 ),
("gabriel", 368 ),
("about", 363 ),
("well", 362 ),
("more", 357 ),
("down", 340 ),
("boldwood", 335 )];

}
