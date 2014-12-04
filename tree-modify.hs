{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Main where {
import qualified Data.Set as Set;
import Data.Set(Set);
import qualified Data.Map as Map;
import Data.Map(Map);
import Debug.Trace;
import Data.List;
import HuffmanNAry;
import Alice;
import Data.Maybe;
import System.Environment;

-- cycle in type synonym declarations
-- type Foo a = Map a (Foo a);

newtype Trie a = Trie {unTrie :: (Map a (Trie a))} deriving (Show);
type Ctrie = Trie Char;


main :: IO();
main = getArgs >>= \case {
["alice",n] -> let {
corpus = take (read n) alice;
} in
print $ show_hdepth corpus $ p1_keep corpus;
["go",fn] -> do {
corpus <- read_corpus fn;
print $ show_hdepth corpus $ p1_keep corpus;
};
["z2"] -> do {
-- this, with HuffmanArity 3, shows that greedy 1 step does not work.
-- two steps, the maximum is th the.
eval_show initial1;
eval_show initial;
let { he =fromJust $ p1_improve a10 initial1;};
eval_show he;
let { he2 =fromJust $ p1_improve a10 he;};
eval_show he2;
eval_show $ fromJust $ p1_improve a10 initial;
print $ length $ double_step initial1;
eval_show $ fromJust $ improve_1 initial1 double_step (eval_trie a10);
};
["z3"] -> do {
-- two single steps from "the" does get the same result as a double, namely nd of
let { the = fromJust $ p1_improve a10 initial };
eval_show the;
let { t2 = fromJust $ p1_improve a10 the };
eval_show t2;
let { t3 = fromJust $ p1_improve a10 t2 };
eval_show t3;
print $ length $ double_step the;
eval_show $ fromJust $ improve_1 the double_step (eval_trie a10);

};
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

initial1 :: Ctrie;
initial1 = flat alphabet;

initial2 :: Ctrie;
initial2 = (cmod initial1) !! 501;  -- th 501

initial :: Ctrie;
initial=initial1;

initial3 :: Ctrie;
initial3 = Trie {unTrie = Map.fromList [('a',Trie {unTrie = Map.fromList [('k',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList []})]}),('n',Trie {unTrie = Map.fromList []}),('q',Trie {unTrie = Map.fromList []})]}),('b',Trie {unTrie = Map.fromList []}),('c',Trie {unTrie = Map.fromList [('k',Trie {unTrie = Map.fromList []})]}),('d',Trie {unTrie = Map.fromList []}),('e',Trie {unTrie = Map.fromList [('x',Trie {unTrie = Map.fromList [('p',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList [('c',Trie {unTrie = Map.fromList [('t',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList [('d',Trie {unTrie = Map.fromList []})]})]})]}),('n',Trie {unTrie = Map.fromList [('s',Trie {unTrie = Map.fromList [('i',Trie {unTrie = Map.fromList [('v',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList []})]})]})]})]}),('r',Trie {unTrie = Map.fromList [('i',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList [('n',Trie {unTrie = Map.fromList [('c',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList []})]})]})]})]}),('t',Trie {unTrie = Map.fromList []})]})]}),('l',Trie {unTrie = Map.fromList [('a',Trie {unTrie = Map.fromList []})]})]})]})]}),('f',Trie {unTrie = Map.fromList []}),('g',Trie {unTrie = Map.fromList []}),('h',Trie {unTrie = Map.fromList []}),('i',Trie {unTrie = Map.fromList [('n',Trie {unTrie = Map.fromList [('k',Trie {unTrie = Map.fromList []})]})]}),('j',Trie {unTrie = Map.fromList [('o',Trie {unTrie = Map.fromList []}),('u',Trie {unTrie = Map.fromList [('d',Trie {unTrie = Map.fromList [('g',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList []})]}),('i',Trie {unTrie = Map.fromList [('c',Trie {unTrie = Map.fromList [('i',Trie {unTrie = Map.fromList [('a',Trie {unTrie = Map.fromList []})]})]})]})]}),('r',Trie {unTrie = Map.fromList [('y',Trie {unTrie = Map.fromList []})]}),('s',Trie {unTrie = Map.fromList [('t',Trie {unTrie = Map.fromList []})]})]})]}),('k',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList []}),('n',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList [('w',Trie {unTrie = Map.fromList []})]}),('o',Trie {unTrie = Map.fromList [('w',Trie {unTrie = Map.fromList []})]})]})]}),('l',Trie {unTrie = Map.fromList []}),('m',Trie {unTrie = Map.fromList []}),('n',Trie {unTrie = Map.fromList []}),('o',Trie {unTrie = Map.fromList [('k',Trie {unTrie = Map.fromList []})]}),('p',Trie {unTrie = Map.fromList []}),('q',Trie {unTrie = Map.fromList [('u',Trie {unTrie = Map.fromList [('a',Trie {unTrie = Map.fromList [('l',Trie {unTrie = Map.fromList [('i',Trie {unTrie = Map.fromList []})]})]}),('e',Trie {unTrie = Map.fromList [('n',Trie {unTrie = Map.fromList []}),('s',Trie {unTrie = Map.fromList [('t',Trie {unTrie = Map.fromList [('i',Trie {unTrie = Map.fromList [('o',Trie {unTrie = Map.fromList [('n',Trie {unTrie = Map.fromList [('s',Trie {unTrie = Map.fromList []})]})]})]})]})]})]}),('i',Trie {unTrie = Map.fromList []}),('o',Trie {unTrie = Map.fromList [('t',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList []})]})]})]})]}),('r',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList []}),('k',Trie {unTrie = Map.fromList []})]}),('s',Trie {unTrie = Map.fromList []}),('t',Trie {unTrie = Map.fromList [('h',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList []})]}),('v',Trie {unTrie = Map.fromList []})]}),('u',Trie {unTrie = Map.fromList []}),('v',Trie {unTrie = Map.fromList [('a',Trie {unTrie = Map.fromList []}),('e',Trie {unTrie = Map.fromList [('n',Trie {unTrie = Map.fromList []}),('r',Trie {unTrie = Map.fromList [('y',Trie {unTrie = Map.fromList []})]})]}),('i',Trie {unTrie = Map.fromList [('n',Trie {unTrie = Map.fromList [('c',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList [('d',Trie {unTrie = Map.fromList []})]})]}),('g',Trie {unTrie = Map.fromList []})]})]}),('o',Trie {unTrie = Map.fromList [('i',Trie {unTrie = Map.fromList [('c',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList []})]}),('d',Trie {unTrie = Map.fromList []})]}),('l',Trie {unTrie = Map.fromList [('u',Trie {unTrie = Map.fromList []}),('v',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList [('d',Trie {unTrie = Map.fromList []})]})]})]}),('r',Trie {unTrie = Map.fromList []}),('t',Trie {unTrie = Map.fromList [('e',Trie {unTrie = Map.fromList []})]})]}),('s',Trie {unTrie = Map.fromList []})]}),('w',Trie {unTrie = Map.fromList []}),('x',Trie {unTrie = Map.fromList []}),('y',Trie {unTrie = Map.fromList []}),('z',Trie {unTrie = Map.fromList []})]};

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

keep_improving :: (Show score, Ord score, Show a) => a -> (a -> [a]) -> (a -> score) -> a;
keep_improving start nexts eval = case improve_1 start nexts eval of {
Just new -> trace ("improved " ++ show new) $ keep_improving new nexts eval;
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

arity :: HuffmanArity;
arity = HuffmanArity 2;

hcounts :: (Ord a, Ord w, Num w) => [(a,w)] -> Map a Int;
hcounts = huffman_depths . huffman arity;

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

p1_keep :: Wordcounts -> Ctrie;
p1_keep l = keep_improving initial cmod (eval_trie l);

p1_improve :: Wordcounts -> Ctrie -> Maybe Ctrie;
p1_improve l t = improve_1 t cmod (eval_trie l);

cmod :: Ctrie -> [Ctrie];
cmod = modifications set_alphabet;

double_step :: Ctrie -> [Ctrie];
double_step = concatMap cmod . cmod;
cscores :: Wordcounts -> Ctrie -> [(Integer,Ctrie)];
cscores l t = sortBy (\a b -> compare (fst a)(fst b)) $
do {
u <- cmod t;
return (eval_trie l u,u)};

a10 :: Wordcounts;
a10 = take 10 alice;

eval_show :: Ctrie -> IO();
eval_show t = print $ (eval_trie a10 t, all_strings t);

read_corpus :: String -> IO Wordcounts;
read_corpus fn = readFile fn >>= return . map parse . lines;

parse :: String -> (String,Integer);
parse s = let { w = words s} in (w!!1, read $ head w);

show_hdepth :: Wordcounts -> Ctrie -> (HuffmanTreeE String,[(String,Int)]);
show_hdepth l t = let {
h :: HuffmanTreeE String;
h = huffman arity $ Map.assocs $ phase1 t l;
} in
(h,Map.assocs $ hcounts $ Map.assocs $ phase1 t l);

}
