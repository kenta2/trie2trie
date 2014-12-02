{-# LANGUAGE ScopedTypeVariables #-}
-- Based on Data.Compression.Huffman

module HuffmanNAry where {
import qualified Data.PriorityQueue.FingerTree as PQ;
import Control.Monad;
import qualified Data.Map as Map;
import Data.Map(Map);
import Data.Either;

newtype HuffmanArity = HuffmanArity Int deriving (Show);

get_n :: forall k v . (Ord k) => PQ.PQueue k v -> Int -> Maybe ([(k,v)], PQ.PQueue k v);
get_n q 0 = Just ([],q);
get_n q n = do {
  (x :: (k,v), q2 :: PQ.PQueue k v) <- PQ.minViewWithKey q;
  -- whole thing fails atomically if not enough elements
  (y :: [(k,v)], q3 :: PQ.PQueue k v) <- get_n q2 (pred n);
  return (x:y, q3);
};

get_singleton :: (Ord k) => PQ.PQueue k v -> Maybe (k,v);
get_singleton q = do {
  (x,q2) <- PQ.minViewWithKey q;
  case PQ.minView q2 of {
    Nothing -> return x;
    _ -> mzero;
  }
};

data HuffmanTree a = Leaf a | Node [HuffmanTree a] deriving (Show);

prepare :: (Ord w) => [(a,w)] -> PQ.PQueue w (HuffmanTree a);
prepare = PQ.fromList . map (\ (x,w) -> (w, Leaf x));

-- needs to be padded to (length == 1 mod (n-1))
huffman1 :: forall a w . (Ord w, Num w) => HuffmanArity -> [(a,w)] -> HuffmanTree a;
huffman1 (HuffmanArity n) = build . prepare where {
  build :: PQ.PQueue w (HuffmanTree a) -> HuffmanTree a;
  build pq = case get_singleton pq of {
    Just x -> snd x;
    _ -> case get_n pq n of {
    Nothing -> error "Wrong number of elements";
    Just (l, pq2) -> build $ PQ.insert (sum $ map fst l) (Node $ map snd l) pq2;
}}};

get_depths :: forall a . (Ord a) => HuffmanTree a -> Map a Int;
get_depths = f 0 where {
f :: Int -> HuffmanTree a -> Map a Int;
f current (Leaf x) = Map.singleton x current;
f current (Node ts) = Map.unions $ map (f (1+current)) ts;
};

num_to_add :: HuffmanArity -> Int -> Int;
num_to_add (HuffmanArity n) l = let {
 m :: Int;
 m = mod l (n-1);
} in mod (n-m) (n-1);

huffman :: forall a w . (Ord w, Num w) => HuffmanArity -> [(a,w)] -> HuffmanTree (Either Int a);
huffman n l = let {
padsize :: Int;
padsize = num_to_add n (length l);
pad :: [(Either Int a,w)];
pad = zip (map Left $ enumFromTo 1 padsize) $ repeat 0;
mkRight :: (a,w) -> (Either Int a,w);
mkRight (x,y) = (Right x, y);
} in huffman1 n $ pad ++ map mkRight l;


noLefts :: (Ord b) => Map (Either a b) v -> Map b v;
noLefts = Map.fromList . map (\(Right x,y)->(x,y)) . filter (isRight . fst) . Map.assocs;
-- fromAscList is possible

huffman_depths :: (Ord a, Ord w, Num w) => HuffmanArity -> [(a,w)] -> Map a Int;
huffman_depths n l = noLefts $ get_depths $ huffman n l;
}
