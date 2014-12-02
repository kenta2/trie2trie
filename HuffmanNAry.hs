{-# LANGUAGE ScopedTypeVariables #-}
-- Based on Data.Compression.Huffman

module HuffmanNAry where {
import qualified Data.PriorityQueue.FingerTree as PQ;
import Control.Monad;

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
huffman :: forall a w . (Ord w, Num w) => Int -> [(a,w)] -> HuffmanTree a;
huffman n = build . prepare where {
  build :: PQ.PQueue w (HuffmanTree a) -> HuffmanTree a;
  build pq = case get_singleton pq of {
    Just x -> snd x;
    _ -> case get_n pq n of {
    Nothing -> error "Wrong number of elements";
    Just (l, pq2) -> build $ PQ.insert (sum $ map fst l) (Node $ map snd l) pq2;
}}};


}
