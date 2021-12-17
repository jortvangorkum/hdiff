module Tree where
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe, isJust)
import           Generics.Simplistic.Digest

data BinTree a = Leaf a
               | Node (BinTree a) a (BinTree a)
               deriving (Show, Eq)

data MerkleTree a = LeafM (a, Digest)
                  | NodeM (MerkleTree a) (a, Digest) (MerkleTree a)
                  deriving (Eq)

instance Functor MerkleTree where
  fmap f (LeafM (x, h))     = LeafM (f x, h)
  fmap f (NodeM l (x, h) r) = NodeM (fmap f l) (f x, h) (fmap f r)

showHash :: Digest -> String
showHash d = take 5 (show (getDigest d))

instance (Show a) => Show (MerkleTree a) where
  show (LeafM (x, h)) = "Leaf (" ++ show x ++ ", " ++ showHash h ++ ")"
  show (NodeM l (x, h) r)  = "Node [" ++ show x ++ ", " ++ showHash h  ++ "] (" ++ show l ++ ") (" ++ show r ++ ")"

leafDigest :: Digest
leafDigest = digest "Leaf"

generateTree :: Int -> BinTree Int
generateTree 0 = Leaf 0
generateTree n = Node (generateTree (n - 1)) n (generateTree (n - 1))

heightTree :: BinTree Int -> Int
heightTree (Leaf _)     = 0
heightTree (Node l _ r) = 1 + max (heightTree l) (heightTree r)

getRootDigest :: MerkleTree a -> Digest
getRootDigest (LeafM (_, h))     = h
getRootDigest (NodeM _ (_, h) _) = h

merkelize :: (Show a) => BinTree a -> MerkleTree a
merkelize (Leaf x)   = LeafM (x, h)
  where
    h = digestConcat [digest "Leaf", digest x]
merkelize (Node l x r) = NodeM l' (x, h) r'
  where
    l' = merkelize l
    hl = getRootDigest l'
    r' = merkelize r
    hr = getRootDigest r'
    h  = digestConcat [digest "Node", digest x, hl, hr]

sumMerkleTree :: MerkleTree Int -> (Int, M.Map String Int)
sumMerkleTree (LeafM (x, h))      = (x, M.insert (showHash h) x M.empty)
sumMerkleTree (NodeM l (x, h) r)  = (x', M.insert (showHash h) x' (ml <> mr))
  where
    x' = x + lx + rx
    (lx, ml) = sumMerkleTree l
    (rx, mr) = sumMerkleTree r

sumMerkleTreeWithMap :: M.Map String Int -> MerkleTree Int -> (Int, M.Map String Int)
sumMerkleTreeWithMap _ (LeafM (x, h))     = (x, M.insert (showHash h) x M.empty)
sumMerkleTreeWithMap m (NodeM l (x, h) r) = y
  where
    y = case M.lookup (showHash h) m of
      Nothing -> (x', M.insert (showHash h) x' (ml <> mr))
      Just n  -> (n, m)
    x' = x + lx + rx
    (lx, ml) = sumMerkleTreeWithMap m l
    (rx, mr) = sumMerkleTreeWithMap m r

cataMerkleTree :: ((a, Digest) -> (a, M.Map String a))
               -> ((a, M.Map String a) -> (a, Digest) -> (a, M.Map String a) -> (a, M.Map String a))
               -> MerkleTree a
               -> (a, M.Map String a)
cataMerkleTree leaf _    (LeafM x)     = leaf x
cataMerkleTree leaf node (NodeM l x r) = node l' x r'
  where
    l' = cataMerkleTree leaf node l
    r' = cataMerkleTree leaf node r

sumMerkleTree2 :: MerkleTree Int -> (Int, M.Map String Int)
sumMerkleTree2 = cataMerkleTree leaf node
  where
    leaf (x, h) = (x, M.insert (showHash h) x M.empty)
    node (lx, ml) (x, h) (rx, mr) = let x' = x + lx + rx
                                    in (x', M.insert (showHash h) x (ml <> mr))

cataMerkleTreeMap :: ((a, Digest) -> (a, M.Map String a))
               -> (M.Map String a -> (a, M.Map String a) -> (a, Digest) -> (a, M.Map String a) -> (a, M.Map String a))
               -> M.Map String a
               -> MerkleTree a
               -> (a, M.Map String a)
cataMerkleTreeMap leaf _    _ (LeafM x)     = leaf x
cataMerkleTreeMap leaf node m (NodeM l x r) = node m l' x r'
  where
    l' = cataMerkleTreeMap leaf node m l
    r' = cataMerkleTreeMap leaf node m r

cataTree :: (a -> b) -> (b -> a -> b -> b) -> BinTree a -> b -- and MerkleTree
cataTree leaf node (Leaf x) = leaf x
cataTree leaf node (Node l x r) = node l' x r'
  where
    l' = cataTree leaf node l
    r' = cataTree leaf node r

changeLeaf :: a -> BinTree a -> BinTree a
changeLeaf y (Leaf x)     = Leaf y
changeLeaf y (Node l x r) = Node (changeLeaf y l) x r
