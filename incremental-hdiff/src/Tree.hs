module Tree where
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Generics.Simplistic.Digest

data BinTree a = Leaf a
               | Node (BinTree a) a (BinTree a)
               deriving (Show, Eq)

data MerkleTree a = LeafM (a, Digest)
                  | NodeM (MerkleTree a) (a, Digest) (MerkleTree a)
                  deriving (Eq)

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
sumMerkleTree (LeafM (x, h)) = (x, M.insert (showHash h) x M.empty)
sumMerkleTree (NodeM l (x, h) r)  = (x', mx)
  where
    mx = M.insert (showHash h) x' (ml <> mr)
    x' = x + sum [lx, rx]
    (lx, ml) = sumMerkleTree l
    (rx, mr) = sumMerkleTree r

sumMerkleTreeWithMap :: M.Map String Int -> MerkleTree Int -> (Int, M.Map String Int)
sumMerkleTreeWithMap _ (LeafM (x, h)) = (x, M.insert (showHash h) x M.empty)
sumMerkleTreeWithMap m (NodeM l (x, h) r) = (x'', M.insert (showHash h) x'' (ml <> mr))
  where
    x'' = fromMaybe x' (M.lookup (showHash h) m)
    x' = x + sum [lx, rx]
    (lx, ml) = sumMerkleTreeWithMap m l
    (rx, mr) = sumMerkleTreeWithMap m r
