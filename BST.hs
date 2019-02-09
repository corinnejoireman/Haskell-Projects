{-
File: BST.hs

Rinn Joireman

Represents a binary search tree.
-}

data BST k v = EmptyNode | InteriorNode k v (BST k v) (BST k v)
  deriving (Eq, Show)

-- New instance of Show goes here

newBST :: (BST k v)
newBST = EmptyNode

bstLookup :: Ord k => k -> (BST k v) -> v
bstLookup key EmptyNode = error "Key not found"
bstLookup key (InteriorNode k v left right)
  | key < k = bstLookup key left
  | key > k = bstLookup key right
  | key == k = v

bstAdd :: Ord k => k -> v -> (BST k v) -> (BST k v)
bstAdd key value EmptyNode = InteriorNode key value EmptyNode EmptyNode
bstAdd key value (InteriorNode k v left right)
  | key < k = InteriorNode k v (bstAdd key value left) right
  | key > k = InteriorNode k v left (bstAdd key value right)
  | key == k = error "Duplicate key"

-- Returns a bst built from the pairs in the association list
listToBST :: Ord k => [(k,v)] -> (BST k v)
listToBST aList = buildTree newBST aList where
     buildTree :: Ord k => (BST k v) -> [(k,v)] -> (BST k v)
     buildTree bst [] = bst
     buildTree bst ((key,val):xs) = buildTree (bstAdd key val bst) xs

-- Returns an association list of the pairs in the bst,
-- in inorder
bstInorder :: Ord k => (BST k v) -> [(k,v)]
bstInorder EmptyNode = []
bstInorder (InteriorNode k v left right) = bstInorder (left) ++ [(k,v)] ++ bstInorder(right)


-- Returns an association list of the pairs in the bst,
-- in preorder
bstPreorder :: Ord k => (BST k v) -> [(k,v)]
bstPreorder EmptyNode = []
bstPreorder (InteriorNode k v left right) =[(k,v)] ++ bstPreorder(left) ++ bstPreorder(right)

-- Returns an association list of the pairs in the bst,
-- in postorder
bstPostorder :: Ord k => (BST k v) -> [(k,v)]
bstPostorder EmptyNode = []
bstPostorder (InteriorNode k v left right) = bstPostorder(left) ++ bstPostorder(right) ++ [(k,v)]

-- Returns a string that shows the bst's shape, using just
-- the keys
prettyString :: (Ord k, Show k) => (BST k v) -> String
prettyString bst = helper bst 0 where
  helper EmptyNode _ = ""
  helper (InteriorNode k v left right) level =
    (helper right (level + 1)) ++ (repeatString "| "  level) ++
    (show k) ++ "\n" ++ (helper left (level+ 1))

-- Returns a string repeated count - 1 times
repeatString :: String -> Int -> String
repeatString str 0 = ""
repeatString str count =
    str ++ (repeatString str (count - 1))

-- Builds a balanced BST for testing purposes
sampleBST :: (BST String Int)
sampleBST = listToBST (zip ["D", "B", "A", "C", "F", "E", "G"] [1..7])
