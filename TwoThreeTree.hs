{--
Two three tree in haskell

Bugs:
-delete sometimes leaves a Branch with two Emptys

Notes:
-delete silently returns the same tree if requested key isn't
in the tree
-if new key equals current key in tree, send it left with the small guys

--}

import Data.Maybe

data Tree k v = SmallBranch (k,v) (Tree k v) (Tree k v) | BigBranch (k,v) (k,v) (Tree k v) (Tree k v) (Tree k v)
    | SmallLeaf (k,v) | BigLeaf (k,v) (k,v) | Empty
        deriving Show



--Given a (key, value) pair, return the Tree with the addition of the pair
insert :: Ord k => (k,v) -> Tree k v -> Tree k v

insert (inKey, inValue) (BigBranch (key1,val1) (key2,val2) left middle right)
    | inKey <= key1 = Branch (key,val) (insert (inKey, inValue) left) right
    | inKey >= key2 = Branch (key,val) (insert (inKey, inValue) left) right
    | otherwise = Branch (key,val) left (insert (inKey, inValue) right)


insert (inKey, inValue) (Leaf (key,val))
    | inKey <= key = Branch (key,val) (Leaf (inKey, inValue)) Empty
    | otherwise = Branch (key,val) Empty (Leaf (inKey, inValue))

insert (inKey,inValue) (SmallLeaf (key,val))
    | inKey >= key
insert (inKey,inValue) Empty = SmallLeaf (key,val)









--Given a key, return the associated value in the tree
find :: Ord k => k -> Tree k v -> Maybe v
find inKey (Branch (key,val) left right)
    | inKey < key = find inKey left
    | inKey == key = Just val
    | inKey > key = find inKey right
find inKey (Leaf (key,val))
    | inKey == key = Just val
    | otherwise = Nothing
find inKey Empty = Nothing


--Given a key, return tree with the associated node removed
delete :: Ord k => k -> Tree k v ->  Tree k v
delete inKey (Branch (key,val) left right)
    | inKey < key = Branch (key,val) (delete inKey left) right
    | inKey == key =  deleteHelper $ Branch (key, val) left right
    | inKey > key = Branch (key,val) left (delete inKey right)
delete inKey (Leaf (key,val))
    | inKey == key =  Empty
    | otherwise = Leaf (key,val)
delete inKey Empty = Empty

--Helper functions for delete:
--maxkey returns the highest key on this tree by going right
maxKey:: Tree k v -> (k,v)
maxKey (Branch (key,val) left Empty) = (key,val)
maxKey (Branch (key,val) left right) = maxKey right
maxKey (Leaf (key,val)) = (key,val)

--fix deletes the appropriate node from the left subtree
removeMax:: Tree k v -> Tree k v
removeMax (Branch (key,val) left Empty) = left
removeMax (Branch (key,val) left right) = Branch (key,val) left (removeMax right)
removeMax (Leaf (key,val)) = Empty

--called by delete on the subtree whose root should be deleted
deleteHelper :: Tree k v -> Tree k v
--if the left branch is empty, avoid errors. this case is trivial
deleteHelper (Branch (key,val) Empty right) = right
--otherwise, try to run classic deletion algorithm on the left branch.
deleteHelper (Branch (key, val) left right) = Branch (maxKey left) (removeMax left) right
