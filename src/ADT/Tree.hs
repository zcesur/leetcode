module ADT.Tree where

data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show, Eq)

empty :: Tree a -> Bool
empty Nil = True
empty _   = False

val :: Tree a -> a
val Nil          = undefined
val (Node _ x _) = x

left :: Tree a -> Tree a
left Nil          = undefined
left (Node l _ _) = l

right :: Tree a -> Tree a
right Nil          = undefined
right (Node _ _ r) = r

children :: Tree a -> [Tree a]
children Nil          = []
children (Node l _ r) = filter (not . empty) [l, r]

leaf :: a -> Tree a
leaf x = Node Nil x Nil

height :: Tree a -> Int
height Nil          = -1
height (Node l _ r) = 1 + max (height l) (height r)

balanced :: Tree a -> Bool
balanced Nil = True
balanced (Node l _ r) =
  abs (height l - height r) <= 1 && balanced l && balanced r

preorder :: Tree a -> [a]
preorder Nil          = []
preorder (Node l x r) = [x] ++ preorder l ++ preorder r

inorder :: Tree a -> [a]
inorder Nil          = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r

postorder :: Tree a -> [a]
postorder Nil          = []
postorder (Node l x r) = postorder l ++ postorder r ++ [x]

levelorder :: Tree a -> [a]
levelorder Nil = []
levelorder t   = go [t]
 where
  go [] = []
  go ts = map val ts ++ go (concatMap children ts)

printT :: Show a => Tree a -> IO ()
printT (Node l x r) = go "" (Node l x r)
 where
  go _       Nil          = return ()
  go padding (Node l x r) = do
    putStrLn $ padding ++ show x
    go ("  " ++ padding) l
    go ("  " ++ padding) r
