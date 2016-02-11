{-# LANGUAGE TupleSections #-}

import           Control.Monad
convolve xs ys = fst . walk $ xs
  where
    walk [] =  ([],  ys)

    walk (a:as) = let
      (r, b:bs) = walk as
      in
        ((a, b):r, bs)

convolve' :: [a] -> [b] -> [(a, b)]
convolve' xs ys = fst . foldr func ([], ys) $ xs
  where
    func x (r, a:as) = ((x, a):r, as)



-- |What happens if we run convolve through pointfree?

convolve'' :: [a] -> [b] -> [(a, b)]
convolve'' = flip ((fst .) . foldr ((`ap` snd) . (. fst) . flip flip tail . (ap .) . flip flip head . (((.) . ((,) .)) .) . flip . ((:) .) . (,)) . ([],))


-- |Here is isPalindrome, in O(n) recursive calls.
-- Note that it's doing an extra n/2 computations!

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = fst . foldr func (True, xs) $ xs
  where
    -- our acculated Bool is on the left of the && so we can short circuit if false
    func x (r, a:as) = (r && (x == a), as)

-- | This is O(n/2) recursive calls!
-- we have two pointers which walk down our stack
-- one going at single speed, one going at double speed.
-- Once the double speed is done, the single speed will be
-- in the middle. So we return the tail of the single speed,
-- which will just be the latter half of the list.
-- And then we just spring back up!

convolveHalves :: [a] -> [(a, a)]
convolveHalves xs = fst $ walk xs xs
  where
    -- a is for single time, d is for double time

    walk as [] = ([], as) -- even case
    walk (a:as) [_] = ([], as) -- odd case
    walk (a:as) (_:_:ds) =
      let
        (r, b:bs) = walk as ds
      in
        ((a, b):r, bs)


convolveHalves' :: [a] -> [(a, a)]
convolveHalves' xs = fst $ fst $ foldr func (([], xs), xs) xs
  where
    func _ ((r, as), []) = ((r, as), []) -- short circuit (even)
    func _ ((r, as), [d]) = ((r, as), [d]) -- short circuit (odd)
    func x ((r, a:as), _:_:ds) = (((x, a):r, as), ds)


isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = fst $ fst $ foldr func ((True, xs), xs) xs
  where
    func _ ((r, as), []) = ((r, as), []) -- short circuit (even)
    func _ ((r, as), [d]) = ((r, as), [d]) -- short circuit (odd)
    func x ((r, a:as), _:_:ds) = ((r && (x == a), as), ds)


-- |Note: Am I doing two folds? Or is Haskell's laziness basically letting me do just one?
isPalindrome'' :: (Eq a) => [a] -> Bool
isPalindrome'' xs = foldl func True $ convolveHalves' xs
  where
    func acc (x,y) = acc && x == y

palindromize :: [a] -> [a]
palindromize xs = xs ++ reverse xs

p = "a;wejnga;wejnglkawejngalwkejngalkwejnglakwjenggnejwkalgnjewklagnjekwlagnjewaklgnjew;agnjew;a"
