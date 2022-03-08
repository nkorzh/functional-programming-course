module HW1.T6 where

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap toMono
  where
    toMono (Just el) = el
    toMono _ = mempty

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap toPair
  where
    toPair (Left el) = (el, mempty)
    toPair (Right el) = (mempty, el)
