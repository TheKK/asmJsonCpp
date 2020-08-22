module AsmJsonCpp.Internal.List
  ( appendOnLast,
  )
where

import RIO

-- | Append item on last element. It's useful to construct source code.
--
-- When input list is empty, the given item would be inserted.
--
-- @
-- >>> appendOnLast ";" ["{", "ok", "}"]
-- ["{", "ok", "};"]
-- >>> appendOnLast ";" []
-- [";"]
-- @
appendOnLast :: Monoid a => a -> [a] -> [a]
appendOnLast a [] = [a]
appendOnLast a (x : []) = [x <> a]
appendOnLast a (x : xs) = x : appendOnLast a xs
