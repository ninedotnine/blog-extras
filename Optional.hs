module Optional (
    Optional(..)
) where

fromMaybe :: a -> Maybe a -> a
fromMaybe d mx = case mx of
    Just x -> x
    Nothing -> d

fromEitherE :: a -> Either e a -> a
fromEitherE d rx = case rx of
    Right x -> x
    Left _ -> d

fromList :: a -> [a] -> a
fromList dfault l = case l of
    x:rest -> x
    [] -> dfault

fold :: (a -> a -> a) -> a -> [a] -> a
fold f start list = recurse list
    where
        recurse [] = start
        recurse (first:rest) = f first (recurse rest)


class Foldable f => Optional f where
    (//) :: f a -> a -> a
    (//) = flip (foldr const)

instance Optional Maybe
instance Optional []
instance Optional (Either e)
