-- simple type-safe string formatting in (almost) standard Haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypedPrintf where

import Data.Functor

data FmtSpecifier = FmtStr String
                  | FmtChar Char
                  | FmtInt Int
                  | FmtFloat Double
                  | FmtPaddedFmt Int Char FmtSpecifier
                  | FmtQualFloat FmtFloatQualifiers Double


data FmtFloatQualifiers = FmtFloatQualifiers {
      show_sign :: Bool
    , show_decimal_point :: Bool
    , scientific_notation :: Bool
    , precision :: Int
    }

convert :: FmtSpecifier -> String
convert = \case
    FmtStr s -> s
    FmtChar c -> [c]
    FmtInt i -> show i
    FmtFloat n -> show n
    FmtPaddedFmt min_len char fmt ->
        if min_len > len
            then replicate (min_len - len) char <> str
            else str
          where
            str = convert fmt
            len = length str
    FmtQualFloat quals n -> fmt_float quals n
        where
            fmt_float :: FmtFloatQualifiers -> Double -> String
            fmt_float = undefined


sprintf :: [FmtSpecifier] -> String
sprintf = (>>= convert)

printf :: [FmtSpecifier] -> IO ()
printf = sprintf <&> putStr

report :: String -> Int -> IO ()
report name number =
    printf [ FmtStr name
           , FmtStr " is player: "
           , FmtInt number
           , FmtChar '\n' ]

greet :: IO String
greet = do
    putStrLn "what is your name?"
    name <- getLine
    printf [ FmtStr "hello "
           , FmtStr name
           , FmtStr ", nice to meet you!\n" ]
    pure name

play_game :: Int -> String -> IO ()
play_game correct name = do
    putStrLn "guess the number:"
    get_guess >>= \case
        Left str -> do
            printf [ FmtChar '`'
                   , FmtStr str
                   , FmtStr "` is not a valid number.\n" ]
            recurse
        Right n -> if n == correct
            then printf [ FmtStr "good guess, "
                        , FmtStr name
                        , FmtStr "! you win!\n" ]
            else do
                printf [ FmtStr "sorry, but "
                       , FmtInt n
                       , FmtStr " is not the answer. try again!\n" ]
                recurse
  where
    recurse = play_game correct name
    get_guess :: IO (Either String Int)
    get_guess = do
        input <- getLine
        pure $ case reads input of
            [(n, _)] -> Right n
            _ -> Left input


main :: IO ()
main = greet >>= play_game 6
