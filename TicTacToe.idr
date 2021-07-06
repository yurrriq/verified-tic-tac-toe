module TicTacToe

import Data.Vect

%default total

data Player
  = X
  | O

implementation Eq Player where
  X == X = True
  O == O = True
  _ == _ = False

implementation Ord Player where
  compare X O = LT
  compare O X = GT
  compare _ _ = EQ

implementation MinBound Player where
  minBound = X

implementation MaxBound Player where
  maxBound = O

implementation Enum Player where
  pred X = O
  pred O = X
  succ = pred
  toNat X = 0
  toNat O = 1
  fromNat n with (modNatNZ n (S (S Z)) SIsNotZ)
    | Z = X
    | (S _) = O

implementation Show Player where
  show X = "X"
  show O = "O"

implementation [tttplayer] Show (Maybe Player) where
  show (Just player) = show player
  show Nothing = " "

split : (n, m : Nat) -> (xs : Vect (m * n) a) -> Vect m (Vect n a)
split _ Z [] = []
split n (S k) xs = take n xs :: split n k (drop n xs)

data SplitView : {n : Nat} -> (m : Nat) -> Vect (m * n) a -> Type where
  Split : (xss : Vect m (Vect n a)) -> SplitView m (concat xss)

data TakeView : (m, n : Nat) -> Vect (m + n) a -> Type where
  Take : (xs : Vect m a) -> (ys : Vect n a) -> TakeView m n (xs ++ ys)

takeView : (m : Nat) -> (xs : Vect (m + n) a) -> TakeView m n xs
takeView Z xs = Take [] xs
takeView (S k) (x :: xs) with (takeView k xs)
  takeView (S k) (x :: (ys ++ zs)) | Take ys zs = Take (x :: ys) zs

takeLemma : (ys : Vect n a) -> (zs : Vect m a) -> take n (ys ++ zs) = ys
takeLemma [] zs        = Refl
takeLemma (y :: ys) zs = cong (takeLemma ys zs)

dropLemma : (ys : Vect n a) -> (zs : Vect m a) -> drop n (ys ++ zs) = zs
dropLemma []        zs = Refl
dropLemma (y :: ys) zs = dropLemma ys zs

splitConcatLemma : (xs : Vect (m * n) a) -> concat (split n m xs) = xs
splitConcatLemma {m = Z} [] = Refl
splitConcatLemma {m = S k} {n} xs with (takeView n xs)
  splitConcatLemma {m = S k} {n} (ys ++ zs) | (Take ys zs) =
    let inductiveHypothesis = splitConcatLemma zs {m = k} {n = n} in
        rewrite takeLemma ys zs in
        rewrite dropLemma ys zs in
        rewrite inductiveHypothesis in
                Refl

splitView : (n, m : Nat) -> (xs : Vect (m * n) a) -> SplitView m xs
splitView n m xs =
  rewrite sym (splitConcatLemma xs) in
          Split (split n m xs)

Board : (n : Nat) -> {auto prf : LT Z n} -> Type
Board (S k) = Vect (S k * S k) (Maybe Player)

implementation [tttboard] Show (Board (S k)) where
  show {k} =
    unlines . toList
      . map (concatMap (show @{tttplayer}))
      . split (S k) (S k)

emptyBoard : Board (S k)
emptyBoard {k} = replicate (S k * S k) Nothing

namespace Board
  index : (x, y : Fin (S k)) -> Vect ((S k) * (S k)) a -> a
  index x y board {k} with (splitView (S k) (S k) board)
    index x y (concat rows) {k} | (Split rows) =
      Vect.index x (Vect.index y rows)
  -- TODO: Verify Board.index

  isEmpty : (x, y : Fin (S k)) -> Board (S k) -> Bool
  isEmpty x y = isNothing . Board.index x y

wins : Player -> Vect (S k + (S k + 2)) (Board (S k))
wins winner {k} = winRows ++ winCols ++ winDiags
  where
    winRows : Vect (S k) (Board (S k))
    winRows =
      flip map (range {len = S k}) $ \row =>
        concat . replaceAt row (replicate (S k) (Just winner)) $
          split (S k) (S k) emptyBoard

    winCols : Vect (S k) (Board (S k))
    winCols = map (concat . transpose . split (S k) (S k)) winRows

    winDiags : Vect 2 (Board (S k))
    winDiags =
      [id, concat . map reverse . split (S k) (S k)]
        <*> pure
          ( foldl (flip (\n => replaceAt n (Just winner))) emptyBoard $
              fromNat <$> [0, (S (S k)) .. S k * S k]
          )

allWins : Vect (2 * (S k + (S k + 2))) (Board (S k))
allWins {k} = concat $ wins <$> [X,O]

namespace Main
  main : IO ()
  main = let k = 2 in traverse_ (go {k}) (allWins {k})
    where
     go : Board (S k) -> IO ()
     go board {k} =
       do
         putStr "+" *> putStr (pack (List.replicate (S k) '-')) *> putStrLn "+"
         putStr (unlines (map (\s => "|" ++ s ++ "|") (lines (show @{tttboard} board))))
         putStr "+" *> putStr (pack (List.replicate (S k) '-')) *> putStrLn "+\n"
