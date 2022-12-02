import Control.Applicative
import Control.Arrow ((***))
import Control.Monad
import System.IO

data Shape = Rock | Paper | Scissors deriving Enum
shapeValue :: Shape -> Int
shapeValue = (+1) . fromEnum
data Outcome = Lose | Draw | Win deriving Enum
outcomeValue :: Outcome -> Int
outcomeValue = (*3) . fromEnum

fromABC :: Enum a => String -> a
fromABC = toEnum . subtract (fromEnum 'A') . fromEnum . head
fromXYZ :: Enum a => String -> a
fromXYZ = toEnum . subtract (fromEnum 'X') . fromEnum . head

toTuple :: [a] -> (a, a)
toTuple [u, v] = (u, v)

outcomeFromShapes :: (Shape, Shape) -> Outcome
outcomeFromShapes = toEnum . (`mod` 3) . (+1) . uncurry subtract . (fromEnum *** fromEnum)

shapeFromOutcome :: (Shape, Outcome) -> Shape
shapeFromOutcome = toEnum . (`mod` 3) . (+2) . uncurry (+) . (fromEnum *** fromEnum)

strategy1 = sum . map (((+) <$> shapeValue . snd <*> outcomeValue . outcomeFromShapes) . (fromABC *** fromXYZ))
strategy2 = sum . map (((+) <$> shapeValue . shapeFromOutcome <*> outcomeValue . snd) . (fromABC *** fromXYZ))

main = print . ((,) <$> strategy1 <*> strategy2) . map (toTuple . words) . lines =<< readFile "input"
