{-# LANGUAGE ViewPatterns, DeriveFunctor#-}
module FRP.Helm.Turtle
    ( Move(..)
    , fd
    , forward
    , rt
    , right
    , lt
    , left
    , goto
    , penUp
    , penDown
    , setSpeed
    , done
    ) where

import FRP.Helm
import FRP.Helm.Time
import Control.Monad.Free
import Prelude hiding (lines)

-- Maybe change this to Double.
type NumType = Int

data Move next = Fd NumType next
               | Rt NumType next
               | Goto NumType NumType next
               | PenUp next
               | PenDown next
               | PenSize NumType next
               | PenColor Color next
               | SetSpeed NumType next
               | SetHeading NumType next
               | Clear next
               | Done deriving (Eq, Show, Functor)

data Turtle = Turtle { getX, getY, angle :: Double
                     , speed :: NumType
                     , isPenDown :: Bool
                     } deriving (Show)

-- Maybe make something interesting with the "()". Maybe
-- use the "a <- fd 100" syntax to clone a turtle.
type FieldAction = Free Move ()

data Field = Field {turtle :: Turtle, lines :: Path, program :: FieldAction}
    deriving (Show)

{-| Moves the turtle forward. -}
fd :: NumType -> FieldAction
fd a = liftF (Fd a ())

forward :: NumType -> FieldAction
forward = fd

{-| Turns the turtle right in degrees. -}
rt :: NumType -> FieldAction
rt a = liftF (Rt (-a) ())

right :: NumType -> FieldAction
right = rt

{-| Turns the turtle left in degrees. -}
lt :: NumType -> FieldAction
lt a = rt (-a)

left :: NumType -> FieldAction
left = lt

{-| Moves the turtle to position x y. -}
goto :: (NumType, NumType) -> FieldAction
goto (x, y) = liftF (Goto x y ())

{-| Do nothing more. -}
done :: FieldAction
done = liftF Done

{-| Stop the turtle from drawing. -}
penUp :: FieldAction
penUp = liftF (PenUp ())

{-| Start drawing with the turtle, The drawing does
 - not happen until the turtle moves.
 - The pen is down by default.
 -}
penDown :: FieldAction
penDown = liftF (PenDown ())

penSize :: NumType -> FieldAction
penSize a = liftF (PenSize a ())

penColor :: Color -> FieldAction
penColor a = liftF (PenColor a ())

setSpeed :: NumType -> FieldAction
setSpeed a = liftF (SetSpeed a ())

{-| Set the heading of the turtle in degrees. 0 is to the right. 90 is up and so fourth. -}
setHeading :: NumType -> FieldAction
setHeading a = liftF (SetHeading a ())

{-| Clear all the drawings -}
clear :: FieldAction
clear = liftF (Clear ())

goForward :: Double -> Turtle -> Turtle
goForward amount (t@Turtle{getX = x, getY = y}) =
         t{getX = x + amount * cos (degrees (angle t))
          ,getY = y + amount * sin (degrees (angle t))
          }

turnRight :: Turtle -> Double -> Turtle
turnRight t a = t {angle = angle t - a}

goto' :: Turtle -> Double -> Double -> Turtle
goto' t x y = t {getX = x, getY = y}

update :: Time -> Field -> Field
update _ (Field t' points (Free mv)) =
    let ptss = if null points && isPenDown t' then [(getX t', getX t')] else points
    in case (t', points, mv) of
        (t, points, Fd ((<= 0) -> True) next) -> Field t points next
        (t, _,      Fd n next) ->
            let stepAmt = min n (speed t)
                movedTurtle = (goForward (fromIntegral stepAmt) t)
            in  Field movedTurtle
                      ((getX movedTurtle, getY movedTurtle):ptss)
                      (Free (Fd (n - stepAmt) next))
        (t, points, Rt 0 next) -> Field t points next
        (t, points, Rt n next) ->
            let turnAmt = signum n * min (abs n) (speed t) in
            Field (turnRight t (fromIntegral turnAmt)) points (Free (Rt (n - turnAmt) next))
        (t, points, (Goto (fromIntegral -> x') (fromIntegral -> y') next)) ->
            Field (goto' t x' y') ((x', y'):points) next
        (t, points, PenUp   next) -> Field (t {isPenDown = False}) points next
        (t, points, PenDown next) -> Field (t {isPenDown = True }) points next
        (t, points, SetSpeed s next) -> Field (t {speed = s}) points next
        (t, p, (Done)) -> Field t p (Free Done)
update _ self@(Field _ _ (Pure _)) = self

runTurtle :: Turtle -> Free Move () -> Signal Field
runTurtle t prog =
    foldp update (Field t [] prog) (fps 60)

-- Helm crashes if you paint an empty path.
renderTurtle :: (Int, Int) -> Field -> Element
renderTurtle (w, h) (Field t path' _) = centeredCollage w h $ if null path'
    then [drawTurtle ]
    else [drawTurtle, traced (solid white) (path path')]
        where drawTurtle = rotate (degrees (angle t)) $ move (getX t, getY t) $
                filled white $ polygon $ path [(0, 0), (-20, -5), (-20, 5)]

newTurtle :: Turtle
newTurtle = Turtle 0 0 0 10 True

