helm-turtle
===========

Turtle module based on the helm game engine.

## Example

```haskell
import FRP.Helm
import FRP.Helm.Window as Window
import FRP.Helm.Turtle


myprogram :: FieldAction
myprogram = do
    setSpeed 20
    penUp
    goto ((-200), 0)
    penDown
    koch 500

koch :: Int -> FieldAction
koch n
    | n <= 10 = fd 5
    | otherwise = do
        koch (n `div` 3)
        lt 60
        koch (n `div` 3)
        rt 120
        koch (n `div` 3)
        lt 60
        koch (n `div` 3)


main :: IO ()
main = run defaultConfig $ renderTurtle <~ Window.dimensions
                         ~~ runTurtle newTurtle myprogram

```
