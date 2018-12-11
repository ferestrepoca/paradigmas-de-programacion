import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

main :: IO ()
main = do
    st <- newIORef (Value "" Nothing)
    void <- initGUI -- Gtk function
    window <- windowNew -- Gtk action. Returns an opaque Window in which we will put the content
    set window [  windowTitle := "Calculator"
                , windowDefaultWidth := 250
                , windowDefaultHeight := 250 -- Set is an action. It lets us change the attributes
                , windowResizable := False]
    display <- entryNew -- This is a widget. We will use it to add numbers
    set display [ entryEditable := False
                , entryXalign := 1 -- right aligns content
                , entryText := "0"]
    grid <- gridNew                  -- creates a new grid
    gridSetRowHomogeneous grid True  -- makes every row have equal height
    let attach x y w h item = gridAttach grid item x y w h -- define attach function
        mkBtn = mkButton st display
    attach 0 0 5 1 display           -- attach the display previously created to the grid
    mkBtn "MC" id >>= attach 0 1 1 1   -- use a combination of the mkBtn helper to create a button and place them on the grid
    mkBtn "MR" id >>= attach 1 1 1 1
    mkBtn "MS" id >>= attach 2 1 1 1
    mkBtn "M+" id >>= attach 3 1 1 1
    mkBtn "M–" id >>= attach 4 1 1 1
    mkBtn "←"  backspace >>= attach 0 2 1 1
    mkBtn "CE" clearEntry >>= attach 1 2 1 1
    mkBtn "C"  clearAll >>= attach 2 2 1 1
    mkBtn "±"  id >>= attach 3 2 1 1
    mkBtn "√"  id >>= attach 4 2 1 1
    mkBtn "7"  (enterDigit '7') >>= attach 0 3 1 1
    mkBtn "8"  (enterDigit '8') >>= attach 1 3 1 1
    mkBtn "9"  (enterDigit '9') >>= attach 2 3 1 1
    mkBtn "÷"  (operator Division) >>= attach 3 3 1 1
    mkBtn "%"  (operator Module) >>= attach 4 3 1 1
    mkBtn "4"  (enterDigit '4') >>= attach 0 4 1 1
    mkBtn "5"  (enterDigit '5') >>= attach 1 4 1 1
    mkBtn "6"  (enterDigit '6') >>= attach 2 4 1 1
    mkBtn "*"  (operator Multiplication) >>= attach 3 4 1 1
    mkBtn "1/x" id >>= attach 4 4 1 1
    mkBtn "1"  (enterDigit '1') >>= attach 0 5 1 1
    mkBtn "2"  (enterDigit '2') >>= attach 1 5 1 1
    mkBtn "3"  (enterDigit '3') >>= attach 2 5 1 1
    mkBtn "–"  (operator Substraction) >>= attach 3 5 1 1
    mkBtn "="  equals >>= attach 4 5 1 2
    mkBtn "0"  (enterDigit '0') >>= attach 0 6 2 1
    mkBtn "."  enterDot >>= attach 2 6 1 1
    mkBtn "+"  (operator Addition) >>= attach 3 6 1 1
    containerAdd window grid -- add the grid to the window
    window `on` deleteEvent $ do
        liftIO mainQuit
        return False
    widgetShowAll window
    mainGUI

----------------------------------------------------
--DATA TYPES

-- | Value is our state, it contains textual representation of first argument 
-- | and optionally representation of action that should be performed on it
-- | 'Value' holds textual representation of first argument reversed and
-- 'Action' to apply to it, which see.
data Value = Value String (Maybe Action)
    deriving Show

-- | Action to apply to first argument and textual representation of second
-- argument reversed (if relevant).
data Action
    = Addition          String
    | Substraction      String
    | Multiplication    String
    | Division          String
    | Module            String
    deriving Show

----------------------------------------------------
-- HELPER FUNCTIONS

-- Expects a function that is going to receive a string and return a string,
-- along with an action, and return an action
mapAction :: (String -> String) -> Action -> Action
mapAction f (Addition       x) = Addition       (f x)
mapAction f (Substraction   x) = Substraction   (f x)
mapAction f (Multiplication x) = Multiplication (f x)
mapAction f (Division       x) = Division       (f x)
mapAction f (Module         x) = Module         (f x)

-- Gets the second argument. Receives an action and returns the string of that action
getSndArg :: Action -> String
getSndArg (Addition         x) = x
getSndArg (Substraction     x) = x
getSndArg (Multiplication   x) = x
getSndArg (Division         x) = x
getSndArg (Module           x) = x

-- Render a given 'Value'
-- When the function receives any value it will render:
-- g(x) being x the string of that value. Concatenated by
--      if the input string is empty, it woll render 0, otherwise it will render the input string reversed
-- f(a) being (a, y) the possible following action (operation)
--      if a and y are null, then don't print anything, otherwise, print f(a), which would be the operator
--      concatenated by the string of the following action, which can be empty or can be another string
renderValue :: Value -> String
renderValue (Value x action) =                              
    g x ++ f a ++ (if null y then "" else g y)
    where
        (a, y) =
            case action of
                Nothing                         -> ("", "")
                Just (Addition          arg)    -> ("+", arg)
                Just (Substraction      arg)    -> ("-", arg)
                Just (Multiplication    arg)    -> ("*", arg)
                Just (Division          arg)    -> ("/", arg)
                Just (Module            arg)    -> ("%", arg)
        f "" = ""
        f l = " " ++ l ++ " "
        g "" = "0"
        g xs = reverse xs

---------------------------------------------------------------------

-- In order to render a dot, a condition must be checked, that there's no dots in the current value
-- In the case where an action is void, then a new Value will be rendered with only the string of the value
-- In any other case, create a new value with x being the string and let mapAction decide what is the action
-- to take
enterDot :: Value -> Value
enterDot (Value x action) =
    let f xs = if '.' `elem` xs then xs else '.' : xs
    in case action of
        Nothing -> Value (f x) Nothing
        Just  a -> Value x (Just (mapAction f a))

-- A Char and a Value are received, and the function returns a Value
-- the char received has the name ch, and the value received will have the form (x action)
-- in case of the action being Nothing:
--      create a new value with only a string, made by the contcatenation of ch + the current value
-- if the action is not Nothing, then:
--      create a new value and assign the current value, and for the action
--      map the character just entered using mapAction
enterDigit :: Char -> Value -> Value
enterDigit ch (Value x action) =
    case action of
        Nothing -> Value (ch:x) Nothing
        Just a  -> Value x $ Just $ mapAction (ch:) a

backspace :: Value -> Value
backspace (Value x action) = 
    case action of
        Nothing -> Value (drop 1 x) Nothing
        Just a  -> Value x (Just $ mapAction (drop 1) a)

operator :: (String -> Action) -> Value -> Value
operator op value =
    let (Value x action) = equals value
    in Value x $ Just $
        case action of
            Nothing -> op ""
            Just a  -> op (getSndArg a)

clearEntry :: Value -> Value
clearEntry (Value x action) = 
    case action of
        Nothing -> Value "" Nothing
        Just a  -> 
            if null (getSndArg a)
                then Value "" Nothing
                else Value x (Just $ mapAction (const "") a)

clearAll :: Value -> Value
clearAll = const (Value "" Nothing)

-- Cast double to int
toInt :: Double -> Int
toInt x = round x

-- This is a function for dobule to int casting. Used for the mod operator
moduleForDoubles :: Double -> Double -> Double
moduleForDoubles n m = ans
    where
        x = toInt n
        y = toInt m
        res = mod x y
        ans = fromIntegral res

equals :: Value -> Value
equals (Value x action) =
    case action of
        Nothing -> Value x Nothing
        Just  a ->
            if null (getSndArg a)
                then Value x action
                else Value result Nothing
                    where
                        g  :: String -> Double
                        g ""       = 0
                        g ('.':xs) = g ('0':'.':xs)
                        g xs       = read (reverse xs)
                        x' = g x
                        y' = g (getSndArg a)
                        result = reverse . show $
                            case a of
                                Addition        _ -> x' + y'
                                Substraction    _ -> x' - y'
                                Multiplication  _ -> x' * y'
                                Division        _ -> x' / y'
                                Module          _ -> moduleForDoubles x' y'

---------------------------------------------------------------------
-- Updateds the calculator display
updateDisplay :: Entry -> Value -> IO()
updateDisplay display value = set display [ entryText := renderValue value ]

mkButton :: IORef Value             -- 'IO Ref' to the calculator state
            -> Entry                -- Display Update
            -> String               -- Button label
            -> (Value -> Value)     -- How this button affects the calculator state
            -> IO Button            -- Resulting button object
mkButton state display label mutateState = do
    btn <- buttonNew
    set btn [ buttonLabel := label ]
    btn `on` buttonActivated $ do       -- Register a handler that will fire on button activation
        value <- atomicModifyIORef state $ \x -> let r = mutateState x in (r, r) -- atomicModifyIORef modifies given IORef atomically
        updateDisplay display value
    return btn