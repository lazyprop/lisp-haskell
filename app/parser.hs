import Control.Monad.State

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)


coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank, Unlocked)

push Locked   = (Tut , Locked)
push Unlocked = (Open, Locked)


coinS, pushS :: State TurnstileState TurnstileOutput
coinS = state coin
pushS = state push


--mondayS :: State TurnstileState [TurnstileOutput]
--mondayS = sequence [coinS, pushS, pushS, coinS, pushS]

mondayS :: State TurnstileState [TurnstileOutput] -- state transfomer
mondayS = do
    a1 <- coinS
    a2 <- pushS
    a3 <- pushS
    a4 <- coinS
    a5 <- pushS
    return [a1, a2, a3, a4, a5]
