module XBattBar.Types (Options(..), Position(..)) where

data Position = Top | Bottom | Left | Right
                deriving (Show, Read)

data Options = Options {
                    onTop               :: Bool,
                    thickness           :: Int,
                    interval            :: Int,
                    chargeColorAC       :: String,
                    dischargeColorAC    :: String,
                    chargeColorBat      :: String,
                    dischargeColorBat   :: String,
                    position            :: Position
                } deriving (Show)
