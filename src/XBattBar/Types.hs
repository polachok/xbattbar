module XBattBar.Types (Options(..)) where

data Options = Options {
                    onTop               :: Bool,
                    thickness           :: Int,
                    interval            :: Int,
                    chargeColorAC       :: String,
                    dischargeColorAC    :: String,
                    chargeColorBat      :: String,
                    dischargeColorBat   :: String,
                    position            :: String
                } deriving (Show)
