module Main (main) where
import System.Environment
import System.Console.GetOpt

import XBattBar.Types
import XBattBar.Core (start)

defaultOptions :: Options
defaultOptions = Options {
                      onTop             = False,
                      thickness         = 2,
                      interval          = 5,
                      chargeColorAC     = "green",
                      dischargeColorAC  = "olive drab",
                      chargeColorBat    = "blue",
                      dischargeColorBat = "red",
                      position          = "top"
                 }

options :: [ OptDescr (Options -> Options) ]
options =
    [ Option "a" [] (NoArg (\opts -> opts { onTop = True }))
        "Always on top",
      Option "t" [] (ReqArg (\x s -> s { thickness = read x }) "PX")
        "Thickness",
      Option "p" [] (ReqArg (\x s -> s { interval = read x }) "PX")
        "Polling interval",
      Option "I" [] (ReqArg (\x s -> s { chargeColorAC = x }) "PX")
        "Charge color when on AC",
      Option "O" [] (ReqArg (\x s -> s { dischargeColorAC = x }) "PX")
        "Discharge color when on AC",
      Option "o" [] (ReqArg (\x s -> s { dischargeColorBat = x }) "PX")
        "Charge color when on battery",
      Option "i" [] (ReqArg (\x s -> s { chargeColorBat = x }) "PX")
        "Discharge color when on battery",
      Option "h" [] (NoArg (usage))
        "Print help message"
    ]

nonoptions :: Options -> [String] -> Options
nonoptions opts [] = opts
nonoptions opts ["top"] = opts { position = "top" } 
nonoptions opts ["bottom"] = opts { position = "bottom" } 
nonoptions opts ["left"] = opts { position = "left" } 
nonoptions opts ["right"] = opts { position = "right" } 
nonoptions _ _ = error "wrong position"

usage = error $ "usage: xbattbar [-a] [-h|v] [-p sec] [-t thickness] " ++
              "[-I color] [-O color] [-i color] [-o color] " ++
              "[ top | bottom | left | right ]"

main = do
    args <- getArgs
    
    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    let opts = foldl (flip id) defaultOptions actions
    start $ nonoptions opts nonOptions
