module XBattBar.Backend (getCharge, getPower, Power(..)) where
import System.Environment

data Power = AC | Battery
            deriving (Show)

-- | retrieves battery charge
getCharge :: IO Double
getCharge = getChargeLinux

-- | retrieves power status
getPower :: IO Power
getPower  = getPowerLinux

-- | get charge from ACPI on linux
getChargeLinux :: IO Double
getChargeLinux = do
    let path = "/sys/bus/acpi/drivers/battery/PNP0C0A:00/power_supply/BAT0/"
    fullS <- readFile $ path++"energy_full"
    nowS <- readFile $ path++"energy_now"
    let f = read fullS
    let n = read nowS
    return (n / f)

-- | get power state from ACPI on linux
getPowerLinux :: IO Power
getPowerLinux = do
    let path = "/sys/bus/acpi/drivers/ac/ACPI0003:00/power_supply/AC/online"
    s <- readFile path
    return $ case (read s) of 
                0 -> Battery
                1 -> AC
