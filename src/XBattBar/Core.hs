module XBattBar.Core (start) where
import Prelude hiding (Left, Right)
import Data.Bits ((.|.))
import Control.Monad (forever)
import Graphics.X11.Types
import Graphics.X11.Xlib.Types hiding (Position)
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Color hiding (allocColor)
import System.Posix.IO.Select
import System.Posix.IO.Select.Types
import System.Posix.Types
import System.Time (getClockTime, ClockTime)

import XBattBar.Types
import XBattBar.Backend
import XBattBar.Widgets

data XBattBar = XBattBar {
                    options  :: Options,
                    bar      :: ProgressBar,
                    popup    :: Label,
                    colorAC  :: (Pixel, Pixel),
                    colorBat :: (Pixel, Pixel)
                }

-- | retrieve screen geometry
getScreenRect :: XContext -> Rectangle
getScreenRect ctx = Rectangle 0 0 sw sh
                            where dpy' = dpy ctx
                                  screen' = screen ctx
                                  sw = fromIntegral $ displayWidth dpy' screen'
                                  sh = fromIntegral $ displayHeight dpy' screen'

-- | transform screen geometry into bar geometry
getBarRect :: Position -> Dimension -> Rectangle -> Rectangle
getBarRect pos th rect = case pos of
                        Top -> rect { rect_height = th }
                        Bottom -> getBarRect Top th $ rect { rect_y = fromIntegral $ rect_height rect - th }
                        Left -> rect { rect_width = th }
                        Right -> getBarRect Left th $ rect { rect_x = fromIntegral $ rect_width rect - th }

-- | transform screen geometry into popup window geometry
getPopupRect :: Rectangle -> Rectangle
getPopupRect scr = Rectangle x y w h
    where x = fromIntegral $ rect_width scr `div` 2 - w `div` 2
          y = fromIntegral $ rect_height scr `div` 2 - h `div` 2
          w = 240
          h = 60

-- | get pixels from color names
getColors :: XContext -> Options -> IO ((Pixel, Pixel), (Pixel, Pixel))
getColors ctx opts = do
    let dpy' = dpy ctx
        screen' = screen ctx
        allocColor = allocNamedColor dpy' (defaultColormap dpy' screen')
        allocPixel f = (allocColor $ f opts) >>= (\(c,_) -> return $ color_pixel c)
    [a1, a2, b1, b2] <- mapM allocPixel [chargeColorAC, dischargeColorAC, chargeColorBat, dischargeColorBat]
    return ((a1, a2), (b1, b2))

start :: Options -> IO ()
start opts = do
    dpy' <- openDisplay ""
    let screen' = defaultScreen dpy'
    root <- rootWindow dpy' screen'
    let ctx = XContext dpy' screen' root
    let geom' = getBarRect (position opts) (fromIntegral $ thickness opts) (getScreenRect ctx)
    let fg = whitePixel dpy' screen'
    let bg = blackPixel dpy' screen'
    let orientation' x | x == Top || x == Bottom = Horizontal
                       | otherwise = Vertical
    bar' <- mkProgressBar ctx geom' fg bg (orientation' $ position opts) (exposureMask .|. enterWindowMask .|. leaveWindowMask)
    let popupGeom = getPopupRect $ getScreenRect ctx
    popup' <- mkLabel ctx popupGeom bg fg "fixed" [] noEventMask
    (ac, bat) <- getColors ctx opts
    run $ XBattBar opts bar' popup' ac bat
    return ()

handleTimeout :: XBattBar -> IO ()
handleTimeout xbb = drawWidget (bar xbb)

-- | dispatch X11 events to widgets
handleEvents :: XBattBar -> IO ()
handleEvents xbb = do
    let bar'     = bar xbb
        popup'   = popup xbb
        dpy'     = dpy $ xContext bar'
        barWin   = window $ widgetContext bar'
        popupWin = window $ widgetContext popup'
    n <- pending dpy'
    case n of 
        0 -> return ()
        _ -> allocaXEvent $ \ev -> do
                nextEvent dpy' ev
                ty <- get_EventType ev
                win <- get_Window ev
                let dispatch w e t | t == enterNotify = displayWidget popup' >> drawWidget popup'
                                   | t == leaveNotify = hideWidget popup'
                                   | w == barWin = handleWidgetEvent bar' e t
                                   | w == popupWin = handleWidgetEvent popup' e t
                                   | otherwise = return ()
                dispatch win ev ty
                handleEvents xbb

selectWrapper fd int eventH timeoutH = do
    n <- select' [fd] [] [] (Time $ CTimeval int 0)
    case n of 
        Nothing -> error "select() error"
        Just ([], [], []) ->  return timeoutH
        Just _ ->  return eventH

-- | necessary transformations on state change
applyState :: XBattBar -> Double -> Power -> ClockTime -> XBattBar
applyState xbb charge state time =
    let bar' = bar xbb
        popup' = popup xbb
        (af, ab) = colorAC xbb
        (bf, bb) = colorBat xbb
        text' = [(show state)++"-powered: battery level is "++(show $ floor $ 100 * charge)++"%", show time]
    in case state of
        AC -> xbb { bar =
                    bar' { colorBack = ab,
                           colorBar = af,
                           progress = charge },
                    popup = popup' { text = text' }
                  }
        Battery -> xbb { bar =
                    bar' { colorBack = bb,
                           colorBar = bf,
                           progress = charge },
                    popup = popup' { text = text' }
                    }

-- | main loop
run :: XBattBar -> IO ()
run xbb = do
    let bar' = bar xbb
        dpy' = dpy $ xContext bar'
        int  = fromIntegral $ interval $ options xbb
    displayWidget bar'
    sync dpy' False
    let fd = Fd $ connectionNumber dpy'
    forever $ do
        c <- getCharge
        s <- getPower
        t <- getClockTime
        let xbb' = applyState xbb c s t
        drawWidget $ bar xbb'
        selectWrapper fd int handleEvents handleTimeout >>= (\h -> h xbb')
