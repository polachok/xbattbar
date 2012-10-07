module XBattBar.Types (Options(..), Position(..), Orientation(..), XContext(..), ExtContext(..), XWidget(..)) where

import Graphics.X11.Types (expose, Window, EventType)
import Graphics.X11.Xlib.Event (XEventPtr)
import Graphics.X11.Xlib.Font (FontStruct)
import Graphics.X11.Xlib.Window (mapWindow)
import Graphics.X11.Xlib.Extras (unmapWindow)
import Graphics.X11.Xlib.Types hiding (Position)

data Position = Top | Bottom | Left | Right
                deriving (Eq, Show)

-- | progress bar vartiants
data Orientation = Vertical | Horizontal

-- | command-line options map to this
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

-- | basic X11 context
data XContext = XContext {
                  dpy    :: Display,
                  screen :: ScreenNumber,
                  parent :: Window
                }
-- | extended X11 context
data ExtContext = ExtContext {
                   window :: Window,
                   geom   :: Rectangle,
                   gc     :: GC
                  }

-- | XWidget is an X11 window with some context attached
class XWidget a where
            xContext            :: a -> XContext
            widgetContext       :: a -> ExtContext
            -- | makes the actual widget drawing
            drawWidget          :: a -> IO ()
            -- | display widget on screen
            displayWidget       :: a -> IO ()
            -- | hide widget
            hideWidget          :: a -> IO ()
            -- | handle X11 events for the widget
            handleWidgetEvent   :: a -> XEventPtr -> EventType -> IO ()

            displayWidget a = do
                let window' = window $ widgetContext a
                    dpy'    = dpy $ xContext a
                mapWindow dpy' window'

            hideWidget a = do
                let window' = window $ widgetContext a
                    dpy'    = dpy $ xContext a
                unmapWindow dpy' window'

            handleWidgetEvent a ev et | et == expose = drawWidget a
                                      | otherwise = return ()
