module XBattBar.Types (Options(..), Position(..), Orientation(..), XContext(..), ExtContext(..), ProgressBar(..), Label(..), XWidget(..)) where

import Graphics.X11.Types
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Window (mapWindow)
import Graphics.X11.Xlib.Extras (unmapWindow)
import Graphics.X11.Xlib.Types hiding (Position)

data Position = Top | Bottom | Left | Right
                deriving (Eq, Show)

data Orientation = Vertical | Horizontal

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

data XContext = XContext {
                  dpy    :: Display,
                  screen :: ScreenNumber,
                  parent :: Window
                }

data ExtContext = ExtContext {
                   window :: Window,
                   geom   :: Rectangle,
                   gc     :: GC
                  }


class XWidget a where
            xContext            :: a -> XContext
            widgetContext       :: a -> ExtContext
            drawWidget          :: a -> IO ()
            displayWidget       :: a -> IO ()
            hideWidget          :: a -> IO ()
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

data ProgressBar = ProgressBar {
                 pbXContext     :: XContext,
                 pbExContext    :: ExtContext,
                 colorBack      :: Pixel,
                 colorBar       :: Pixel,
                 progress       :: Double,
                 orientation    :: Orientation
               }

data Label = Label {
                 colorBG        :: Pixel,
                 colorFont      :: Pixel,
                 font           :: Font,
                 text           :: String
             }
