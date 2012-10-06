module XBattBar.Widgets (mkWidget, mkProgressBar) where

import Data.Word
import Data.Bits
import Control.Monad
import Graphics.X11.Types
import Graphics.X11.Xlib.Types hiding (Position)
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Window
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Context
import Graphics.X11.Xlib.Color
import Graphics.X11.Xlib.Extras (unmapWindow)

import XBattBar.Types

mkWidget :: XContext -> Rectangle -> EventMask -> (XContext -> ExtContext -> b) -> IO b
mkWidget ctx geom mask which = do
    let attrmask = cWOverrideRedirect
        borderWidth = 0
        dpy'    = dpy ctx
        screen' = screen ctx
        parent' = parent ctx
    window <- allocaSetWindowAttributes $
        \attrs -> do 
                    set_override_redirect attrs True
                    createWindow dpy' parent'
                                (rect_x geom)
                                (rect_y geom)
                                (rect_width geom)
                                (rect_height geom)
                                borderWidth
                                (defaultDepth dpy' screen')
                                inputOutput
                                (defaultVisual dpy' screen')
                                attrmask
                                attrs
    gc <- createGC dpy' window
    selectInput dpy' window mask
    let ectx = ExtContext window geom gc
    return $ which ctx ectx

getIndicatorRect :: Orientation -> Double -> Rectangle -> Rectangle
getIndicatorRect pos perc rect = case pos of
                        Horizontal ->
                            rect { rect_x = p (rect_width rect) - fromIntegral (rect_width rect), rect_y = 0 }
                        Vertical ->
                            rect { rect_y = fromIntegral (rect_height rect) - p (rect_height rect), rect_x = 0 }
                        where p x = floor $ perc * fromIntegral x

instance XWidget ProgressBar
    where xContext = pbXContext
          widgetContext = pbExContext
          drawWidget bar = do
                let ctx'    = xContext bar
                    ectx'   = widgetContext bar
                    dpy'    = dpy ctx'
                    screen' = screen ctx'
                    window' = window ectx'
                    gc'     = gc ectx'
                    geom'   = geom ectx'
                    fg      = colorBar bar
                    bg      = colorBack bar
                setForeground dpy' gc' bg
                fillRectangles dpy' window' gc' [geom']
                setForeground dpy' gc' fg
                fillRectangles dpy' window' gc' [getIndicatorRect (orientation bar) (progress bar) geom']
                flush dpy'

mkProgressBar :: XContext -> Rectangle -> Pixel -> Pixel -> Orientation -> EventMask -> IO ProgressBar
mkProgressBar xctx geom fg bg orientation mask = do
    let dpy' = dpy xctx
        screen' = screen xctx
    f <- mkWidget xctx geom mask ProgressBar
    return $ f fg bg 0.0 orientation
