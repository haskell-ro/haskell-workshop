% GTK
% Mihai Maruseac
% 11.07.2013

# Install

~~~~ {.bash}
cabal install gtk2hs-buildtools
cabal install glib
cabal install gtk
~~~~

# Main Window

~~~~ {.haskell}
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
~~~~

~~~~ {.haskell}
main = do
    initGUI
    window <- windowNew
    window `on` deleteEvent $
        liftIO mainQuit >> return False
    widgetShowAll window
    mainGUI
~~~~

# The Pacman in a new world

Live demo

# Other GUI frameworks

* wxHaskell
* qtHaskell
* lGTK
    * lenses and GTK
* reactive-banana
    * functional reactive programming (FRP)
