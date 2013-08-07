import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk

data IORState = IORScons
    { score :: Int
    , pacStream :: [(Int, Int)]
    , ghostStream :: [(Int, Int)]
    , dangerStream :: [Bool]
    , fruit :: [(Int, Int)]
    }

initState = IORScons 0 pacmanPosition ghostPosition (dangerous 7) []

main = do
    initGUI
    window <- windowNew
    window `on` deleteEvent $ liftIO mainQuit >> return False 
    populate window
    widgetShowAll window
    mainGUI

populate w = do 
    windowSetDefaultSize w 800 600
    mainVBox <- vBoxNew False 0
    btn <- buttonNewWithLabel "Sart game"
    boxPackStart mainVBox btn PackNatural 5
    table <- tableNew 11 7 True
    images <- populateTable table
    btn `on` buttonActivated $ newGame images
    boxPackEnd mainVBox table PackGrow 5
    w `containerAdd` mainVBox
    return (images)

populateTable tb = do
    images <- mapM (attachImg tb "imgs/empty.png") 
                   [(x, y) | x <- [0 .. 6], y <- [0 .. 10]]
    let roadPositions = [(a, b) | a <- [1, 5], b <- [1 .. 9]] ++
                        [(a, b) | a <- [2, 3, 4], b <- [1, 5, 9]]
    mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $
          filter ((`elem` roadPositions) . fst) images
    mapM_ (flip imageSetFromFile "imgs/pacman.png" . snd) $
          filter (( == (5, 1)) . fst) images
    mapM_ (flip imageSetFromFile "imgs/ghost.png" . snd) $
          filter (( == (1, 9)) . fst) images
    return images


attachImg tb path (x, y) = do
    img <- imageNewFromFile path
    tableAttach tb img x (x + 1) y (y + 1) [] [] 1 1
    return ((x, y), img)

newGame images = do
    ref <- newIORef initState
    timeoutAdd (evolve ref images) 500
    return () 

evolve state images = do
    s <- readIORef state
    let newPac = tail $ pacStream s
    let newGhost = tail $ ghostStream s
    let newDanger = tail $ dangerStream s
    let nFruit = if head newDanger then head newGhost : fruit s else fruit s
    let newFruit = filter (/= head newPac) nFruit
    let newScore = (score s) +
                   if head newGhost `elem` nFruit then 1 else 0  
    updatePosition (pacStream s) (ghostStream s) newFruit images
    writeIORef state (IORScons newScore newPac newGhost newDanger newFruit)
    return $ not ( (\x y z -> x == y && z) 
            (head newPac) (head newGhost) (head newDanger))

updatePosition pac ghost fruit images = do
    
    mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $
          filter ((\x -> x == head pac || x == head ghost) . fst) images
    mapM_ (flip imageSetFromFile "imgs/fruit.png" . snd) $
          filter ((`elem` fruit) . fst) images
    mapM_ (flip imageSetFromFile "imgs/pacman.png" . snd) $
          filter (( == (head $ tail pac)) . fst) images 
    mapM_ (flip imageSetFromFile "imgs/ghost.png" . snd) $
          filter (( == (head $ tail ghost)) . fst) images
    return()

pacmanPosition = pacmanPath ++ tail pacmanPosition 
                 where pacmanPath = 
                           pacmanHalfPath ++ 
                           tail (reverse pacmanHalfPath)
                               where pacmanHalfPath = 
                                         [(x, 1) | x <- [5, 4 .. 1]] ++
                                         [(1, y) | y <- [2 .. 5]] ++
                                         [(x, 5) | x <- [2 .. 5]] ++
                                         [(5, y) | y <- [6 .. 9]] ++
                                         [(x, 9) | x <- [4, 3 .. 1]]

ghostPosition = ghostPath ++ tail ghostPosition
                where ghostPath = [(x, 9) | x <- [1 .. 5]] ++
                                  [(5, y) | y <- [8, 7 .. 1]] ++
                                  [(x, 1) | x <- [4, 3 .. 1]] ++
                                  [(1, y) | y <- [2 .. 9]]

dangerous n = frankfurt 
                  where frankfurt =
                         ((take (n - 1) $ repeat False) ++ [True]) ++ frankfurt
