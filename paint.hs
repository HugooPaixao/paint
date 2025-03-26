{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import CodeWorld
import qualified Data.Text as T

type World = ([Figura], Estado, Modo, Point, Color, Bool)

data Modo = Ret | Cir | MaoLivre | Apagar | Mover
          | TrazParaFrente | TrazParaTras
          | AlterarPreenchimento | SelecionarCor | AlterarCor
          deriving (Eq, Show)

data Estado = NaoIniciado
            | DesenhandoRC { pontoInicial :: Point, pontoFinal :: Point }
            | DesenhandoMaoLivre { pontos :: [Point], ultimoPonto :: Point }
            | MovendoFigura { figuraIndice :: Int, origem :: Point }

data Figura = Circulo { centro :: Point, raio :: Double, preenchido :: Bool, cor :: Color }
            | Retangulo { ponto1 :: Point, ponto2 :: Point, preenchido :: Bool, cor :: Color }
            | DesenhoMaoLivre { pontosMaoLivre :: [Point], preenchido :: Bool, cor :: Color }

main = activityOf mundoInicial atualiza visualiza

mundoInicial :: World
mundoInicial = ([], NaoIniciado,Cir , (0, 0), black, False)

cursor :: Modo -> Point -> Bool -> Picture
cursor m (x, y) preenche = translated x y (crs m preenche)  where
    crs Ret preenche = if preenche then colored gray (solidRectangle 0.4 0.4) else colored gray (rectangle 0.4 0.4)
    crs Cir preenche = if preenche then colored gray (solidCircle 0.2) else colored gray (circle 0.2)
    crs MaoLivre _ = colored blue (lettering "liv")
    crs Apagar _ = colored red (lettering "apa")
    crs Mover _ = colored yellow (lettering "mov")
    crs TrazParaFrente _ = colored orange (lettering "frt")
    crs TrazParaTras _ = colored orange (lettering "trás")
    crs AlterarPreenchimento _ = colored black (lettering "pre")
    crs SelecionarCor _ = colored black (lettering " ")
    crs AlterarCor _ = colored purple (lettering "AltCor")

figParaPic :: Figura -> Picture
figParaPic (Circulo c r preenchido cor) = translated (fst c) (snd c) (if preenchido then colored cor
                                      (solidCircle r) else colored cor (circle r))
figParaPic (Retangulo p1 p2 preenchido cor) =
    let pq = vectorDifference p2 p1
        (x, y) = vectorSum p1 (scaledVector 0.5 pq)
        (w, h) = pq
    in translated x y (if preenchido then colored cor
        (solidRectangle w h) else colored cor (rectangle w h))
figParaPic (DesenhoMaoLivre pts preenchido cor) =
    if preenchido
    then colored cor (solidPolygon pts)
    else colored cor (polyline pts)

figsParaPic :: [Figura] -> Picture
figsParaPic = foldr (&) blank . map figParaPic

estadoParaPic :: Estado -> Modo -> Picture
estadoParaPic NaoIniciado _ = blank
estadoParaPic (DesenhandoRC p q) Cir = colored gray (translated x y (circle r))  where
    (x, y) = p
    r = vectorLength (vectorDifference p q)
estadoParaPic (DesenhandoRC p q) Ret = colored gray (translated x y (rectangle w h))  where
    pq = vectorDifference q p
    (x, y) = vectorSum p (scaledVector 0.5 pq)
    (w, h) = pq
estadoParaPic (DesenhandoMaoLivre pts _) MaoLivre = colored gray (polyline (reverse pts))
estadoParaPic (MovendoFigura _ _) Mover = blank

alteraPreenchimento :: Point -> World -> World
alteraPreenchimento p (cs, st, modo, pc, cor, preenche) =
    let fig = encontraFigura 0 cs p
        figurasAtualizadas = if fig == -1
            then cs
            else take fig cs ++ [alternaPreenchimento (cs !! fig)] ++ drop (fig + 1) cs
    in (figurasAtualizadas, st, modo, pc, cor, preenche)

alternaPreenchimento :: Figura -> Figura
alternaPreenchimento (Circulo c r preenchido cor) = Circulo c r (not preenchido) cor
alternaPreenchimento (Retangulo p1 p2 preenchido cor) = Retangulo p1 p2 (not preenchido) cor
alternaPreenchimento (DesenhoMaoLivre pts preenchido cor) = DesenhoMaoLivre pts (not preenchido) cor

paraTexto :: T.Text -> String
paraTexto = T.unpack            

atualiza :: Event -> World -> World
atualiza (KeyPress " ") (cs, st, modo, pc, cor, preenche) =
    (cs, st, modo, pc, cor, not preenche)
atualiza (KeyPress tecla) world = atualizaKeyPress (paraTexto tecla) world
atualiza (PointerPress p) world = atualizaPointerPress p world
atualiza (PointerMovement q) world = atualizaPointerMovement q world
atualiza (PointerRelease p) world = atualizaPointerRelease p world
atualiza _ world = world

visualiza :: World -> Picture
visualiza (cs, st, m, pc, corAtual, preenche) =
    case st of
        MovendoFigura fig orig ->
            let figuraMovida = figParaPic (moverPara (cs !! fig) (vectorDifference pc orig))
            in cursor m pc preenche & colored gray figuraMovida & figsParaPic (take fig cs ++ drop (fig + 1) cs) &
            estadoParaPic st m & if m == SelecionarCor then painelCores corAtual else blank
        _ -> cursor m pc preenche & figsParaPic cs & estadoParaPic st m & if m == SelecionarCor then painelCores corAtual else blank


atualizaModo :: Modo -> World -> World
atualizaModo novoModo (cs, st, _, pc, cor, preenche) = (cs, st, novoModo, pc, cor, preenche)

atualizaKeyPress :: String -> World -> World
atualizaKeyPress keyStr world =
    case filter (\(k, _) -> k == keyStr) teclasPressionadas of
        [(_, f)] -> f world
        _        -> world

teclasPressionadas:: [(String, World -> World)]
teclasPressionadas =
    [ ("D", atualizaModo Apagar)
    , ("M", atualizaModo Mover)
    , ("L", atualizaModo MaoLivre)
    , ("R", atualizaModo Ret)
    , ("C", atualizaModo Cir)
    , ("Up", atualizaModo TrazParaFrente)
    , ("Down", atualizaModo TrazParaTras)
    , ("P", atualizaModo AlterarPreenchimento)
    , ("S", atualizaModo SelecionarCor)
    , ("A", atualizaModo AlterarCor)
    ]

atualizaPointerMovement :: Point -> World -> World
atualizaPointerMovement q world@(cs, st, modo, _, cor, preenche) =
    case st of
        MovendoFigura fig orig ->
            let deslocamento = vectorDifference q orig
                figuraAtualizada = moverPara (cs !! fig) deslocamento
            in (atualizaFigura fig figuraAtualizada cs, MovendoFigura fig q, modo, q, cor, preenche)
        DesenhandoMaoLivre points _ ->
            (cs, DesenhandoMaoLivre (q:points) q, modo, q, cor, preenche)
        DesenhandoRC p _ ->
            (cs, DesenhandoRC p q, modo, q, cor, preenche)
        _ -> (cs, st, modo, q, cor, preenche)

atualizaPointerRelease :: Point -> World -> World
atualizaPointerRelease p (cs, MovendoFigura fig _, Mover, _, cor, preenche) =
    (cs, NaoIniciado, Mover, p, cor, preenche)
atualizaPointerRelease p (cs, DesenhandoMaoLivre points _, MaoLivre, _, cor, preenche) =
    ((DesenhoMaoLivre (reverse points) False cor) : cs, NaoIniciado, MaoLivre, p, cor, preenche)
atualizaPointerRelease p (cs, DesenhandoRC p1 p2, Cir, _, cor, preenche) =
    ((Circulo p1 (vectorLength (vectorDifference p2 p1)) preenche cor) : cs, NaoIniciado, Cir, p, cor, preenche)
atualizaPointerRelease p (cs, DesenhandoRC p1 p2, Ret, _, cor, preenche) =
    ((Retangulo p1 p2 preenche cor) : cs, NaoIniciado, Ret, p, cor, preenche)
atualizaPointerRelease _ world = world

pontoDentroFigura :: Point -> Figura -> Bool
pontoDentroFigura (x, y) (Circulo (cx, cy) r _ _) = (x - cx)^2 + (y - cy)^2 <= r^2
pontoDentroFigura (x, y) (Retangulo (x1, y1) (x2, y2) _ _) =
    let xmin = min x1 x2
        xmax = max x1 x2
        ymin = min y1 y2
        ymax = max y1 y2
    in x >= xmin && x <= xmax && y >= ymin && y <= ymax
pontoDentroFigura p (DesenhoMaoLivre pts _ _) = any (distMenorQue p) pts
  where
    distMenorQue (x, y) (px, py) = (x - px)^2 + (y - py)^2 < 0.25

apagaFig :: Int -> [Figura] -> [Figura]
apagaFig n cs = take n cs ++ drop (n + 1) cs

atualizaFigura :: Int -> Figura -> [Figura] -> [Figura]
atualizaFigura n novaFig cs = take n cs ++ [novaFig] ++ drop (n + 1) cs

moverPara :: Figura -> Vector -> Figura
moverPara (Circulo c r preenchido cor) v = Circulo (vectorSum c v) r preenchido cor
moverPara (Retangulo p1 p2 preenchido cor) v = Retangulo (vectorSum p1 v) (vectorSum p2 v) preenchido cor
moverPara (DesenhoMaoLivre pts preenchido cor) v = DesenhoMaoLivre (map (`vectorSum` v) pts) preenchido cor

encontraFigura :: Int -> [Figura] -> Point -> Int
encontraFigura _ [] _ = -1
encontraFigura n (f:fs) p
  | pontoDentroFigura p f = n
  | otherwise = encontraFigura (n+1) fs p

atualizaPointerPress :: Point -> World -> World
atualizaPointerPress p (cs, _, AlterarCor, _, corAtual, preenche) =
    let fig = encontraFigura 0 cs p
    in if fig == -1
       then (cs, NaoIniciado, AlterarCor, p, corAtual, preenche)
       else (alteraCorFigura fig corAtual cs, NaoIniciado, AlterarCor, p, corAtual, preenche)
atualizaPointerPress p (cs, _, Apagar, _, cor, preenche) =
    let fig = encontraFigura 0 cs p
    in if fig == -1
       then (cs, NaoIniciado, Apagar, p, cor, preenche)
       else (apagaFig fig cs, NaoIniciado, Apagar, p, cor, preenche)
atualizaPointerPress p (cs, _, Mover, _, cor, preenche) =
    let fig = encontraFigura 0 cs p
    in if fig == -1
       then (cs, NaoIniciado, Mover, p, cor, preenche)
       else (cs, MovendoFigura fig p, Mover, p, cor, preenche)
atualizaPointerPress p (cs, _, MaoLivre, _, cor, preenche) =
    (cs, DesenhandoMaoLivre [p] p, MaoLivre, p, cor, preenche)
atualizaPointerPress p (cs, _, Ret, _, cor, preenche) =
    (cs, DesenhandoRC p p, Ret, p, cor, preenche)
atualizaPointerPress p (cs, _, Cir, _, cor, preenche) =
    (cs, DesenhandoRC p p, Cir, p, cor, preenche)
atualizaPointerPress p (cs, _, TrazParaFrente, _, cor, preenche) =
    trazParaFrente (cs, NaoIniciado, TrazParaFrente, p, cor, preenche)
atualizaPointerPress p (cs, _, TrazParaTras, _, cor, preenche) =
    trazParaTras (cs, NaoIniciado, TrazParaTras, p, cor, preenche)
atualizaPointerPress p (cs, _, AlterarPreenchimento, _, cor, preenche) =
    let fig = encontraFigura 0 cs p
    in if fig == -1
       then (cs, NaoIniciado, AlterarPreenchimento, p, cor, preenche)
       else (atualizaPreenchimento fig cs, NaoIniciado, AlterarPreenchimento, p, cor, preenche)
atualizaPointerPress p (cs, _, SelecionarCor, _, cor, preenche) =
    (cs, NaoIniciado, SelecionarCor, p, corEscolhida p, preenche)

trazParaFrente :: World -> World
trazParaFrente (cs, st, m, p, c, preenche) =
    let fig = encontraFigura 0 cs p
    in if fig == -1
       then (cs, st, m, p, c, preenche)
       else (moveFiguraparaFrente fig cs, st, m, p, c, preenche)

moveFiguraparaFrente :: Int -> [Figura] -> [Figura]
moveFiguraparaFrente i cs = figAtual : (removeFigura i cs)
  where figAtual = cs !! i

trazParaTras :: World -> World
trazParaTras (cs, st, m, p, c, preenche) =
    let fig = encontraFigura 0 cs p
    in if fig == -1
       then (cs, st, m, p, c, preenche)
       else (moveFiguraparaTras fig cs, st, m, p, c, preenche)

moveFiguraparaTras :: Int -> [Figura] -> [Figura]
moveFiguraparaTras i cs = (removeFigura i cs) ++ [figAtual]
  where figAtual = cs !! i

removeFigura :: Int -> [Figura] -> [Figura]
removeFigura _ [] = []
removeFigura i cs = take i cs ++ drop (i + 1) cs

atualizaPreenchimento :: Int -> [Figura] -> [Figura]
atualizaPreenchimento i cs =
    let figura = cs !! i
    in case figura of
         Circulo c r preenchido cor -> atualizaFigura i (Circulo c r (not preenchido) cor) cs
         Retangulo p1 p2 preenchido cor -> atualizaFigura i (Retangulo p1 p2 (not preenchido) cor) cs

painelCores :: Color -> Picture
painelCores corAtual = translated 0 posYPainel (rectangle 2 1 &
                      translated 0 0 (colored corAtual (solidRectangle 2 1)) &
                      translated 0 (-0.75) (rectangle 16.5 0.5) & painel)
  where
    painel = pictures $ zipWith quadCol [-8, -7.5 .. 8] pColors
    quadCol x col = colored col . translated x (-0.75) $ solidRectangle 0.5 0.5

pColors :: [Color]
pColors = [black, white, gray, red, green, blue, yellow, orange, brown, pink, purple] ++ assortedColors

posYPainel :: Double
posYPainel = -8

corEscolhida :: Point -> Color
corEscolhida (x, y)
    | y < posYPainel - 1 || y > posYPainel - 0.5 || x < -8.25 || x > 8.25 = black
    | otherwise = pColors !! (floor (x * 2 + 0.5) + 16)

alteraCorFigura :: Int -> Color -> [Figura] -> [Figura]
alteraCorFigura i novaCor cs = 
    let figura = cs !! i
        novaFigura = case figura of
            Circulo c r preenchido _ -> Circulo c r preenchido novaCor
            Retangulo p1 p2 preenchido _ -> Retangulo p1 p2 preenchido novaCor
            DesenhoMaoLivre pts preenchido _ -> DesenhoMaoLivre pts preenchido novaCor
    in atualizaFigura i novaFigura cs

