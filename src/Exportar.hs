module Exportar where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (intercalate, nub, sortBy)
import Data.Ord (comparing)
import Data.Time (Day, diffDays)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Types
import AST
import Monads
import PrettyPrinter

-- Directorio de salida

-- Directorio donde se guardan los archivos generados.
outputDir :: FilePath
outputDir = "output"

-- Crea el directorio de salida si no existe y devuelve la ruta completa.
ensureOutputPath :: FilePath -> IO FilePath
ensureOutputPath path = do
  let dir = takeDirectory fullPath -- Se queda con el directorio
  createDirectoryIfMissing True dir
  return fullPath
  where fullPath = outputDir ++ "/" ++ path


-- Reporte HTML completo

-- Genera un reporte HTML autocontenido con contratos, billeteras,
-- historial y gráficos SVG embebidos inline.
exportarHTML :: InterpState -> Env -> FilePath -> IO String
exportarHTML ist env path = do
  fullPath <- ensureOutputPath path
  let contenido = renderReporte ist env
  writeFile fullPath contenido
  return $ "Reporte exportado a " ++ fullPath

renderReporte :: InterpState -> Env -> String
renderReporte ist env = unlines
  [ htmlHeader
  , "<body>"
  , "<div class='container'>"
  , htmlTitulo "Reporte de Sesión EDSL Financiero"
  , htmlSubtitulo $ "Fecha: " ++ show (fechaHoy env)
                 ++ "  |  Partes: " ++ yo env ++ " / " ++ contraparte env
  , seccionContratos (isContracts ist)
  , seccionBilleteras (isWallets ist)
  , seccionHistorial (yo env) (isHistorial ist)
  , seccionEvolucion (isSnapshots ist)
  , "</div></body></html>"
  ]

-- Secciones

seccionContratos :: ContractStore -> String
seccionContratos cs
  | Map.null cs = seccion "Contratos Definidos" "<p class='empty'>Ninguno</p>"
  | otherwise   =
      seccion "Contratos Definidos" $
        "<table><thead><tr><th>Nombre</th><th>Definición</th></tr></thead><tbody>"
        ++ concatMap contratoFila (Map.toList cs)
        ++ "</tbody></table>"

contratoFila :: (String, Contract) -> String
contratoFila (name, c) =
  "<tr><td><code>" ++ escHtml name ++ "</code></td>"
  ++ "<td><code>" ++ escHtml (ppContract c) ++ "</code></td></tr>"

seccionBilleteras :: Wallets -> String
seccionBilleteras ws
  | Map.null ws = seccion "Billeteras" "<p class='empty'>Sin billeteras registradas</p>"
  | otherwise   = seccion "Billeteras" $ concatMap walletCard (Map.toList ws)

walletCard :: (PartyId, Map.Map Currency Double) -> String
walletCard (party, bals) =
  "<div class='wallet-card'>"
  ++ "<h3>" ++ escHtml party ++ "</h3>"
  ++ "<table><thead><tr><th>Moneda</th><th>Saldo</th></tr></thead><tbody>"
  ++ concat [ "<tr><td>" ++ ppCurrency cur ++ "</td>"
              ++ "<td class='" ++ (if v >= 0 then "pos" else "neg") ++ "'>" -- rojo o verde dependiendo el saldo
              ++ showDouble v ++ "</td></tr>" -- de todas formas las billeteras no debitan y dejan en negativo
            | (cur, v) <- Map.toList bals ]
  ++ "</tbody></table></div>"

seccionHistorial :: PartyId -> [HistorialEntry] -> String
seccionHistorial _ [] =
  seccion "Historial de Ejecuciones" "<p class='empty'>Sin ejecuciones registradas</p>"
seccionHistorial party hs =
  seccion "Historial de Ejecuciones" $ concatMap (entryCard party) hs

entryCard :: PartyId -> HistorialEntry -> String
entryCard party h =
  "<div class='entry-card'>"
  ++ "<div class='entry-header'>"
  ++ "<span class='entry-name'>" ++ escHtml (hNombre h) ++ "</span>"
  ++ "<span class='entry-date'>" ++ show (hFecha h) ++ "</span>"
  ++ "<span class='entry-parties'>"
  ++ escHtml (hPartyA h) ++ " / " ++ escHtml (hPartyB h)
  ++ "</span></div>"
  -- Tabla de cashflows
  ++ tablaCashflows (hCashflows h)
  -- Gráfico SVG embebido inline
  ++ "<div class='svg-container'>"
  ++ renderSVG party (hCashflows h)
  ++ "</div>"
  ++ "</div>"

tablaCashflows :: [Cashflow] -> String
tablaCashflows [] = "<p class='empty'>Sin cashflows</p>"
tablaCashflows cfs =
  "<table><thead><tr>"
  ++ "<th>Fecha</th><th>Monto</th><th>Moneda</th><th>Desde</th><th>Hacia</th>"
  ++ "</tr></thead><tbody>"
  ++ concat [ "<tr>"
              ++ "<td>" ++ show (fecha cf) ++ "</td>"
              ++ "<td>" ++ showDouble (value (cantidad cf)) ++ "</td>"
              ++ "<td>" ++ ppCurrency (currency (cantidad cf)) ++ "</td>"
              ++ "<td>" ++ escHtml (desde cf) ++ "</td>"
              ++ "<td>" ++ escHtml (hacia cf) ++ "</td>"
              ++ "</tr>"
            | cf <- cfs ]
  ++ "</tbody></table>"

-- ─── Helpers HTML ─────────────────────────────────────────────────────────────

seccion :: String -> String -> String
seccion titulo contenido =
  "<section><h2>" ++ escHtml titulo ++ "</h2>" ++ contenido ++ "</section>"

htmlTitulo :: String -> String
htmlTitulo t = "<h1>" ++ escHtml t ++ "</h1>"

htmlSubtitulo :: String -> String
htmlSubtitulo t = "<p class='subtitulo'>" ++ escHtml t ++ "</p>"

-- sirve para evitar caracteres especiales en html
escHtml :: String -> String
escHtml = concatMap esc
  where
    esc '<'  = "&lt;"
    esc '>'  = "&gt;"
    esc '&'  = "&amp;"
    esc '"'  = "&quot;"
    esc '\'' = "&#39;"
    esc c    = [c]

htmlHeader :: String
htmlHeader = unlines
  [ "<!DOCTYPE html>"
  , "<html lang='es'><head>"
  , "<meta charset='UTF-8'>"
  , "<meta name='viewport' content='width=device-width, initial-scale=1.0'>"
  , "<title>Reporte EDSL Financiero</title>"
  , "<style>"
  , "  * { box-sizing: border-box; margin: 0; padding: 0; }"
  , "  body { font-family: 'Segoe UI', Arial, sans-serif; background: #f4f6f8;"
  , "         color: #212121; line-height: 1.5; }"
  , "  .container { max-width: 1000px; margin: 0 auto; padding: 2rem; }"
  , "  h1 { font-size: 1.8rem; color: #1565C0; margin-bottom: 0.25rem; }"
  , "  h2 { font-size: 1.2rem; color: #424242; border-bottom: 2px solid #1565C0;"
  , "       padding-bottom: 0.3rem; margin-bottom: 1rem; }"
  , "  h3 { font-size: 1rem; color: #1565C0; margin-bottom: 0.5rem; }"
  , "  .subtitulo { color: #757575; font-size: 0.9rem; margin-bottom: 2rem; }"
  , "  section { background: white; border-radius: 8px; padding: 1.5rem;"
  , "            margin-bottom: 1.5rem; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }"
  , "  table { width: 100%; border-collapse: collapse; font-size: 0.9rem; }"
  , "  th { background: #E3F2FD; color: #1565C0; padding: 0.5rem 0.75rem;"
  , "       text-align: left; font-weight: 600; }"
  , "  td { padding: 0.4rem 0.75rem; border-bottom: 1px solid #F0F0F0; }"
  , "  tr:last-child td { border-bottom: none; }"
  , "  tr:hover td { background: #FAFAFA; }"
  , "  code { font-family: 'Courier New', monospace; font-size: 0.85rem;"
  , "         background: #F5F5F5; padding: 0.1rem 0.3rem; border-radius: 3px; }"
  , "  .pos { color: #2E7D32; font-weight: 600; }"
  , "  .neg { color: #C62828; font-weight: 600; }"
  , "  .empty { color: #9E9E9E; font-style: italic; padding: 0.5rem 0; }"
  , "  .wallet-card { display: inline-block; vertical-align: top;"
  , "                 margin: 0 1rem 1rem 0; min-width: 180px; }"
  , "  .entry-card { border: 1px solid #E0E0E0; border-radius: 6px;"
  , "                padding: 1rem; margin-bottom: 1rem; }"
  , "  .entry-header { display: flex; gap: 1rem; align-items: center;"
  , "                  margin-bottom: 0.75rem; flex-wrap: wrap; }"
  , "  .entry-name { font-weight: 700; font-size: 1rem; color: #1565C0; }"
  , "  .entry-date { color: #757575; font-size: 0.85rem; }"
  , "  .entry-parties { color: #424242; font-size: 0.85rem;"
  , "                   background: #E3F2FD; padding: 0.1rem 0.5rem;"
  , "                   border-radius: 12px; }"
  , "  .svg-container { margin-top: 1rem; overflow-x: auto; }"
  , "</style></head>"
  ]

-- ═══════════════════════════════════════════════════════════════════════════════
-- Evolución temporal de billeteras
-- ═══════════════════════════════════════════════════════════════════════════════
-- Genera un gráfico SVG por moneda mostrando la evolución de los saldos de
-- cada parte a lo largo de los snapshots registrados, más una tabla de eventos.

seccionEvolucion :: [WalletSnapshot] -> String
seccionEvolucion [] =
  seccion "Evolución Temporal" "<p class='empty'>Sin snapshots registrados</p>"
seccionEvolucion snaps =
  seccion "Evolución Temporal" $
    tablaEventos snaps
    ++ concatMap (chartMoneda snaps) monedas
  where
    monedas = nub
      [ cur
      | s <- snaps
      , (_party, bals) <- Map.toList (snapWallets s)
      , (cur, _) <- Map.toList bals
      ]

-- Tabla de eventos

tablaEventos :: [WalletSnapshot] -> String
tablaEventos snaps =
  "<h3 style='margin-top:1rem;color:#424242'>Registro de eventos</h3>"
  ++ "<table><thead><tr>"
  ++ "<th>Fecha</th><th>Evento</th>"
  ++ "</tr></thead><tbody>"
  ++ concatMap eventoFila snaps
  ++ "</tbody></table>"

eventoFila :: WalletSnapshot -> String
eventoFila s =
  "<tr><td>" ++ show (snapFecha s) ++ "</td>"
  ++ "<td>" ++ escHtml (snapEvento s) ++ "</td></tr>"

-- ─── Gráfico SVG por moneda ──────────────────────────────────────────────────
-- Un SVG independiente para cada moneda, con una línea por parte.

-- Dimensiones del gráfico de evolución
evWidth, evHeight, evPadL, evPadR, evPadT, evPadB :: Int
evWidth  = 900
evHeight = 300
evPadL   = 80   -- espacio para etiquetas Y
evPadR   = 20
evPadT   = 40   -- espacio superior (para leyenda)
evPadB   = 50   -- espacio para etiquetas X (fechas)

evChartW, evChartH :: Int
evChartW = evWidth  - evPadL - evPadR
evChartH = evHeight - evPadT - evPadB

-- Colores para las líneas (rotamos entre varios)
evLineColors :: [String]
evLineColors = ["#1565C0","#E53935","#43A047","#FF9800","#9C27B0","#00838F","#E65100"]

evColorForIdx :: Int -> String
evColorForIdx i = evLineColors !! (i `mod` length evLineColors)

chartMoneda :: [WalletSnapshot] -> Currency -> String
chartMoneda snaps cur =
  let -- Partes que tienen esta moneda en algún snapshot
      partes = nub
        [ party
        | s <- snaps
        , (party, bals) <- Map.toList (snapWallets s)
        , Map.member cur bals
        ]

      -- Series de datos: para cada parte, lista de (fecha, saldo)
      series =
        [ (party, [ (snapFecha s, walletBalanceSnap s party cur) | s <- snaps ])
        | party <- partes
        ]

      -- Rangos globales
      allDates  = map snapFecha snaps
      minD      = minimum allDates
      maxD      = maximum allDates
      allVals   = concatMap (map snd . snd) series
      rawMinV   = minimum (0 : allVals)
      rawMaxV   = maximum (0 : allVals)
      -- Garantizar un rango no nulo en Y
      (minV, maxV) =
        if rawMinV == rawMaxV
          then (rawMinV - max 1 (abs rawMinV * 0.1), rawMaxV + max 1 (abs rawMaxV * 0.1))
          else (rawMinV, rawMaxV)
      rangeD    = max 1 (toInteger (diffDays maxD minD))
      rangeV    = maxV - minV

      -- Funciones de coordenadas (compartidas por todas las líneas)
      toX d = evPadL + round (fromIntegral evChartW
                * fromIntegral (toInteger (diffDays d minD))
                / fromIntegral rangeD :: Double)
      toY v = evPadT + evChartH - round (fromIntegral evChartH
                * (v - minV) / rangeV :: Double)

      -- Fechas únicas para el eje X
      uniqueDates = nub allDates

      -- Título HTML
      titulo = "<h3 style='margin-top:1.5rem;color:#1565C0'>Evolución — "
               ++ ppCurrency cur ++ "</h3>"

      -- Apertura del SVG
      svgOpen = "<svg xmlns='http://www.w3.org/2000/svg' width='" ++ show evWidth
                ++ "' height='" ++ show evHeight
                ++ "' font-family='monospace' font-size='11'"
                ++ " style='margin-top:0.5rem'>"
                ++ "<rect width='100%' height='100%' fill='#FAFAFA' rx='4'/>"

      -- Grilla Y (fondo, se dibuja primero)
      gridY = evRenderGridY minV maxV rangeV

      -- Línea de referencia Y=0
      zeroLine
        | minV < 0 && maxV > 0 =
            let y0 = toY 0
            in  "<line x1='" ++ show evPadL ++ "' y1='" ++ show y0
                ++ "' x2='" ++ show (evPadL + evChartW) ++ "' y2='" ++ show y0
                ++ "' stroke='#BDBDBD' stroke-dasharray='4'/>"
        | otherwise = ""

      -- Líneas de datos
      dataLines = concatMap
        (\(i, (party, pts)) ->
          evRenderSeries (evColorForIdx i) pts toX toY)
        (zip [0..] series)

      -- Eje X (una sola vez, con fechas únicas)
      axisX = evRenderAxisX uniqueDates toX

      -- Leyenda apilada verticalmente
      legend = evRenderLegend (zip [0..] (map fst series))

  in  titulo
      ++ svgOpen
      ++ gridY
      ++ zeroLine
      ++ dataLines
      ++ axisX
      ++ legend
      ++ "</svg>"

-- ─── Helpers del gráfico de evolución ─────────────────────────────────────────

walletBalanceSnap :: WalletSnapshot -> PartyId -> Currency -> Double
walletBalanceSnap snap party cur =
  case Map.lookup party (snapWallets snap) of
    Nothing  -> 0
    Just bal -> Map.findWithDefault 0 cur bal

-- | Renderiza una serie como polilínea con puntos.
--   Soporta un solo punto (dibuja solo un círculo).
evRenderSeries :: String -> [(Day, Double)]
               -> (Day -> Int) -> (Double -> Int) -> String
evRenderSeries _   []  _   _   = ""
evRenderSeries col [p] toX toY =
  let (d, v) = p; x = toX d; y = toY v
  in  "<circle cx='" ++ show x ++ "' cy='" ++ show y
      ++ "' r='5' fill='" ++ col ++ "' stroke='white' stroke-width='1.5'/>"
evRenderSeries col pts toX toY =
  let screenPts = [ (toX d, toY v) | (d, v) <- pts ]
      polyPts   = concatMap (\(x,y) -> show x ++ "," ++ show y ++ " ") screenPts
      dots      = concatMap (\(x,y) ->
                    "<circle cx='" ++ show x ++ "' cy='" ++ show y
                    ++ "' r='4' fill='" ++ col
                    ++ "' stroke='white' stroke-width='1'/>"
                  ) screenPts
  in  "<polyline points='" ++ polyPts ++ "'"
      ++ " fill='none' stroke='" ++ col ++ "' stroke-width='2'/>"
      ++ dots

-- | Grilla horizontal y etiquetas del eje Y (renderizada una sola vez).
evRenderGridY :: Double -> Double -> Double -> String
evRenderGridY minV _maxV rangeV =
  let steps = 4 :: Int
      step  = rangeV / fromIntegral steps
      vals  = [ minV + fromIntegral i * step | i <- [0..steps] ]
      toY v = evPadT + evChartH - round (fromIntegral evChartH
                * (v - minV) / rangeV :: Double)
  in  concatMap (\v ->
        let y = toY v
        in  "<text x='" ++ show (evPadL - 5) ++ "' y='" ++ show (y + 4)
            ++ "' text-anchor='end' font-size='9' fill='#757575'>"
            ++ show (round v :: Integer) ++ "</text>"
            ++ "<line x1='" ++ show evPadL ++ "' y1='" ++ show y
            ++ "' x2='" ++ show (evPadL + evChartW) ++ "' y2='" ++ show y
            ++ "' stroke='#F0F0F0'/>"
      ) vals

-- | Etiquetas de fechas en el eje X (renderizada una sola vez, sin duplicados).
evRenderAxisX :: [Day] -> (Day -> Int) -> String
evRenderAxisX dates toX =
  concatMap (\d ->
    let x = toX d
    in  "<text x='" ++ show x ++ "' y='" ++ show (evHeight - 10)
        ++ "' text-anchor='middle' font-size='9' fill='#757575'>"
        ++ show d ++ "</text>"
        ++ "<line x1='" ++ show x ++ "' y1='" ++ show (evPadT + evChartH)
        ++ "' x2='" ++ show x ++ "' y2='" ++ show (evPadT + evChartH + 4)
        ++ "' stroke='#BDBDBD'/>"
  ) dates

-- | Leyenda con entradas apiladas verticalmente (sin solapamiento).
evRenderLegend :: [(Int, String)] -> String
evRenderLegend entries =
  concatMap (\(i, (idx, label)) ->
    let y   = evPadT + 6 + i * 18
        col = evColorForIdx idx
    in  "<rect x='" ++ show (evPadL + evChartW - 150) ++ "' y='" ++ show y
        ++ "' width='12' height='12' fill='" ++ col ++ "' rx='2'/>"
        ++ "<text x='" ++ show (evPadL + evChartW - 134) ++ "' y='" ++ show (y + 11)
        ++ "' font-size='11' fill='#424242'>" ++ escHtml label ++ "</text>"
  ) (zip [0..] entries)

-- ═══════════════════════════════════════════════════════════════════════════════
-- Gráfico SVG de cashflows (barras)
-- ═══════════════════════════════════════════════════════════════════════════════
-- Genera un SVG con tabla de cashflows, gráfico de barras por parte, y neto.
-- Se embebe inline en cada entrada del historial dentro del reporte HTML.

renderSVG :: PartyId -> [Cashflow] -> String
renderSVG _party [] = ""
renderSVG party cfs =
    let sorted  = sortBy (comparing fecha) cfs
        tablaH  = svgTablaHeight sorted -- altura tabla de cashflows
        barrasH = svgBarrasHeight cfs -- altura grafico de barras
        netoH   = svgNetoHeight cfs -- altura seccion "neto"
        padding = 40
        totalH  = 60 + tablaH + padding + barrasH + padding + netoH + 40 -- titulo (perspectiva de) y sumar todo con paddings
        yTabla  = 60 -- pos y donde empieza cada seccion
        yBarras = yTabla  + tablaH  + padding
        yNeto   = yBarras + barrasH + padding
    in  unlines
            [ svgBarHeader svgBarWidth totalH -- aca se abre la etiqueta <svg>
            , svgBarDefs
            , svgBarTitulo party
            , svgRenderTabla sorted yTabla
            , svgRenderBarras party cfs yBarras
            , svgRenderNeto party cfs yNeto
            , "</svg>"
            ]

svgBarWidth :: Int
svgBarWidth = 860

colorCurrency :: Currency -> String
colorCurrency USD = "#2196F3"
colorCurrency EUR = "#FF9800"
colorCurrency ARS = "#9C27B0"
colorCurrency GBP = "#009688"

colorIncoming, colorOutgoing, colorNeutral :: String
colorIncoming = "#43A047"
colorOutgoing  = "#E53935"
colorNeutral   = "#BDBDBD"

-- Dado ancho y alto arma el header para empezar a escribir código SVG en HTML.
svgBarHeader :: Int -> Int -> String
svgBarHeader w h =
    "<svg xmlns=\"http://www.w3.org/2000/svg\" "
    ++ "width=\"" ++ show w ++ "\" height=\"" ++ show h ++ "\" "
    ++ "font-family=\"monospace\" font-size=\"13\">"

-- definiciones CSS internas del SVG
svgBarDefs :: String
svgBarDefs = unlines
    [ "<defs><style>"
    , "  text { font-family: 'Courier New', monospace; }"
    , "  .titulo  { font-size: 16px; font-weight: bold; fill: #212121; }"
    , "  .seccion { font-size: 14px; font-weight: bold; fill: #424242; }"
    , "  .label   { font-size: 12px; fill: #616161; }"
    , "  .valor   { font-size: 12px; fill: #212121; }"
    , "  .neto-pos { font-size: 13px; font-weight: bold; fill: #43A047; }"
    , "  .neto-neg { font-size: 13px; font-weight: bold; fill: #E53935; }"
    , "</style></defs>"
    , "<rect width=\"100%\" height=\"100%\" fill=\"#FAFAFA\"/>"
    ]

svgBarTitulo :: PartyId -> String
svgBarTitulo party =
    svgBarText 30 35 "titulo" ("Flujos de caja — perspectiva de " ++ party)

-- ─── Tabla SVG ────────────────────────────────────────────────────────────────

svgRowH :: Int
svgRowH = 22 -- Altura en pixeles de cada fila

svgTablaHeight :: [Cashflow] -> Int -- Altura total de la tabla
svgTablaHeight cfs = 30 + (length cfs + 1) * svgRowH + 10 -- altura de titulo + altura de cada entrada de cf + padding

svgRenderTabla :: [Cashflow] -> Int -> String
svgRenderTabla cfs y0 = unlines $
    [ svgBarText 30 y0 "seccion" "Cashflows"
    , svgBarLine 30 (y0+8) (svgBarWidth-30) (y0+8) "#BDBDBD"
    , svgBarText 30  (y0+28) "label" "Fecha"
    , svgBarText 170 (y0+28) "label" "Monto"
    , svgBarText 360 (y0+28) "label" "Desde"
    , svgBarText 520 (y0+28) "label" "Hacia"
    , svgBarLine 30 (y0+32) (svgBarWidth-30) (y0+32) "#E0E0E0"
    ] ++ zipWith (svgRenderTablaRow (y0+30)) [1..] cfs

svgRenderTablaRow :: Int -> Int -> Cashflow -> String
svgRenderTablaRow y0 i cf =
    let yRow = y0 + i * svgRowH
        bgCol = if even i then "#F5F5F5" else "#FAFAFA"
        col   = colorCurrency (currency (cantidad cf))
        monto = showDouble (value (cantidad cf)) ++ " "
                ++ ppCurrency (currency (cantidad cf))
    in  unlines
            [ svgBarRect 28 (yRow-14) (svgBarWidth-56) (svgRowH-2) bgCol "none"
            , svgBarText     30  yRow "valor" (ppDate (fecha cf))
            , svgBarTextCol 170  yRow col monto
            , svgBarText    360  yRow "valor" (desde cf)
            , svgBarText    440  yRow "label" "→"
            , svgBarText    520  yRow "valor" (hacia cf)
            ]

-- ─── Barras SVG ───────────────────────────────────────────────────────────────

svgBarRowH :: Int -- Altura de cada fila del grafico de barras
svgBarRowH = 28

svgBarrasHeight :: [Cashflow] -> Int -- altura total del grafico de barras
svgBarrasHeight cfs = 30 + length cfs * svgBarRowH + 20

svgBarMaxW :: Double -- Altura maxima para cada barra
svgBarMaxW = 400

cfToEntry :: PartyId -> Cashflow -> (Double, String)
cfToEntry party cf
    | hacia cf == party =
        (v, "+" ++ showDouble v ++ " " ++ ppCurrency cur)
    | desde cf == party =
        (negate v, "-" ++ showDouble v ++ " " ++ ppCurrency cur)
    | otherwise = (0, "0 " ++ ppCurrency cur)
  where
    v   = value (cantidad cf)
    cur = currency (cantidad cf)

accumNet :: PartyId -> Map Currency Double -> Cashflow -> Map Currency Double
accumNet party acc cf =
    let cur   = currency (cantidad cf)
        v     = value (cantidad cf)
        delta = if hacia cf == party then v
                else if desde cf == party then negate v
                else 0
    in  Map.insertWith (+) cur delta acc

svgRenderBarras :: PartyId -> [Cashflow] -> Int -> String
svgRenderBarras party cfs y0 =
    let entries = map (cfToEntry party) cfs
        maxAbs  = maximum (1 : map (abs . fst) entries)
        sc v = round (abs v / maxAbs * svgBarMaxW) :: Int
    in  unlines $
            [ svgBarText 30 y0 "seccion" ("Gráfico de barras — perspectiva de " ++ party)
            , svgBarLine 30 (y0+8) (svgBarWidth-30) (y0+8) "#BDBDBD"
            , svgBarLine 230 (y0+15) 230 (y0+15+length cfs*svgBarRowH) "#9E9E9E"
            ]
            ++ zipWith (svgRenderBarra sc (y0+15)) [0..] (zip entries cfs)

svgRenderBarra :: (Double -> Int) -> Int -> Int -> ((Double, String), Cashflow) -> String
svgRenderBarra sc y0 i ((val, label), _cf) =
    let yRow   = y0 + i * svgBarRowH
        yMid   = yRow + svgBarRowH `div` 2
        barW   = sc val
        col    = if val > 0 then colorIncoming
                 else if val < 0 then colorOutgoing
                 else colorNeutral
        barX   = if val >= 0 then 230 else 230 - barW
        anchor = if val >= 0 then "start" else "end"
        labelX = if val >= 0 then 225 else 235
    in  unlines
            [ svgBarRect barX (yRow+4) barW (svgBarRowH-8) col col
            , svgBarTextAnchor labelX yMid "valor" anchor label
            ]

-- ─── Neto SVG ─────────────────────────────────────────────────────────────────

svgNetoRowH :: Int
svgNetoRowH = 32

svgNetoHeight :: [Cashflow] -> Int
svgNetoHeight cfs = 30 + length (nub (map (currency . cantidad) cfs)) * svgNetoRowH + 20

svgNetoBarMax :: Double
svgNetoBarMax = 350

svgRenderNeto :: PartyId -> [Cashflow] -> Int -> String
svgRenderNeto party cfs y0 =
    let netMap  = foldl (accumNet party) Map.empty cfs
        rows    = Map.toList netMap
        maxAbsN = maximum (1 : map (abs . snd) rows)
        scaleN v = round (abs v / maxAbsN * svgNetoBarMax) :: Int
    in  unlines $
            [ svgBarText 30 y0 "seccion" "Neto por moneda"
            , svgBarLine 30 (y0+8) (svgBarWidth-30) (y0+8) "#BDBDBD"
            ]
            ++ zipWith (svgRenderNetoRow scaleN (y0+20)) [0..] rows

svgRenderNetoRow :: (Double -> Int) -> Int -> Int -> (Currency, Double) -> String
svgRenderNetoRow scaleN y0 i (cur, net) =
    let yRow  = y0 + i * svgNetoRowH
        yMid  = yRow + svgNetoRowH `div` 2
        barW  = scaleN net
        col   = colorCurrency cur
        sign  = if net >= 0 then "+" else ""
        cls   = if net >= 0 then "neto-pos" else "neto-neg"
    in  unlines
            [ svgBarTextCol 30 yMid col (ppCurrency cur)
            , svgBarRect 80 (yRow+6) barW (svgNetoRowH-12) col col
            , svgBarText (80+barW+8) yMid cls (sign ++ showDouble net)
            ]

-- ─── Primitivas SVG (barras) ──────────────────────────────────────────────────

svgBarText :: Int -> Int -> String -> String -> String
svgBarText x y clase content =
    "<text x=\"" ++ show x ++ "\" y=\"" ++ show y
    ++ "\" class=\"" ++ clase ++ "\">" ++ escapeXML content ++ "</text>"

svgBarTextCol :: Int -> Int -> String -> String -> String
svgBarTextCol x y col content =
    "<text x=\"" ++ show x ++ "\" y=\"" ++ show y
    ++ "\" fill=\"" ++ col ++ "\" class=\"valor\">"
    ++ escapeXML content ++ "</text>"

svgBarTextAnchor :: Int -> Int -> String -> String -> String -> String
svgBarTextAnchor x y cls anchor content =
    "<text x=\"" ++ show x ++ "\" y=\"" ++ show y
    ++ "\" class=\"" ++ cls ++ "\" text-anchor=\"" ++ anchor ++ "\">"
    ++ escapeXML content ++ "</text>"

svgBarRect :: Int -> Int -> Int -> Int -> String -> String -> String
svgBarRect x y w h fill stroke =
    "<rect x=\"" ++ show x ++ "\" y=\"" ++ show y
    ++ "\" width=\"" ++ show (max 1 w) ++ "\" height=\"" ++ show h
    ++ "\" fill=\"" ++ fill ++ "\" stroke=\"" ++ stroke ++ "\"/>"

svgBarLine :: Int -> Int -> Int -> Int -> String -> String
svgBarLine x1 y1 x2 y2 col =
    "<line x1=\"" ++ show x1 ++ "\" y1=\"" ++ show y1
    ++ "\" x2=\"" ++ show x2 ++ "\" y2=\"" ++ show y2
    ++ "\" stroke=\"" ++ col ++ "\" stroke-width=\"1\"/>"

-- cambios de caracteres de escape para XML
escapeXML :: String -> String
escapeXML = concatMap escape
  where
    escape '<'  = "&lt;"
    escape '>'  = "&gt;"
    escape '&'  = "&amp;"
    escape '"'  = "&quot;"
    escape '\'' = "&apos;"
    escape c    = [c]