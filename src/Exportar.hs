module Exportar where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (nub)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Types
import AST
import Monads
import PrettyPrinter

-- ═══════════════════════════════════════════════════════════════════════════════
-- Directorio de salida
-- ═══════════════════════════════════════════════════════════════════════════════

-- Directorio donde se guardan los archivos generados.
outputDir :: FilePath
outputDir = "output"

-- Crea el directorio de salida si no existe y devuelve la ruta completa.
ensureOutputPath :: FilePath -> IO FilePath
ensureOutputPath path = do
  let dir = takeDirectory fullPath
  createDirectoryIfMissing True dir
  return fullPath
  where fullPath = outputDir ++ "/" ++ path

-- ═══════════════════════════════════════════════════════════════════════════════
-- Reporte HTML completo
-- ═══════════════════════════════════════════════════════════════════════════════

-- Genera un reporte HTML autocontenido con contratos, billeteras,
-- historial y gráficos embebidos inline.
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

-- ═══════════════════════════════════════════════════════════════════════════════
-- Sección: Contratos definidos
-- ═══════════════════════════════════════════════════════════════════════════════

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

-- ═══════════════════════════════════════════════════════════════════════════════
-- Sección: Billeteras
-- ═══════════════════════════════════════════════════════════════════════════════

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
              ++ "<td class='" ++ (if v >= 0 then "pos" else "neg") ++ "'>"
              ++ showDouble v ++ "</td></tr>"
            | (cur, v) <- Map.toList bals ]
  ++ "</tbody></table></div>"

-- ═══════════════════════════════════════════════════════════════════════════════
-- Sección: Historial de ejecuciones
-- ═══════════════════════════════════════════════════════════════════════════════

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
  -- Gráfico de barras
  ++ renderBarChart party (hCashflows h)
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

-- ═══════════════════════════════════════════════════════════════════════════════
-- Gráfico de barras HTML+CSS de cashflows
-- ═══════════════════════════════════════════════════════════════════════════════
-- Genera barras horizontales con HTML y CSS puro (sin SVG). Se embebe inline
-- en cada entrada del historial dentro del reporte HTML.

renderBarChart :: PartyId -> [Cashflow] -> String
renderBarChart _party [] = ""
renderBarChart party cfs =
    "<div class='bar-chart'>"
    ++ "<h4>Flujos de caja &mdash; perspectiva de " ++ escHtml party ++ "</h4>"
    ++ concatMap (renderBarra maxAbs) entries
    ++ "<h4 class='neto-titulo'>Neto por moneda</h4>"
    ++ concatMap (renderNetoRow maxAbsN) netRows
    ++ "</div>"
  where
    entries = map (cfToEntry party) cfs
    maxAbs  = maximum (1 : map (abs . fst) entries)
    netMap  = foldl (accumNet party) Map.empty cfs
    netRows = Map.toList netMap
    maxAbsN = maximum (1 : map (abs . snd) netRows)

-- | Una fila del gráfico de barras. Positivos van a la derecha del centro,
--   negativos a la izquierda.
renderBarra :: Double -> (Double, String) -> String
renderBarra maxAbs (val, label) =
    let pct = show (round (abs val / maxAbs * 100) :: Int) ++ "%"
        col = if val > 0 then colorIncoming
              else if val < 0 then colorOutgoing
              else colorNeutral
        barDiv = "<div class='bar' style='width:" ++ pct
                 ++ ";background:" ++ col ++ ";'></div>"
    in  "<div class='bar-row'>"
        ++ "<div class='bar-track'>"
        ++ "<div class='bar-half bar-half-left'>"
        ++ (if val < 0 then barDiv else "") ++ "</div>"
        ++ "<div class='bar-half'>"
        ++ (if val >= 0 then barDiv else "") ++ "</div>"
        ++ "</div>"
        ++ "<span class='bar-label'>" ++ escHtml label ++ "</span>"
        ++ "</div>"

-- | Una fila del resumen neto por moneda.
renderNetoRow :: Double -> (Currency, Double) -> String
renderNetoRow maxAbsN (cur, net) =
    let pct  = show (round (abs net / maxAbsN * 100) :: Int) ++ "%"
        col  = colorCurrency cur
        sign = if net >= 0 then "+" else ""
        cls  = if net >= 0 then "pos" else "neg"
    in  "<div class='neto-row'>"
        ++ "<span class='neto-cur' style='color:" ++ col ++ "'>"
        ++ ppCurrency cur ++ "</span>"
        ++ "<div class='neto-bar' style='width:" ++ pct
        ++ ";background:" ++ col ++ ";'></div>"
        ++ "<span class='" ++ cls ++ "'>"
        ++ sign ++ showDouble net ++ "</span>"
        ++ "</div>"

-- ─── Helpers de barras ────────────────────────────────────────────────────────

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

colorCurrency :: Currency -> String
colorCurrency USD = "#2196F3"
colorCurrency EUR = "#FF9800"
colorCurrency ARS = "#9C27B0"
colorCurrency GBP = "#009688"

colorIncoming, colorOutgoing, colorNeutral :: String
colorIncoming = "#43A047"
colorOutgoing  = "#E53935"
colorNeutral   = "#BDBDBD"

-- ═══════════════════════════════════════════════════════════════════════════════
-- Sección: Evolución temporal de billeteras (tabla)
-- ═══════════════════════════════════════════════════════════════════════════════
-- Genera una tabla HTML por moneda mostrando la evolución de los saldos de
-- cada parte a lo largo de los snapshots registrados, más una tabla de eventos.

seccionEvolucion :: [WalletSnapshot] -> String
seccionEvolucion [] =
  seccion "Evolución Temporal" "<p class='empty'>Sin snapshots registrados</p>"
seccionEvolucion snaps =
  seccion "Evolución Temporal" $
    tablaEventos snaps
    ++ concatMap (tablaMoneda snaps) monedas
  where
    monedas = nub
      [ cur
      | s <- snaps
      , (_party, bals) <- Map.toList (snapWallets s)
      , (cur, _) <- Map.toList bals
      ]

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

-- ─── Tabla de saldos por moneda ──────────────────────────────────────────────

tablaMoneda :: [WalletSnapshot] -> Currency -> String
tablaMoneda snaps cur =
  let partes = nub
        [ party
        | s <- snaps
        , (party, bals) <- Map.toList (snapWallets s)
        , Map.member cur bals
        ]
      titulo = "<h3 style='margin-top:1.5rem;color:#1565C0'>Evolución — "
               ++ ppCurrency cur ++ "</h3>"
      header = "<table><thead><tr><th>Fecha</th>"
               ++ concatMap (\p -> "<th>" ++ escHtml p ++ "</th>") partes
               ++ "</tr></thead><tbody>"
      filas  = concatMap (\s ->
                 "<tr><td>" ++ show (snapFecha s) ++ "</td>"
                 ++ concatMap (\p ->
                      let v = walletBalanceSnap s p cur
                          cls | v > 0    = "pos"
                              | v < 0    = "neg"
                              | otherwise = ""
                      in  "<td class='" ++ cls ++ "'>" ++ show v ++ "</td>"
                    ) partes
                 ++ "</tr>"
               ) snaps
  in  titulo ++ header ++ filas ++ "</tbody></table>"

walletBalanceSnap :: WalletSnapshot -> PartyId -> Currency -> Double
walletBalanceSnap snap party cur =
  case Map.lookup party (snapWallets snap) of
    Nothing  -> 0
    Just bal -> Map.findWithDefault 0 cur bal

-- ═══════════════════════════════════════════════════════════════════════════════
-- Helpers HTML
-- ═══════════════════════════════════════════════════════════════════════════════

seccion :: String -> String -> String
seccion titulo contenido =
  "<section><h2>" ++ escHtml titulo ++ "</h2>" ++ contenido ++ "</section>"

htmlTitulo :: String -> String
htmlTitulo t = "<h1>" ++ escHtml t ++ "</h1>"

htmlSubtitulo :: String -> String
htmlSubtitulo t = "<p class='subtitulo'>" ++ escHtml t ++ "</p>"

-- | Escapa caracteres especiales para HTML.
escHtml :: String -> String
escHtml = concatMap esc
  where
    esc '<'  = "&lt;"
    esc '>'  = "&gt;"
    esc '&'  = "&amp;"
    esc '"'  = "&quot;"
    esc '\'' = "&#39;"
    esc c    = [c]

-- ═══════════════════════════════════════════════════════════════════════════════
-- CSS del reporte
-- ═══════════════════════════════════════════════════════════════════════════════

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
  , "  .bar-chart { margin-top: 1rem; padding: 0.75rem; background: #FAFAFA;"
  , "               border-radius: 6px; }"
  , "  .bar-chart h4 { font-size: 0.9rem; color: #424242; margin-bottom: 0.5rem; }"
  , "  .neto-titulo { margin-top: 1rem; padding-top: 0.75rem;"
  , "                 border-top: 1px solid #E0E0E0; }"
  , "  .bar-row { display: flex; align-items: center; height: 26px; gap: 8px; }"
  , "  .bar-track { flex: 1; display: flex; height: 18px; }"
  , "  .bar-half { width: 50%; display: flex; overflow: hidden; }"
  , "  .bar-half-left { justify-content: flex-end; }"
  , "  .bar { height: 100%; border-radius: 2px; min-width: 2px; }"
  , "  .bar-label { font-size: 0.8rem; color: #424242; white-space: nowrap;"
  , "               min-width: 140px; }"
  , "  .neto-row { display: flex; align-items: center; height: 30px; gap: 8px; }"
  , "  .neto-cur { font-weight: 600; font-size: 0.85rem; min-width: 40px; }"
  , "  .neto-bar { height: 16px; border-radius: 2px; min-width: 2px; }"
  , "</style></head>"
  ]
