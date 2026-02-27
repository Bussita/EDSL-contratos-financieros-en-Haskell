module ExportarAST where

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import AST
import PrettyPrinter

-- Directorio de salida

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
-- Diagrama de árbol SVG del AST
-- ═══════════════════════════════════════════════════════════════════════════════

-- | Genera un SVG con el árbol del AST de un contrato y lo escribe en `path`.
exportarASTSVG :: Contract -> FilePath -> IO String
exportarASTSVG c path = do
  fullPath <- ensureOutputPath path
  let svg = renderASTSVG c
  writeFile fullPath svg
  return $ "Diagrama AST exportado a " ++ fullPath

-- ─── Árbol de layout ──────────────────────────────────────────────────────────

data ASTTree = ASTNode
  { nodeLabel    :: String
  , nodeChildren :: [ASTTree]
  } deriving (Show)

-- | Convierte un Contract en un ASTTree para layout.
contractToTree :: Contract -> ASTTree
contractToTree Zero            = leaf "zero"
contractToTree (One c)         = leaf ("one " ++ ppCurrency c)
contractToTree (Var x)         = leaf ("var " ++ x)
contractToTree (Give c)        = node1 "give" c
contractToTree (And c1 c2)     = node2 "and" c1 c2
contractToTree (Or  c1 c2)     = node2 "or"  c1 c2
contractToTree (Then c1 c2)    = node2 "then" c1 c2
contractToTree (Scale o c)     = ASTNode ("scale\n" ++ ppObs o) [contractToTree c]
contractToTree (Truncate d c)  = ASTNode ("truncate\n" ++ show d) [contractToTree c]
contractToTree (If b c1 c2)    = ASTNode ("if\n" ++ ppObsBool b)
                                   [contractToTree c1, contractToTree c2]

leaf :: String -> ASTTree
leaf l = ASTNode l []

node1 :: String -> Contract -> ASTTree
node1 l c = ASTNode l [contractToTree c]

node2 :: String -> Contract -> Contract -> ASTTree
node2 l c1 c2 = ASTNode l [contractToTree c1, contractToTree c2]

-- ─── Cálculo de posiciones ────────────────────────────────────────────────────

-- Cada nodo tiene una posición (x, y) y un ancho de subárbol.
data PosTree = PosNode
  { ptLabel    :: String
  , ptX        :: Double
  , ptY        :: Double
  , ptChildren :: [PosTree]
  } deriving (Show)

nodeW, nodeH, hGap, vGap :: Double
nodeW = 100   -- ancho de cada nodo
nodeH = 36    -- alto de cada nodo
hGap  = 20    -- espacio horizontal entre nodos hermanos
vGap  = 60    -- espacio vertical entre niveles

-- | Calcula el ancho total que ocupa un subárbol.
treeWidth :: ASTTree -> Double
treeWidth (ASTNode _ []) = nodeW
treeWidth (ASTNode _ cs) =
  let childWidths = map treeWidth cs
      totalChildren = sum childWidths + hGap * fromIntegral (length cs - 1)
  in  max nodeW totalChildren

-- | Asigna posiciones a cada nodo dado el centro X del subárbol y la Y del nivel.
layoutTree :: Double -> Double -> ASTTree -> PosTree
layoutTree cx y (ASTNode label children) =
  let positioned = layoutChildren cx (y + nodeH + vGap) children
  in  PosNode label cx y positioned

layoutChildren :: Double -> Double -> [ASTTree] -> [PosTree]
layoutChildren _ _ [] = []
layoutChildren cx y cs =
  let widths     = map treeWidth cs
      totalW     = sum widths + hGap * fromIntegral (length cs - 1)
      startX     = cx - totalW / 2
      positions  = scanl (\acc w -> acc + w + hGap) startX widths
      pairs      = zip positions (zip widths cs)
  in  [ layoutTree (x + w / 2) y t | (x, (w, t)) <- pairs ]

-- ─── Renderizado SVG ──────────────────────────────────────────────────────────

renderASTSVG :: Contract -> String
renderASTSVG c =
  let tree   = contractToTree c
      total  = treeWidth tree
      depth  = treeDepth tree
      w      = round (total + 60) :: Int
      h      = round (fromIntegral depth * (nodeH + vGap) + 80) :: Int
      cx     = total / 2 + 30
      pos    = layoutTree cx 30 tree
  in  unlines
      [ "<svg xmlns='http://www.w3.org/2000/svg' width='" ++ show w
        ++ "' height='" ++ show h ++ "' font-family='monospace' font-size='11'>"
      , "<rect width='100%' height='100%' fill='#FAFAFA'/>"
      , renderPosTree pos
      , "</svg>"
      ]

treeDepth :: ASTTree -> Int
treeDepth (ASTNode _ []) = 1
treeDepth (ASTNode _ cs) = 1 + maximum (map treeDepth cs)

renderPosTree :: PosTree -> String
renderPosTree pt =
  -- Líneas a los hijos (primero para que queden debajo de los nodos)
  concatMap (renderEdge pt) (ptChildren pt)
  -- El nodo en sí
  ++ renderNode pt
  -- Recursión sobre los hijos
  ++ concatMap renderPosTree (ptChildren pt)

renderEdge :: PosTree -> PosTree -> String
renderEdge parent child =
  let x1 = round (ptX parent) :: Int
      y1 = round (ptY parent + nodeH) :: Int
      x2 = round (ptX child) :: Int
      y2 = round (ptY child) :: Int
  in  "<line x1='" ++ show x1 ++ "' y1='" ++ show y1
      ++ "' x2='" ++ show x2 ++ "' y2='" ++ show y2
      ++ "' stroke='#90CAF9' stroke-width='1.5'/>"

renderNode :: PosTree -> String
renderNode pt =
  let x     = round (ptX pt - nodeW / 2) :: Int
      y     = round (ptY pt) :: Int
      label = ptLabel pt
      -- Si el label tiene salto de línea, dividimos en dos líneas
      (line1, line2) = splitLabel label
      cx    = round (ptX pt) :: Int
      cy1   = round (ptY pt + nodeH / 2 - 5) :: Int
      cy2   = round (ptY pt + nodeH / 2 + 8) :: Int
      isLeaf = null (filter (== '\n') label) && null []
      bgCol = nodeColor label
  in  "<rect x='" ++ show x ++ "' y='" ++ show y
      ++ "' width='" ++ show (round nodeW :: Int)
      ++ "' height='" ++ show (round nodeH :: Int)
      ++ "' rx='6' fill='" ++ bgCol ++ "' stroke='#90CAF9' stroke-width='1'/>"
      ++ "<text x='" ++ show cx ++ "' y='" ++ show cy1
      ++ "' text-anchor='middle' fill='white' font-weight='bold'>"
      ++ escAst line1 ++ "</text>"
      ++ (if null line2 then ""
          else "<text x='" ++ show cx ++ "' y='" ++ show cy2
               ++ "' text-anchor='middle' fill='#E3F2FD' font-size='9'>"
               ++ escAst line2 ++ "</text>")

splitLabel :: String -> (String, String)
splitLabel s = case break (== '\n') s of
  (a, [])    -> (a, "")
  (a, _:b)   -> (a, b)

-- Color del nodo según el tipo de combinador
nodeColor :: String -> String
nodeColor "zero"    = "#9E9E9E"
nodeColor s
  | take 3 s == "one"  = "#43A047"
  | take 3 s == "var"  = "#8D6E63"
  | s == "give"        = "#E53935"
  | s == "and"         = "#1565C0"
  | s == "or"          = "#6A1B9A"
  | s == "then"        = "#E65100"
  | take 5 s == "scale"    = "#00838F"
  | take 8 s == "truncate" = "#558B2F"
  | take 2 s == "if"       = "#AD1457"
  | otherwise              = "#455A64"

escAst :: String -> String
escAst = concatMap esc
  where
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '&' = "&amp;"
    esc c   = [c]
