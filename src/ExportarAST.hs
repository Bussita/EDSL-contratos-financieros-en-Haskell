module ExportarAST where

import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeDirectory)
import System.Process   (readProcess)
import Control.Exception (try, IOException)

import AST
import PrettyPrinter

-- Directorio de salida

outputDir :: FilePath
outputDir = "output"

ensureOutputPath :: FilePath -> IO FilePath
ensureOutputPath path = do
  let dir = takeDirectory fullPath
  createDirectoryIfMissing True dir
  return fullPath
  where fullPath = outputDir ++ "/" ++ path

-- Diagrama de árbol SVG del AST (via Graphviz)

-- Genera un SVG con el árbol del AST usando Graphviz `dot` y lo escribe
-- en el archivo indicado. Si Graphviz no está instalado, guarda el DOT
-- como fallback.
exportarASTSVG :: Contract -> FilePath -> IO String
exportarASTSVG c path = do
  fullPath <- ensureOutputPath path
  let dot = contractToDot c
  result <- try (readProcess "dot" ["-Tsvg"] dot) :: IO (Either IOException String)
  case result of
    Right svg -> do
      writeFile fullPath svg
      return $ "Diagrama AST exportado a " ++ fullPath
    Left _ -> do
      let dotPath = fullPath ++ ".dot"
      writeFile dotPath dot
      return $ "Graphviz no encontrado. DOT guardado en " ++ dotPath

-- Generación de DOT

-- Convierte un Contract en un string DOT completo.
contractToDot :: Contract -> String
contractToDot c =
  let (_, nodes, edges) = buildDot 0 c
  in  unlines
      [ "digraph AST {"
      , "  graph [bgcolor=\"#FAFAFA\" rankdir=TB];"
      , "  node  [shape=box style=\"filled,rounded\" fontname=monospace"
      , "         fontsize=11 fontcolor=white penwidth=1 color=\"#90CAF9\"];"
      , "  edge  [color=\"#90CAF9\" penwidth=1.5 arrowhead=none];"
      , nodes ++ edges
      , "}"
      ]

-- Recorrido recursivo del AST
-- Devuelve (próximoId, declaraciones de nodos, declaraciones de aristas).
-- El nodo raíz de cada llamada siempre tiene id = n.

buildDot :: Int -> Contract -> (Int, String, String)
buildDot n Zero =
  (n+1, mkNode n "zero" "#9E9E9E", "")
buildDot n (One cur) =
  (n+1, mkNode n ("one " ++ ppCurrency cur) "#43A047", "")
buildDot n (Var x) =
  (n+1, mkNode n ("var " ++ x) "#8D6E63", "")
buildDot n (Give c) =
  let (n1, ns, es) = buildDot (n+1) c
  in  (n1, mkNode n "give" "#E53935" ++ ns
      , mkEdge n (n+1) ++ es)
buildDot n (And c1 c2) = buildBin n "and" "#1565C0" c1 c2
buildDot n (Or  c1 c2) = buildBin n "or"  "#6A1B9A" c1 c2
buildDot n (Then c1 c2) = buildBin n "then" "#E65100" c1 c2
buildDot n (Scale o c) =
  let (n1, ns, es) = buildDot (n+1) c
  in  (n1, mkNode2 n "scale" (ppObs o) "#00838F" ++ ns
      , mkEdge n (n+1) ++ es)
buildDot n (Truncate d c) =
  let (n1, ns, es) = buildDot (n+1) c
  in  (n1, mkNode2 n "truncate" (show d) "#558B2F" ++ ns
      , mkEdge n (n+1) ++ es)
buildDot n (If b c1 c2) =
  let (n1, ns1, es1) = buildDot (n+1) c1
      (n2, ns2, es2) = buildDot n1 c2
  in  (n2, mkNode2 n "if" (ppObsBool b) "#AD1457" ++ ns1 ++ ns2
      , mkEdge n (n+1) ++ mkEdge n n1 ++ es1 ++ es2)

-- Helper para nodos binarios simples (and, or, then).
buildBin :: Int -> String -> String -> Contract -> Contract
         -> (Int, String, String)
buildBin n label color c1 c2 =
  let (n1, ns1, es1) = buildDot (n+1) c1
      (n2, ns2, es2) = buildDot n1 c2
  in  (n2, mkNode n label color ++ ns1 ++ ns2
      , mkEdge n (n+1) ++ mkEdge n n1 ++ es1 ++ es2)

-- Primitivas DOT

-- Nodo con una sola línea de texto.
mkNode :: Int -> String -> String -> String
mkNode i label color =
  "  n" ++ show i ++ " [label=\"" ++ escDot label
  ++ "\" fillcolor=\"" ++ color ++ "\"];\n"

-- Nodo con dos líneas (título + detalle en tamaño menor).
mkNode2 :: Int -> String -> String -> String -> String
mkNode2 i title detail color =
  "  n" ++ show i ++ " [label=<" ++ escHtml title
  ++ "<br/><font point-size=\"9\">" ++ escHtml detail
  ++ "</font>> fillcolor=\"" ++ color ++ "\"];\n"

-- Arista dirigida (visualmente sin flecha).
mkEdge :: Int -> Int -> String
mkEdge from to =
  "  n" ++ show from ++ " -> n" ++ show to ++ ";\n"

-- Escape

-- Escapa caracteres para strings DOT entre comillas dobles.
escDot :: String -> String
escDot = concatMap esc
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc c    = [c]

-- Escapa caracteres para HTML labels en DOT.
escHtml :: String -> String
escHtml = concatMap esc
  where
    esc '<'  = "&lt;"
    esc '>'  = "&gt;"
    esc '&'  = "&amp;"
    esc '"'  = "&quot;"
    esc c    = [c]
