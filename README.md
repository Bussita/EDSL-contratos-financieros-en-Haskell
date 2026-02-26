# Contratos Financieros — EDSL en Haskell

Implementación de un lenguaje de dominio específico embebido (EDSL) para la composición y evaluación de contratos financieros, basado en el paper *"Composing Contracts: an Adventure in Financial Engineering"* de Simon Peyton Jones, Jean-Marc Eber y Julian Seward.

Trabajo Práctico Final — Análisis de Lenguajes de Programación (ALP), LCC, UNR.

**Autor:** Santiago Bussanich  
**Tutor:** Cecilia Manzino

---

## Características principales

- **Combinadores composicionales:** `zero`, `one`, `give`, `and`, `or`, `scale`, `truncate`, `then`, `if/else`.
- **Observables:** numéricos (`konst`, `obs`, operaciones aritméticas) y booleanos (`true`, `false`, `not`, `and`, `or`, `<`, `>`, `==`).
- **Billeteras con saldos por moneda** (USD, EUR, ARS, GBP): depósitos, verificación de fondos, transferencias atómicas.
- **Sistema de firmas duales:** `propose`, `sign`, `execute` — los contratos requieren la firma de ambas partes antes de ejecutarse.
- **Fecha mutable:** `setfecha` permite avanzar en el tiempo para simular escenarios temporales.
- **Exportación:**
  - Reporte HTML autocontenido con gráficos SVG embebidos (barras de cashflows, evolución temporal de billeteras, diagramas de AST).
  - Exportación de AST a archivo SVG independiente.
- **REPL interactivo** con Haskeline, carga de archivos `.fin`, y 15 ejemplos incluidos.

---

## Compilación y ejecución

### Requisitos

- [Stack](https://docs.haskellstack.org/) (GHC 9.6.6 se descarga automáticamente).

### Compilar

```bash
stack build
```

### Ejecutar el REPL

```bash
stack run
```

### Cargar un ejemplo

Dentro del REPL:

```
> :load Ejemplos/basico.fin
```

---

## Uso del REPL

### Comandos del lenguaje (sin prefijo `:`)

| Comando | Descripción |
|---|---|
| `let <x> = <contrato>` | Definir un contrato con nombre |
| `<contrato>` | Evaluar un contrato directamente |
| `deposit <parte> <monto> <moneda>` | Depositar fondos en billetera |
| `propose <nombre> <contrato>` | Proponer contrato para firma |
| `sign <nombre> <parte>` | Firmar un contrato pendiente |
| `execute <nombre>` | Ejecutar un contrato firmado |
| `setfecha YYYY-MM-DD` | Cambiar la fecha de evaluación |
| `if <cond> then <c1> else <c2>` | Condicional sobre observables |
| `c1 ; c2` | Secuenciar comandos |

### Comandos del REPL (con prefijo `:`)

| Comando | Descripción |
|---|---|
| `:ast <exp>` | Mostrar el AST de un contrato |
| `:pp <exp>` | Pretty print de un contrato |
| `:eval <exp>` | Evaluar un contrato y mostrar flujos |
| `:load <archivo>` | Cargar un archivo `.fin` |
| `:store` | Mostrar contratos definidos |
| `:quote [<nombre> <val>]` | Ver/definir cotización del oráculo |
| `:partes [<yo> <contra>]` | Ver/cambiar las partes |
| `:setfecha <YYYY-MM-DD>` | Cambiar fecha de evaluación |
| `:wallet [<parte>]` | Ver billetera(s) |
| `:pending` | Ver contratos pendientes de firma |
| `:historial` | Ver historial de ejecuciones |
| `:reporte [<archivo.html>]` | Exportar reporte HTML completo |
| `:ast-svg <contrato> > <archivo>` | Exportar AST a SVG |
| `:help` o `:?` | Mostrar ayuda |
| `:quit` | Salir |

---

## Ejemplo rápido

```
> deposit Alice 5000 USD
> deposit Bob 3000 EUR
> let bono = scale 1000.0 (one USD)
> let limitado = truncate 2027-01-01 bono
> propose pago limitado
> sign pago Alice
> sign pago Bob
> execute pago
Cashflows generados:
  2026-07-14  Alice → Bob  1000.00 USD
> :reporte
Reporte HTML exportado a reporte.html
```

---

## Estructura del proyecto

```
.
├── app/
│   └── Main.hs              # REPL interactivo (Haskeline)
├── src/
│   ├── AST.hs               # Tipos algebraicos: Contract, Obs, ObsBool, Comm
│   ├── Types.hs             # Tipos base: Currency, Amount, Cashflow, Env, Wallets
│   ├── Monads.hs            # Mónada Interp (StateT + ReaderT + WriterT + Either)
│   ├── Parser.hs            # Parser con Parsec
│   ├── Evaluator.hs         # Evaluador de contratos, observables y comandos
│   ├── PrettyPrinter.hs     # Pretty printer con precedencias
│   └── Exportar.hs          # HTML con SVG embebido, AST SVG, evolución temporal
├── Ejemplos/                 # 15 archivos .fin de ejemplo
│   ├── basico.fin
│   ├── bono_cupon.fin
│   ├── swap.fin
│   ├── opcion_call.fin
│   ├── empresa.fin
│   ├── graficoTemporal.fin
│   └── ...
├── package.yaml              # Configuración del proyecto (Stack)
├── stack.yaml                # Resolver de Stack
├── informe.tex               # Informe del trabajo
└── README.md
```

---

## Dependencias

| Paquete | Uso |
|---|---|
| `parsec` | Parsing del lenguaje |
| `containers` | `Data.Map` para billeteras, contratos, oráculo |
| `time` | Manejo de fechas (`Day`) |
| `mtl` / `transformers` | Stack de mónadas |
| `haskeline` | REPL interactivo con edición de línea |
| `exceptions` | Manejo de excepciones en Haskeline |

---

## Ejemplos incluidos

| Archivo | Descripción |
|---|---|
| `basico.fin` | Zero-coupon bond, contratos básicos |
| `bono_cupon.fin` | Bono con cupones periódicos |
| `swap.fin` | Currency swap USD↔EUR |
| `opcion_call.fin` | Opción call europea |
| `opcion_put.fin` | Opción put europea |
| `collar.fin` | Estrategia collar (call + put) |
| `derivados.fin` | Derivados con observables y negación |
| `cobertura.fin` | Cobertura (hedging) exportador |
| `prestamo.fin` | Préstamo simple |
| `prestamo_complejo.fin` | Préstamo con cobertura swap |
| `empresa.fin` | Escenario empresarial multi-moneda |
| `eleccion.fin` | Uso de `or` y `then` (fallback) |
| `reutilizacion.fin` | Reutilización de contratos definidos |
| `graficoTemporal.fin` | Evolución temporal con `setfecha` |
| `demo_completo.fin` | Demo integral: billeteras, firmas, ejecución |

---

## Referencias

- S. Peyton Jones, J.-M. Eber, J. Seward. *"Composing Contracts: an Adventure in Financial Engineering"*, ICFP 2000.
- S. Peyton Jones, J.-M. Eber. *"How to Write a Financial Contract"*, 2003.
