# Financial Contracts EDSL

Un **Embedded Domain Specific Language** (EDSL) implementado en Haskell para modelar y evaluar contratos financieros, basado en el paper *"Composing Contracts: An Adventure in Financial Engineering"* de Peyton Jones, Eber y Seward.

## Descripción

Este proyecto implementa un lenguaje específico de dominio para describir, componer y evaluar contratos financieros. El EDSL permite:

- **Definir** contratos financieros de forma declarativa y composicional
- **Parsear** descripciones de contratos usando combinadores de Parsec
- **Evaluar** contratos para generar flujos de caja (cashflows)
- **Componer** contratos de forma modular usando combinadores (`and`, `or`, `then`, `give`, `scale`, `truncate`, etc.)
- **Nombrar** contratos con `let` y reutilizarlos
- **Pretty-print** contratos con paréntesis mínimos

## Estructura del Proyecto

```
.
├── src/
│   ├── AST.hs              # Tipos de datos: Contract, Comm, Obs
│   ├── Types.hs            # Tipos base: Currency, Cashflow, Env, EvalError
│   ├── Monads.hs           # Mónada Eval (Reader + Writer + Either) construida a mano
│   ├── Utils.hs            # Helpers monádicos: ask, throw, localEnv, censor
│   ├── Parser.hs           # Parser de contratos, observables y comandos (Parsec)
│   ├── Evaluator.hs        # Evaluador de contratos y comandos
│   └── PrettyPrinter.hs    # Pretty printer con precedencias
├── app/
│   └── Main.hs             # REPL interactivo con Haskeline
├── test/
│   └── Main.hs             # Suite de tests con HUnit
├── Ejemplos/
│   ├── basico.fin          # Zero-coupon bond
│   ├── swap.fin            # Currency swap
│   ├── observables.fin     # Contratos con observables externos
│   └── composicion.fin     # Composición compleja con or y then
├── package.yaml
├── stack.yaml
└── README.md
```

## Compilación y Ejecución

```bash
# Compilar el proyecto
stack build

# Ejecutar el REPL
stack run

# Ejecutar los tests
stack test
```

## Uso del REPL

```
Fin> one USD
Fecha        Monto          Desde → Hacia
-----------------------------------------
2025-01-01  1 USD  Banco → Bussa

Fin> let swap = give one USD and one EUR
Contrato(s) asignado(s) correctamente.

Fin> scale 1000.0 swap
Fecha        Monto          Desde → Hacia
-----------------------------------------
2025-01-01  1000 USD  Bussa → Banco
2025-01-01  1000 EUR  Banco → Bussa

Fin> :load Ejemplos/basico.fin
Archivo Ejemplos/basico.fin cargado correctamente.

Fin> :store
  zcb = truncate 2026-01-01 scale 1000.0 one USD

Fin> :pp give (swap and truncate 2025-06-01 one GBP)
give (swap and truncate 2025-06-01 one GBP)

Fin> :ast one USD and give one EUR
AST:
And (One USD) (Give (One EUR))
```

### Comandos del REPL

| Comando | Descripción |
|---|---|
| `<contrato>` | Evaluar un contrato/comando y mostrar cashflows |
| `let x = <contrato>` | Asignar un alias a un contrato |
| `c1 ; c2` | Secuenciar comandos |
| `:ast <exp>` | Mostrar el AST de un contrato |
| `:pp <exp>` | Pretty print de un contrato |
| `:eval <exp>` | Evaluar un contrato directamente |
| `:load <archivo>` | Cargar un archivo `.fin` |
| `:store` | Mostrar contratos definidos |
| `:help` o `:?` | Mostrar ayuda |
| `:quit` | Salir |

## Sintaxis del Lenguaje

### Contratos

| Constructor | Sintaxis | Significado |
|---|---|---|
| `Zero` | `zero` | Contrato vacío |
| `One c` | `one USD` | Recibir 1 unidad de moneda `c` |
| `Give c` | `give c` | Invertir roles (yo pago) |
| `And c1 c2` | `c1 and c2` | Adquirir ambos contratos |
| `Or c1 c2` | `c1 or c2` | Elegir el más favorable |
| `Then c1 c2` | `c1 then c2` | Intentar `c1`, si no produce usar `c2` |
| `Scale o c` | `scale o c` | Escalar cashflows por observable `o` |
| `Truncate d c` | `truncate YYYY-MM-DD c` | Limitar contrato a fecha `d` |
| `Get c` | `get c` | Adquirir contrato hoy |
| `Anytime c` | `anytime c` | Ejercer cuando convenga (simplificado: hoy) |

### Observables

Los observables son expresiones aritméticas que se evalúan al momento de ejecutar un `scale`:

- Constantes: `100.0`, `3.14`
- Externos (oráculo): `OIL`, `AAPL`, `USD_ARS`
- Aritmética: `+`, `-`, `*`, `/`

Ejemplo: `scale OIL * 100.0 + 50.0 one USD`

### Monedas soportadas

`USD`, `EUR`, `ARS`, `GBP`

## Arquitectura

### Mónada Eval

La mónada `Eval` está construida **a mano** (sin usar MTL/transformers) y combina tres efectos:

```
Eval a = Env -> Either EvalError (a, [Cashflow])
```

- **Reader** (`Env`): acceso al entorno (fecha, oráculo, partes)
- **Writer** (`[Cashflow]`): acumulación de flujos de caja
- **Either** (`EvalError`): manejo de errores

### Evaluación de comandos

El sistema de comandos separa la **asignación** de la **evaluación**:

1. `Assign`: sustituye variables → guarda en `ContractStore` → no genera cashflows
2. `Run`: sustituye variables → evalúa con `evalContract` → genera cashflows
3. `Seq`: ejecuta secuencialmente, propagando el store

La sustitución de variables (`substContract`) resuelve recursivamente todas las `Var` antes de evaluar, evitando la necesidad de modificar la mónada `Eval`.

### Pretty Printer

Usa niveles de precedencia para minimizar paréntesis, tanto para contratos como para observables. La propiedad `parse (ppContract c) == c` se testea como roundtrip.

## Dependencias

- `parsec` (>= 3.1.14): Combinadores de parseo
- `containers` (>= 0.6): `Data.Map` para el store
- `time` (>= 1.9): Fechas (`Day`)
- `haskeline`: REPL interactivo
- `HUnit`: Tests unitarios

## Referencias

- S. Peyton Jones, J-M. Eber, J. Seward. *"Composing Contracts: An Adventure in Financial Engineering"*. ICFP 2000.
