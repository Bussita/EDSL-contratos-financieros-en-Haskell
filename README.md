# Financial Contracts EDSL

Un **Embedded Domain Specific Language** (EDSL) implementado en Haskell para modelar y evaluar contratos financieros.

## Descripción

Este proyecto implementa un lenguaje específico de dominio para describir contratos financieros. Basándose en el paper de referencia, el EDSL permite:

- Definir contratos financieros de forma declarativa
- Parsear descripciones de contratos usando Parsec
- Evaluar contratos con diferentes semánticas
- Componer contratos de forma modular

## Estructura del Proyecto

```
.
├── src/
│   ├── AST.hs              # Tipos de datos del AST
│   ├── Parser.hs           # Parser con Parsec
│   ├── Types.hs            # Tipos base del EDSL
│   └── Evaluator.hs        # Evaluador de contratos
├── app/
│   └── Main.hs             # Aplicación principal
├── test/
│   └── Main.hs             # Tests
├── package.yaml            # Configuración del proyecto
├── stack.yaml              # Configuración de Stack
└── README.md               # Este archivo
```

## Requisitos

- [Stack](https://docs.haskellstack.org/)
- GHC 9.6+

## Dependencias

- `parsec` (>= 3.1.14): Parser combinator library
- `containers` (>= 0.6): Estructuras de datos
- `time` (>= 1.9): Manejo de fechas
- `text` (>= 1.2): Textos eficientes

## Compilación y Ejecución

```bash
# Compilar el proyecto
stack build

# Ejecutar la aplicación
stack run

# Ejecutar tests
stack test
```

## Desarrollo

El desarrollo se estructura en fases:

1. **Tipos Base**: Definir la estructura del AST
2. **Parser**: Implementar el parser con Parsec
3. **Evaluador**: Implementar la evaluación de contratos
4. **Tests**: Pruebas unitarias

## Referencias

- Paper: Financial Contracts EDSL
