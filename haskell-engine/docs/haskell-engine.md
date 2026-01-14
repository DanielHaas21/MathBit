# MathBit - `haskell-engine`

This is the MathBit haskell-engine package

### Prerequisites

- GHC `>=9.2.7`
- Cabal `>=3.6`
- Docker

### Installation

No specific installation requirements

## Project Structure

Packages folder structure

```
app - contains the main haskell engine app file and server logic
src - main app logic
├───Engine
|   ├───Engine.hs - main engine logic
|   ├───Fold.hs - numeric folding module
|   ├───Normalize.hs - expression normalization module
├───Helpers - helper functions
├───Rules - math rules modules
├───Struct - data structures used in the engine
├───Api.hs - api server types
├───Cleanup.hs - expression cleanup module
├───Parser.hs - parser module into my custom data structure
├───Print.hs - pretty printer module, converts data structure back to LaTeX
├───Validator.hs - flags any special expression for rule application
```

### Workflow
This package works as a standalone microservice that exposes a simple HTTP API. 

Its inner workings quite complex, hence why every file is very well documented with comments.

The workflow is as follows:
1. The server receives a GET or POST request with a math expression in MathJSON array format.

2. The expression is parsed into a custom data structure.

3. The engine then recursively applies applies folding and normalization to the expression while on each iteration checking for applicable math rules. Also on each iteration the engine logs the current expression state into a list of steps that will be returned to the user.

4. Once no more rules can be applied, the expression is "cleaned up" from math expreession artifacts that are used by the engine and work well but arent meant to be seen by the user. Ex. converting Division into negative exponents.

5. The final expression is then pretty printed back into LaTeX format and returned in the HTTP response, along with each logged step.

### Utilities

There are no additional utilities setup for this project.
