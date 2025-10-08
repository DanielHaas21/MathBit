# MathBit - `web-api-client`

This is the MathBit web-api-client package.

### Prerequisites

- Node.js `>=18.17`

### Installation

No specifc installation requierements

## Project Structure

Packages folder structure

```
├───generated -generated api client files
├───schema/schema.json - contains open specification that kubb uses for generating
├───src/index.ts
├───kubb.config.ts - kubb config with plugins 
```

### Workflow

This package works purely as a library, it only has one important command `generate` which generates the api-client routes

The package will be transpiled on the frontend, so that path references arent made directly to this package

### Utilities

This project has some additional tools already setup for you:

- [TypeScript](https://www.typescriptlang.org/) for static type checking
- [eslint](https://eslint.org/) for linting