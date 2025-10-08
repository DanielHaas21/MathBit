# MathBit - `web-api`

This is the MathBit web-api package. It is based on TSOA and swagger spec, and generates an open api specification.

### Prerequisites

- Node.js `>=18.17`
- Docker (running container)

### Installation

No specifc installation requierements

## Project Structure

Packages folder structure

```
src
├───controllers - Controller declarations for TSOA, db repository functions are used here
├───dto - Data transfer objects
├───generated - generated SDKs
├───mappers - Mappers that map model -> dto and vice versa for each given table
├───model
├───services
├───appConfig.ts - port configs etc.
├───authentication.ts jwt-express auth base methods & config
├───generate.ts - SDK transpiling and generating logic
├───index.ts - init file for express and db
├───seed.ts - test data for tables
```

### Workflow

refer to [generating](./generating.md)

### Utilities

This project has some additional tools already setup for you:

- [TypeScript](https://www.typescriptlang.org/) for static type checking
- [nodemon](https://nodemon.io/) for automatic server reloads
- [eslint](https://eslint.org/) for linting
