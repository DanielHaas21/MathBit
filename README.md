# MathBit

This is a Maturita project monorepo for a web app that is used for visually teaching and displaying mathmetical expressions running on a haskell engine

### Prerequisites

- Node.js `>=18.17`
- Docker
- GCHUB

### Installation

Clone the repository and install dependencies:
There are specific installations and instructions for each package of the monorepo; for these refer to docs/.md files in each package

```bash
pnpm install
```

### Development

To develop the app and packages, run the following command:

```bash
pnpm dev
```

### Build

To build the app and packages, run the following command:

```bash
pnpm build
```

## Project Structure

Project consists of four pnpm packages and one standalone package

- **web-app** React Router based frontend and UI toolkit
- **web-api** TSOA/Swagger api specification that generates an SDK
- **web-api-client** API client that generates typed routes used on the frontend from the Swagger SDK
- **db** Kysely based db library that contains table, and repository specifications used to make requests to a Postgres db container
- **haskell-engine** Standalone HTTP-microservice that handles math computations, it exposes a simple GET/POST API

App folder structure

```
├───web-app
├───web-api
├───web-api-client
├───db
├───haskell-engine
```

each package has a `/docs` folder at root that contains more specific documentation & instuctions for each package

### Utilities

This project has some additional tools already setup for you:

- [TypeScript](https://www.typescriptlang.org/) for static type checking
- [Prettier](https://prettier.io/) for better code formatting
- [eslint](https://eslint.org/) for linting
