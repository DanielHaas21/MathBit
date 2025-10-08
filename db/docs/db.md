# MathBit - `db`

This is the MathBit db package. it is based on Kysely and Migrator and contains DB table Models, repostitory functions and migrations

### Prerequisites

- Node.js `>=18.17`
- Docker

### Installation

No specifc installation requierements

## Project Structure

Packages folder structure

```
dist
src
├───migrations - up/down migrations that create/alter tables
├───models - contains models for tables
├───repositories - functions like SelectAll etc. for each given table
├───db.ts - exposed methods like initDb(), migrate() used in other packages
```

Migration files have a specific naming convention used by Kysely to track if the migration was already executed at some point

format: **data_name**

### Workflow

This package works purely as a library, it has no comamnds. All models, functions are exposed to be used elsewhere

Such as:

- Repository functions and Models are called `/controllers` in the **web-api** package
- Migrations have a function that calls all of them, Kysely track which already have been executed
- db.ts exports **initDb()** and **migrateToLatest()** methods which are used in **web-api** initialization file before turning on express routes

### Utilities

This project has some additional tools already setup for you:

- [TypeScript](https://www.typescriptlang.org/) for static type checking
- [eslint](https://eslint.org/) for linting