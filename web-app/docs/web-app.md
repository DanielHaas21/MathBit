# MathBit - `web-app`

This package contains the frontend react-router based app and its libraries, most notably the [ui-toolkit](./ui-toolkit.md)

### Prerequisites

- Node.js `>=18.17`

### Installation

No specifc installation requierements

## Project Structure

Packages folder structure

```
src
├───app - router frontend app
├───i18n - localization config
├───libs
|   ├───auth - auth helper files
|   ├───hooks - custom hooks
|   ├───math - Frontend math functions
|   ├───ui - ui toolkit, for more see ui-toolkit.md
├───messages - contains .json localization files
├───scripts - contains tailwind and color watch and create scripts
├───types
```

This package also transpiles `web-api-client`

### Workflow

...

### Utilities

This project has some additional tools already setup for you:

- [TypeScript](https://www.typescriptlang.org/) for static type checking
- [eslint](https://eslint.org/) for linting
