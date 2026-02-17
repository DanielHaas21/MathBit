import baseConfig from '../eslint.config.mjs';
import reactPlugin from 'eslint-plugin-react';

export default [
  ...baseConfig,
  {
    plugins: {
      react: reactPlugin,
    },
    rules: {
      ...reactPlugin.configs.recommended.rules,
      ...reactPlugin.configs['jsx-runtime'].rules,
      '@typescript-eslint/no-unused-vars': 'off', // allows unused variables in React components, which is common when defining props interfaces
      '@typescript-eslint/no-unused-expressions': 'off', // allows short-circuiting like `condition && doSomething()`, which is common in React components
      'react/prop-types': 'off', // keeps complaining about forwardRef and FC props, which are typed with TypeScript, idk
    },
  },
];
