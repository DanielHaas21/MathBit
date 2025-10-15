import baseConfig from '../../eslint.config.mjs';
import reactPlugin from 'eslint-plugin-react';

export default [
  ...baseConfig,
  {
    rules: {
      ...reactPlugin.configs.recommended.rules,
      ...reactPlugin.configs['jsx-runtime'].rules,
      '@typescript-eslint/no-unused-vars': 'off',
      '@typescript-eslint/no-unused-expressions': 'off',
    },
  },
];
