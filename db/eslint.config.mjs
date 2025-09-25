import baseConfig from '../eslint.config.mjs';

export default [
  ...baseConfig,
  {
    languageOptions: {
      globals: {
        // Node env
        require: 'readonly',
        module: 'readonly',
        process: 'readonly',
      },
    },
    rules: {
      'no-console': 'off',
    },
  },
];
