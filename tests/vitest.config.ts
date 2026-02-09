import { defineConfig } from 'vitest/config';
import path from 'path';

export default defineConfig({
  test: {
    include: ['./src/integration/**/*.test.ts', './src/unit/**/*.test.ts'],
  },
  resolve: {
    alias: {
      'web-api-client': path.resolve(__dirname, '../web-api-client/src'),
      src: path.resolve(__dirname, './src'),
    },
  },
  build: {
    commonjsOptions: {
      include: [/node_modules/, /web-api-client/],
    },
  },
  optimizeDeps: {
    force: true,
  },
});
