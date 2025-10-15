import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import path from 'path';

export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './app'),
      '@libs': path.resolve(__dirname, './libs'),
      'web-api-client': path.resolve(__dirname, '../web-api-client/src'),
    },
  },
  build: {
    commonjsOptions: {
      include: [/node_modules/, /web-api-client/],
    },
  },

  server: {
    fs: {
      allow: ['..'],
    },
  },
});
