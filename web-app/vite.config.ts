// vite.config.ts
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import ssr from 'vite-plugin-ssr/plugin';
import path from 'path';

export default defineConfig({
  plugins: [react(), ssr()],
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './app'),
      '@libs': path.resolve(__dirname, './libs'),
      'web-api-client': path.resolve(__dirname, '../web-api-client/src'), // <-- add this
    },
  },
  build: {
    commonjsOptions: {
      include: [/node_modules/, /web-api-client/],
    },
  },
  server: {
    fs: {
      allow: ['..'], // <-- allow Vite to read outside web-app
    },
  },
});
