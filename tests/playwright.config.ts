import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: './tests/e2e',
  use: {
    baseURL: process.env.WEB_APP_URL || 'http://localhost:4100',
    headless: true,
  },
  webServer: {
    command: 'pnpm run dev:web-app',
    url: process.env.WEB_APP_URL || 'http://localhost:4000',
    reuseExistingServer: true,
  },
});
