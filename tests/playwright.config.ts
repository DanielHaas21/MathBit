import { defineConfig } from '@playwright/test';
import * as dotenv from 'dotenv';

dotenv.config();

export default defineConfig({
  testDir: './src/e2e',
  use: {
    baseURL: process.env.WEB_APP_URL || 'http://localhost:4000',
    headless: true,
  },
});
