import * as dotenv from 'dotenv';

dotenv.config();

export function getAppConfig() {
  const cfg = {
    webapp: {
      url: process.env.WEB_APP_URL,
    },
    app: {
      port: process.env.APP_PORT ? parseInt(process.env.APP_PORT) : 3001,
      secret: process.env.APP_SECRET || '',
    },
    db: {
      user: process.env.DB_USERNAME,
      host: process.env.DB_HOST,
      database: process.env.DB_DATABASE,
      password: process.env.DB_PASSWORD,
      port: process.env.DB_PORT ? parseInt(process.env.DB_PORT) : 5433,
    },
    haskellEngine: {
      url: process.env.HASKELL_ENGINE_URL || 'http://localhost:8080',
    },
  };
  return cfg;
}
