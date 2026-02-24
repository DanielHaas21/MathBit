import express from 'express';
import bodyParser from 'body-parser';
import swaggerUi from 'swagger-ui-express';
import { RegisterRoutes } from './generated/routes/routes';
import swaggerDoc from './generated/swagger/swagger.json';
import cors from 'cors';
import { getAppConfig } from './appConfig';
import { initDb, migrateToLatest } from 'db';
import path from 'path';
import cookieParser from 'cookie-parser';

const config = getAppConfig();
initDb(config.db);

(async () => {
  await migrateToLatest();
})();

const app = express();

const allowedOrigins = (config.webapp.url ?? 'https://haasdaniel.cz')
  .split(',')
  .map((origin) => origin.trim())
  .filter(Boolean);

if (!allowedOrigins.includes('https://www.haasdaniel.cz')) {
  allowedOrigins.push('https://www.haasdaniel.cz');
}

const corsOptions = {
  origin: (origin: string | undefined, callback: (err: Error | null, allow?: boolean) => void) => {
    if (!origin || allowedOrigins.includes(origin)) {
      return callback(null, true);
    }

    return callback(new Error('Not allowed by CORS'));
  },
  credentials: true,
  methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'],
  allowedHeaders: ['Content-Type', 'Authorization'],
};

app.use(cors(corsOptions));

// Preflight for all routes
app.options('*', cors(corsOptions));
app.use(bodyParser.json());

// cookie parser middleware
app.use(cookieParser());

// Register routes generated from controllers
RegisterRoutes(app);
// Swagger UI specification
app.use('/docs', swaggerUi.serve, swaggerUi.setup(swaggerDoc));

// Serve the generated Swagger JSON file at the /swagger.json route
app.use(
  '/swagger.json',
  express.static(path.join(__dirname, 'generated', 'swagger', 'swagger.json'))
);

// Run server
app.listen(config.app.port, '0.0.0.0', () => {
  console.log(`Server is running on http://0.0.0.0:${config.app.port}`);
});
