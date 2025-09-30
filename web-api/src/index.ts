import express from 'express';
import bodyParser from 'body-parser';
import swaggerUi from 'swagger-ui-express';
import { RegisterRoutes } from './generated/routes/routes';
import swaggerDoc from './generated/swagger/swagger.json';

import { getAppConfig } from './appConfig';
import { initDb, migrateToLatest } from 'db';
import path from 'path';

const config = getAppConfig();
initDb(config.db);

(async () => {
  await migrateToLatest();
})();

const app = express();
app.use(bodyParser.json());
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
app.listen(config.app.port, () => {
  console.log(`Server is running on http://localhost:${config.app.port}`);
});
