import { Kysely, Migrator, PostgresDialect, FileMigrationProvider, CamelCasePlugin } from 'kysely';
import { Pool, PoolConfig } from 'pg';
import * as path from 'path';
import { promises as fs } from 'fs';
import { User } from './models/User';
import { MathProblem } from './models/MathProblem';
import { MathProblemStep } from './models/MathProblemStep';

/**
 * Database schema definition.
 */
export interface Database {
  user: User;
  math_problem: MathProblem;
  math_problem_step: MathProblemStep;
  //   user_login: UserLogin;
}

let pool: Pool | null = null;

const initDb = (config?: PoolConfig) => {
  pool = new Pool({ ...config });
};

/**
 * Database query builder.
 */
const getDb = () => {
  if (!pool) {
    throw new Error('DB is not initialized. Configure it using init first.');
  }
  return new Kysely<Database>({
    dialect: new PostgresDialect({
      pool: pool,
    }),
    plugins: [new CamelCasePlugin()],
  });
};

/**
 * Migration function forked from coworker, rest is done by me
 */
async function migrateToLatest() {
  const db = getDb();
  const migrator = new Migrator({
    db,
    provider: new FileMigrationProvider({
      fs,
      path,
      // This needs to be an absolute path.
      migrationFolder: path.join(__dirname, './migrations'),
    }),
  });

  console.log(db);
  console.log('Folder with migrations:');
  console.log(path.join(__dirname, './migrations'));
  console.log('Available migrations:');
  console.log(JSON.stringify(migrator.getMigrations(), null, 2));

  const { error, results } = await migrator.migrateToLatest();

  results?.forEach((it) => {
    if (it.status === 'Success') {
      console.log(`migration "${it.migrationName}" was executed successfully`);
    } else if (it.status === 'Error') {
      console.error(`failed to execute migration "${it.migrationName}"`);
    }
  });

  if (error) {
    console.error('failed to migrate');
    console.error(error);
    process.exit(1);
  }
}

export { initDb, getDb, migrateToLatest };
