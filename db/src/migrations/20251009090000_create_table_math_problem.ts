import { Kysely } from 'kysely';
import { Database } from '../db';

export async function up(db: Kysely<Database>): Promise<void> {
  await db.schema
    .createTable('math_problem')
    .addColumn('id', 'serial', (col) => col.primaryKey())
    .addColumn('user_id', 'integer', (col) => col.notNull())
    .addColumn('original_expression', 'varchar(255)', (col) => col.notNull())
    .addColumn('name', 'varchar(255)', (col) => col.notNull())
    .addColumn('latex', 'varchar(255)', (col) => col.notNull())
    .addColumn('simplified_expression', 'varchar(255)', (col) => col.notNull())
    .addColumn('bookmark', 'boolean', (col) => col.notNull())
    .addColumn('created', 'timestamp', (col) => col.notNull().defaultTo(new Date()))
    .addColumn('updated', 'timestamp', (col) => col.notNull().defaultTo(new Date()))
    .execute();
  await db.schema
    .createTable('math_problem_step')
    .addColumn('id', 'serial', (col) => col.primaryKey())
    .addColumn('problem_id', 'integer', (col) => col.notNull())
    .addColumn('step_index', 'integer', (col) => col.notNull())
    .addColumn('description', 'varchar(255)', (col) => col.notNull())
    .addColumn('expression', 'varchar(255)', (col) => col.notNull())
    .addColumn('latex', 'varchar(255)', (col) => col.notNull())
    .addColumn('created', 'timestamp', (col) => col.notNull().defaultTo(new Date()))
    .execute();
}

export async function down(db: Kysely<Database>): Promise<void> {
  await db.schema.dropTable('math_problem').execute();
  await db.schema.dropTable('math_problem_step').execute();
}
