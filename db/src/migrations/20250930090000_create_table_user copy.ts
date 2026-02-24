import { Kysely } from 'kysely';
import { Database } from '../db';

export async function up(db: Kysely<Database>): Promise<void> {
  await db.schema
    .createTable('user')
    .addColumn('id', 'serial', (col) => col.primaryKey())
    .addColumn('username', 'varchar(255)', (col) => col.notNull())
    .addColumn('email', 'varchar(255)', (col) => col.notNull())
    .addColumn('password', 'varchar(255)', (col) => col.notNull())
    .addColumn('created', 'timestamp', (col) => col.notNull().defaultTo(new Date()))
    .execute();

  await db.schema
    .createTable('math_problem')
    .addColumn('id', 'serial', (col) => col.primaryKey())
    .addColumn('title', 'varchar(255)', (col) => col.notNull())
    .addColumn('user_id', 'integer', (col) => col.notNull())
    .addColumn('original_expression', 'varchar(255)', (col) => col.notNull())
    .addColumn('name', 'varchar(255)', (col) => col.notNull())
    .addColumn('simplified_expression', 'varchar(255)', (col) => col.notNull())
    .addColumn('description', 'varchar(255)', (col) => col.notNull())
    .addColumn('bookmark', 'boolean', (col) => col.notNull().defaultTo(false))
    .addColumn('created', 'timestamp', (col) => col.notNull().defaultTo(new Date()))
    .addColumn('updated', 'timestamp', (col) => col.notNull().defaultTo(new Date()))
    .execute();

  await db.schema
    .createTable('math_problem_step')
    .addColumn('id', 'serial', (col) => col.primaryKey())
    .addColumn('description', 'varchar(255)', (col) => col.notNull())
    .addColumn('step_index', 'integer', (col) => col.notNull())
    .addColumn('problem_id', 'integer', (col) => col.notNull())
    .addColumn('step_before', 'varchar(255)', (col) => col.notNull())
    .addColumn('step_after', 'varchar(255)', (col) => col.notNull())
    .addColumn('created', 'timestamp', (col) => col.notNull().defaultTo(new Date()))
    .execute();
}

export async function down(db: Kysely<Database>): Promise<void> {
  await db.schema.dropTable('math_problem_step').execute();
  await db.schema.dropTable('math_problem').execute();
  await db.schema.dropTable('user').execute();
}
