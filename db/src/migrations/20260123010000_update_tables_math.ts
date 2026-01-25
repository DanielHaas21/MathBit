import { Kysely } from 'kysely';
import { Database } from '../db';

export async function up(db: Kysely<Database>): Promise<void> {
  await db.schema
    .alterTable('math_problem')
    .addColumn('description', 'varchar(255)', (col) => col.notNull())
    .dropColumn('latex')
    .execute();
  await db.schema
    .alterTable('math_problem_step')
    .addColumn('step_before', 'varchar(255)', (col) => col.notNull())
    .addColumn('step_after', 'varchar(255)', (col) => col.notNull())
    .dropColumn('expression')
    .dropColumn('latex')
    .execute();
}
