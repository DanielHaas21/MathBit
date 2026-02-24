import { Kysely } from 'kysely';
import { Database } from '../db';

export async function up(db: Kysely<Database>): Promise<void> {
  await db.schema
    .alterTable('math_problem')
    .addColumn('name', 'varchar(255)', (col) => col.notNull())
    .execute();
}
