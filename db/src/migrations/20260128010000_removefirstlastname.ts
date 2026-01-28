import { Kysely } from 'kysely';
import { Database } from '../db';

export async function up(db: Kysely<Database>): Promise<void> {
  await db.schema.alterTable('user').dropColumn('first_name').dropColumn('last_name').execute();
}
