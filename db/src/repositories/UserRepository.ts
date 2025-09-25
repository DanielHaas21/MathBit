import { Kysely } from 'kysely';
import { Database, getDb } from '../db';
import { User } from '../models/User';

export class UserRepository {
  private db: Kysely<Database>;

  constructor() {
    this.db = getDb();
  }

  async createUser(user: User): Promise<number | undefined> {
    const result = await this.db.insertInto('user').values(user).returning('id').execute();
    return result[0]?.id;
  }

  async getUserById(id: number): Promise<User | null> {
    const result = await this.db
      .selectFrom('user')
      .selectAll()
      .where('id', '=', id)
      .executeTakeFirst();

    return result || null;
  }

  async getUserByEmail(email: string): Promise<User | null> {
    const result = await this.db
      .selectFrom('user')
      .selectAll()
      .where('email', '=', email)
      .executeTakeFirst();

    return result || null;
  }

  async getAllUsers(): Promise<User[]> {
    return await this.db.selectFrom('user').selectAll().execute();
  }

  async updateUser(id: number, user: Partial<User>): Promise<void> {
    await this.db.updateTable('user').set(user).where('id', '=', id).execute();
  }
}
