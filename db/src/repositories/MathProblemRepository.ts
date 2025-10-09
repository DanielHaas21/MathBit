import { Kysely } from 'kysely';
import { Database, getDb } from '../db';
import { MathProblem } from '../models/MathProblem';

export class MathProblemRepository {
  private db: Kysely<Database>;

  constructor() {
    this.db = getDb();
  }

  async createMathProblem(
    userId: number,
    problem: Omit<MathProblem, 'id' | 'created' | 'userId'>
  ): Promise<number | undefined> {
    const result = await this.db
      .insertInto('math_problem')
      .values({
        ...(problem as MathProblem),
        userId: userId, // User without an id cannot exist
      })
      .returning('id')
      .execute();
    return result[0]?.id;
  }

  async getMathProblemById(id: number): Promise<MathProblem | null> {
    const result = await this.db
      .selectFrom('math_problem')
      .selectAll()
      .where('id', '=', id)
      .executeTakeFirst();

    return result || null;
  }

  async getMathProblemsByUserId(userId: number): Promise<MathProblem[] | null> {
    const result = await this.db
      .selectFrom('math_problem')
      .selectAll()
      .where('userId', '=', userId)
      .execute();

    return result || null;
  }

  async getMathProblemByBookmark(userId: number): Promise<MathProblem[] | null> {
    const result = await this.db
      .selectFrom('math_problem')
      .selectAll()
      .where('userId', '=', userId)
      .where('bookmark', '=', true)
      .execute();

    return result || null;
  }

  async updateMathProblem(id: number, problem: Partial<MathProblem>): Promise<void> {
    await this.db.updateTable('math_problem').set(problem).where('id', '=', id).execute();
  }

  async deleteMathProblemById(id: number): Promise<void> {
    await this.db.deleteFrom('math_problem').where('id', '=', id).execute();
  }
}
