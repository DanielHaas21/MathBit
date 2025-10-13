import { Kysely } from 'kysely';
import { Database, getDb } from '../db';
import { MathProblem } from '../models/MathProblem';
import { MathProblemQuery } from '../types/MathProblemQuery';

export class MathProblemRepository {
  private db: Kysely<Database>;

  constructor() {
    this.db = getDb();
  }

  async createMathProblem(
    userId: number,
    problem: Omit<MathProblem, 'id' | 'created' | 'userId'>
  ): Promise<number | undefined> {
    // Count existing problems to generate default name, with an incermenting value
    const countResult = await this.db
      .selectFrom('math_problem')
      .select(({ fn }) => [fn.count<number>('id').as('count')])
      .where('userId', '=', userId)
      .executeTakeFirst();

    const count = countResult?.count ?? 0;
    const defaultName = `Problem #${count + 1}`;

    const result = await this.db
      .insertInto('math_problem')
      .values({
        ...(problem as MathProblem),
        userId: userId,
        name: problem.name ?? defaultName,
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

  async getMathProblems(filters: MathProblemQuery, offset: number = 0, limit: number = 20) {
    let query = this.db.selectFrom('math_problem').selectAll();

    if (filters.userId) query = query.where('userId', '=', filters.userId);
    if (filters.bookmark) query = query.where('bookmark', '=', true);
    if (filters.name) query = query.where('name', 'like', `%${filters.name}%`);

    return await query.limit(limit).offset(offset).execute();
  }

  async updateMathProblem(id: number, problem: Partial<MathProblem>): Promise<void> {
    await this.db.updateTable('math_problem').set(problem).where('id', '=', id).execute();
  }

  async deleteMathProblemById(id: number): Promise<void> {
    await this.db.deleteFrom('math_problem').where('id', '=', id).execute();
  }
}
