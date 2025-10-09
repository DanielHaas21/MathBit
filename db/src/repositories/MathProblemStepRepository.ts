import { Kysely } from 'kysely';
import { Database, getDb } from '../db';
import { MathProblemStep } from '../models/MathProblemStep';

export class MathProblemStepRepository {
  private db: Kysely<Database>;

  constructor() {
    this.db = getDb();
  }

  async createMathProblemStep(
    problemId: number,
    problemStep: Omit<MathProblemStep, 'id' | 'created' | 'problemId'>
  ): Promise<number | undefined> {
    const result = await this.db
      .insertInto('math_problem_step')
      .values({
        ...(problemStep as MathProblemStep),
        problemId: problemId,
      })
      .returning('id')
      .execute();
    return result[0]?.id;
  }

  async getMathProblemStepById(id: number): Promise<MathProblemStep | null> {
    const result = await this.db
      .selectFrom('math_problem_step')
      .selectAll()
      .where('id', '=', id)
      .executeTakeFirst();

    return result || null;
  }

  async getMathProblemStepsByProblemId(problemId: number): Promise<MathProblemStep[] | null> {
    const result = await this.db
      .selectFrom('math_problem_step')
      .selectAll()
      .where('problemId', '=', problemId)
      .execute();

    return result || null;
  }

  async getMathProblemStepByStepIndex(
    stepIndex: number,
    problemId: number
  ): Promise<MathProblemStep | null> {
    const result = await this.db
      .selectFrom('math_problem_step')
      .selectAll()
      .where('problemId', '=', problemId)
      .where('stepIndex', '=', stepIndex)
      .executeTakeFirst();

    return result || null;
  }

  async updateMathProblemStep(id: number, problemStep: Partial<MathProblemStep>): Promise<void> {
    await this.db.updateTable('math_problem_step').set(problemStep).where('id', '=', id).execute();
  }

  async deleteMathProblemStepById(id: number): Promise<void> {
    await this.db.deleteFrom('math_problem_step').where('id', '=', id).execute();
  }

  async deleteMathProblemStepByProblemId(problemId: number): Promise<void> {
    await this.db.deleteFrom('math_problem_step').where('problemId', '=', problemId).execute();
  }
}
