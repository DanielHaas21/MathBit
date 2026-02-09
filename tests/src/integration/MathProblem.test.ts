import { describe, it, expect, beforeAll } from 'vitest';
import {
  type CreateMathProblemRequest,
  type UpdateMathProblemRequest,
  type MathProblemQuery,
  type LoginResponse,
  createUser,
  login,
  createMathProblem,
  getMathProblemById,
  getMathProblems,
  deleteMathProblem,
  updateMathProblem,
  deleteUser,
} from 'web-api-client';
import { getApiConfig } from 'src/utils/getApiConfig';

const testEmail = `mathproblem_${Date.now()}@example.com`;
const testPassword = 'Test123!';
let accessToken: string;
let userId: number;
let mathProblemId: number;
describe('MathProblemController – integration roundtrip', () => {
  beforeAll(async () => {
    const user = await createUser(
      {
        email: testEmail,
        password: testPassword,
        username: 'Math User',
      },
      getApiConfig(true, accessToken)
    );

    expect(user).toBeDefined();
    userId = user?.id as number;

    const response: LoginResponse = await login(
      { email: testEmail, password: testPassword },
      getApiConfig(false, accessToken)
    );

    expect(response.accessToken).toBeDefined();
    accessToken = response.accessToken!;
  });

  it('should create a math problem', async () => {
    const body: CreateMathProblemRequest = {
      userId,
      problem: {
        name: 'Integration test problem',
        originalExpression: '2 + 2',
        simplifiedExpression: '4',
        description: 'A simple addition problem for integration testing',
      },
    };

    const created = await createMathProblem(body, getApiConfig(false, accessToken));

    expect(created).toBeDefined();
    expect(created?.name).toBe('Integration test problem');

    mathProblemId = created!.id!;
  });

  it('should get math problem by id', async () => {
    const fetched = await getMathProblemById(mathProblemId, getApiConfig(false, accessToken));

    expect(fetched).toBeDefined();
    expect(fetched?.id).toBe(mathProblemId);
    expect(fetched?.name).toBe('Integration test problem');
  });

  it('should search math problems', async () => {
    const query: MathProblemQuery = {
      userId,
      name: 'Integration test problem',
    };

    const result = await getMathProblems(
      {
        offset: 0,
        limit: 10,
      },
      query,
      getApiConfig(false, accessToken)
    );

    expect(result.data.length).toBeGreaterThan(0);
    expect(result.data.map((p) => p.id)).toContain(mathProblemId);
  });

  it('should update math problem', async () => {
    const body: UpdateMathProblemRequest = {
      id: mathProblemId,
      problem: {
        name: 'Updated problem',
        originalExpression: '3 + 3',
        simplifiedExpression: '6',
        description: 'Updated description',
      },
    };

    await updateMathProblem(body, getApiConfig(false, accessToken));

    const updated = await getMathProblemById(mathProblemId, getApiConfig(false, accessToken));
    expect(updated?.name).toBe('Updated problem');
  });

  it('should delete math problem', async () => {
    await deleteMathProblem(mathProblemId, getApiConfig(false, accessToken));

    await expect(
      getMathProblemById(mathProblemId, getApiConfig(false, accessToken))
    ).rejects.toThrow();

    await deleteUser(userId, getApiConfig(false, accessToken)); // Cleanup user after tests
  });
});
