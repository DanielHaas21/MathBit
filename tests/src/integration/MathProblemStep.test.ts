import { describe, it, expect, beforeAll } from 'vitest';
import {
  type CreateMathProblemRequest,
  type LoginResponse,
  createUser,
  login,
  createMathProblem,
  deleteMathProblem,
  CreateMathProblemStepRequest,
  createMathProblemStep,
  getMathProblemStepById,
  GetMathProblemStepById200,
  updateMathProblemStep,
  deleteUser,
  deleteMathProblemStepsByProblemId,
} from 'web-api-client';
import { getApiConfig } from 'src/utils/getApiConfig';

const testEmail = `mathproblem_${Date.now()}@example.com`;
const testPassword = 'Test123!';
let accessToken: string;
let userId: number;
let mathProblemId: number;
let mathProblemStepId: number;
describe('MathProblemStepController – integration roundtrip', () => {
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

  it('should create a math problem step', async () => {
    const stepBody: CreateMathProblemStepRequest = {
      problemId: mathProblemId,
      step: {
        problemId: mathProblemId,
        stepIndex: 1,
        stepAfter: '2 + 2',
        stepBefore: '4',
        description: 'First step of the solution',
      },
    };

    const createdStep = await createMathProblemStep(stepBody, getApiConfig(false, accessToken));

    expect(createdStep).toBeDefined();
    expect(createdStep?.stepIndex).toBe(1);
    mathProblemStepId = createdStep?.id!;
  });

  it('should get math problem step by id', async () => {
    const fetchedStep: GetMathProblemStepById200 = await getMathProblemStepById(
      mathProblemStepId,
      {
        idType: 'id',
        problemId: mathProblemId,
      },
      getApiConfig(false, accessToken)
    );

    expect(fetchedStep).toBeDefined();
    expect(fetchedStep?.id).toBe(mathProblemStepId);
    expect(fetchedStep?.stepIndex).toBe(1);
    expect(fetchedStep?.problemId).toBe(mathProblemId);
  });

  it('should update math problem step', async () => {
    const updateBody = {
      id: mathProblemStepId,
      step: {
        stepIndex: 1,
        stepAfter: '3 + 3',
        stepBefore: '6',
        description: 'Updated step description',
      },
    };

    await updateMathProblemStep(updateBody, getApiConfig(false, accessToken));

    const updated: GetMathProblemStepById200 = await getMathProblemStepById(
      mathProblemStepId,
      {
        idType: 'id',
        problemId: mathProblemId,
      },
      getApiConfig(false, accessToken)
    );

    expect(updated).toBeDefined();
    expect(updated?.id).toBe(mathProblemStepId);
    expect(updated?.stepIndex).toBe(1);
    expect(updated?.problemId).toBe(mathProblemId);
    expect(updated?.description).toBe('Updated step description');
    expect(updated?.stepAfter).toBe('3 + 3');
    expect(updated?.stepBefore).toBe('6');
  });

  it('should delete math problem step', async () => {
    await deleteMathProblemStepsByProblemId(mathProblemId, getApiConfig(false, accessToken));

    await expect(
      getMathProblemStepById(
        mathProblemStepId,
        {
          idType: 'id',
          problemId: mathProblemId,
        },
        getApiConfig(false, accessToken)
      )
    ).rejects.toThrow();

    await deleteMathProblem(mathProblemId, getApiConfig(false, accessToken)); // Cleanup math problem after tests
    await deleteUser(userId, getApiConfig(false, accessToken)); // Cleanup user after tests
  });
});
