import {
  solveMathExpression,
  MathEngineSolveResponse,
  SolveMathExpression400,
  SolveMathExpressionMutationResponse,
  SolveMathExpression200,
} from 'web-api-client';
import { ComputeEngine } from '@cortex-js/compute-engine';
import getApiConfig from '@/apiConfig';

async function solve(expr: string): Promise<MathEngineSolveResponse> {
  const ce = new ComputeEngine();

  if (expr.trim() === '') {
    throw new Error('Failed to solve expression: Expression cannot be empty');
  }

  if (JSON.stringify(ce.parse(expr).json).includes('Error')) {
    throw new Error('Failed to solve expression: Invalid expression');
  }

  const MathJSON = ce.parse(expr).json;
  try {
    const response: SolveMathExpression200 | SolveMathExpression400 = await solveMathExpression(
      { rawExpression: MathJSON },
      getApiConfig()
    );
    return response;
  } catch (error) {
    console.log('Error solving expression:', error);
    if (error instanceof Error && !error.message.includes('Network Error')) {
      throw new Error(`Failed to solve expression: "Unknown symbols used"`);
    } else {
      throw new Error('Failed to solve expression: There has been a problem with the request');
    }
  }
}

export default solve;
