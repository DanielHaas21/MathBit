import {
  solveMathExpression,
  MathEngineSolveResponse,
  SolveMathExpression400,
  SolveMathExpression200,
} from 'web-api-client';
import { ComputeEngine } from '@cortex-js/compute-engine';
import getApiConfig from '@/apiConfig';

async function solve(expr: string): Promise<MathEngineSolveResponse> {
  const ce = new ComputeEngine();

  if (expr.trim() === '') {
    throw new Error('empty');
  }

  if (JSON.stringify(ce.parse(expr).json).includes('Error')) {
    throw new Error('invalid');
  }

  const MathJSON = ce.parse(expr).json;
  try {
    const response: SolveMathExpression200 | SolveMathExpression400 = await solveMathExpression(
      { rawExpression: MathJSON },
      getApiConfig()
    );
    return response;
  } catch (error) {
    if (error instanceof Error && !error.message.includes('Network Error')) {
      throw new Error(`unkown`);
    } else {
      throw new Error('network');
    }
  }
}

export default solve;
