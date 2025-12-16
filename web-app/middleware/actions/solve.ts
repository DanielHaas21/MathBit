import {
  solveMathExpression,
  MathEngineSolveResponse,
  SolveMathExpressionMutationRequest,
  RequestConfig,
} from 'web-api-client';
import { ComputeEngine, type Expression } from '@cortex-js/compute-engine';
import getApiConfig from '@/apiConfig';

async function solve(expr: string): Promise<MathEngineSolveResponse> {
  const ce = new ComputeEngine();
  if (expr.trim() === '') {
    throw new Error('Expression cannot be empty');
  }

  if (JSON.stringify(ce.parse(expr).json).includes('Error')) {
    throw new Error('Invalid expression');
  }

  const MathJSON = ce.parse(expr).json;
  const response = await solveMathExpression(
    { rawExpression: MathJSON },
    getApiConfig() as Partial<RequestConfig<SolveMathExpressionMutationRequest>> // Non-auth requests seem to have a different config type, on function level its the same, hence why we cast it 
  );

  return response;
}

export default solve;
