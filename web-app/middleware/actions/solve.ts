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

  const MathJSONe = ce.parse(expr).json;
  console.log('Parsed MathJSON:', MathJSONe);

  if (JSON.stringify(ce.parse(expr).json).includes('Error')) {
    throw new Error('Invalid expression');
  }

  const MathJSON = ce.parse(expr).json;
  console.log('Parsed MathJSON:', MathJSON);
  const response = await solveMathExpression(
    { rawExpression: MathJSON },
    getApiConfig() // Non-auth requests seem to have a different config type, on function level its the same, hence why we cast it
  );
  console.log('Raw response from solveMathExpression:', response);

  return response;
}

export default solve;
