import { solveMathExpression, MathEngineSolveResponse } from 'web-api-client';
import { ComputeEngine, type Expression } from '@cortex-js/compute-engine';

async function solve(expr: string): Promise<MathEngineSolveResponse> {
  const ce = new ComputeEngine();
  
  if (JSON.stringify(ce.parse(expr).json).includes('Error')) {
    throw new Error('Invalid expression');
  }
  

  const response = await solveMathExpression({ rawExpression: expr });
  return response;
}

export default solve;
