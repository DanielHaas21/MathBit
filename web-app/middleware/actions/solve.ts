import { solveMathExpression } from 'web-api-client';
import { ComputeEngine, type Expression } from '@cortex-js/compute-engine';

async function solve(expr: string) {
  const response = await solveMathExpression({ rawExpression: expr });
  return response;
}

export default solve;
