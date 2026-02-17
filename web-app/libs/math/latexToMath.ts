import { ComputeEngine, Expression } from '@cortex-js/compute-engine';

/**
 * This function converts LaTeX to a MathJSON AST
 * @param latex
 * @returns
 */
export function latexToMathJson(latex: string): Expression | null {
  try {
    const ce = new ComputeEngine();
    const expr = ce.parse(latex);
    return expr.json;
  } catch (e) {
    return null;
  }
}
