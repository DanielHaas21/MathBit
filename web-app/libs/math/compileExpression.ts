import { ComputeEngine } from '@cortex-js/compute-engine';

/**
 * Compiles a MathJSON expression into a JavaScript function of one variable x
 * First the expression is boxed, then x is substituted and evaluated numerically
 * @param jsonExpr
 * @returns Real number or NaN if evaluation fails
 */

export function compileExpression(jsonExpr: any): (x: number) => number {
  const ce = new ComputeEngine();

  const boxed = ce.box(jsonExpr);
  const fn = boxed.compile(); //  IMPORTANT

  return (x: number) => {
    try {
      const y = fn({ x });
      return typeof y === 'number' && Number.isFinite(y) ? y : NaN;
    } catch {
      return NaN;
    }
  };
}
