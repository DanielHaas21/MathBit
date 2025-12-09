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

  return (x: number) => {
    const evaluated = boxed
      .subs({ x: ce.number(x) })
      .evaluate()
      .N();

    const val = evaluated.re;
    return typeof val === 'number' ? val : NaN;
  };
}
