import { ComputeEngine } from '@cortex-js/compute-engine';

/**
 * Evalatues a **purely** numerical expression
 * @param latex 
 * @returns 
 */
export function evaluateLatexNumeric(latex: string): number | null {
  try {
    const ce = new ComputeEngine();

    const expr = ce.parse(latex);
    const evaluated = expr.evaluate();

    // If fully numeric, value is available
    const value = evaluated.re;

    return typeof value === 'number' && Number.isFinite(value)
      ? value
      : null;
  } catch (e) {
    console.error('Latex evaluation error', e);
    return null;
  }
}