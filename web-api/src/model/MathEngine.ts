/**
 * Represents the request payload for solving a math expression.
 */
export interface MathEngineSolveRequest {
  /**
   * Raw LaTeX expression
   */
  rawExpression: string;
}

/**
 * Represents the respons for solving a math expression.
 */
export interface MathEngineSolveResponse {
  /**
   * Final LaTeX expression
   */
  finalExpression: string;
}
