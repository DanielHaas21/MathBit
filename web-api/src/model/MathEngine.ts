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
 * Represents a single step in the solving process of a math expression.
 */
export interface MathEngineSolveStep {
  /*
   * Expression step
   */
  expressionStep: String;
  /*
   * Explanation of the step
   */
  explanationStep: String;
}

/**
 * Represents the respons for solving a math expression.
 */
export interface MathEngineSolveResponse {
  /**
   * Final LaTeX expression
   */
  finalExpression: string;
  /**
   * Steps taken to solve the expression
   */
  steps: MathEngineSolveStep[];
}
