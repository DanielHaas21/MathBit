/**
 * Represents the request payload for solving a math expression.
 */
export interface MathEngineSolveRequest {
  /**
   * Raw LaTeX expression, typed as unknown since NodeNext doesn't allow runtime imports of ESM modules, which cortex is, otherwise it would be typed as Expression from @cortex-js/compute-engine
   */
  rawExpression: unknown;
}
/**
 * Represents a single step in the solving process of a math expression.
 */
export interface MathEngineSolveStep {
  /*
   * Expression step
   */
  stepBefore: string;
  /*
   * Expression step
   */
  stepAfter: string;
  /*
   * Explanation of the step
   */
  stepRuleDescription: string;
}

/**
 * Represents the response for solving a math expression.
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

/**
 * Error shape returned by the Haskell engine when it rejects the request.
 */
export interface MathEngineErrorResponse {
  errHTTPCode: number;
  errReason: string;
  errMessage: string;
}
