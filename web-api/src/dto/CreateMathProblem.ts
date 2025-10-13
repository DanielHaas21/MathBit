/**
 * Represents the data requiered to create a new math problem
 */
export interface CreateMathProblem {
  /**
   * Original input expression
   */
  originalExpression: string;
  /**
   * Name of the math problem created when saving one
   */
  name?: string;
  /**
   * Saved LaTeX epxression
   */
  latex?: string;
  /**
   * Final simplified expression
   */
  simplifiedExpression?: string;
  /**
   * Bookmarked check
   */
  bookmark?: boolean;
}
