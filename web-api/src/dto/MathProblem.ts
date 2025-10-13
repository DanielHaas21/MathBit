/**
 * Represents a Math problem in the system
 */
export interface MathProblem {
  /**
   * Id of the math problem
   */
  id?: number;
  /**
   * Id of the user
   */
  userId: number;
  /**
   * Name of the math problem created when saving one
   */
  name?: string;
  /**
   * original input expression
   */
  originalExpression: string;
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
  /**
   * Created timestamp
   */
  created?: Date;
  /**
   * Last updated timestamp
   */
  updated?: Date;
}
