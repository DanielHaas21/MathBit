/**
 * Represents the data requiered to update a math problem
 */
export interface UpdateMathProblem {
  /**
   * Id of the math problem
   */
  id?: number;
  /**
   * Original input expression
   */
  originalExpression?: string;
  /**
   * Name of the math problem created when saving one
   */
  name?: string;
  /**
   * Description of the math problem
   */
  description?: string;
  /**
   * Final simplified expression
   */
  simplifiedExpression?: string;
  /**
   * Bookmarked check
   */
  bookmark?: boolean;
  /**
   * Last updated timestamp
   */
  updated?: Date;
}
