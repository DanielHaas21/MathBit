/**
 * Represents the data requiered to update a math problem step
 */
export interface UpdateMathProblemStep {
  /**
   * Id of the math problem step
   */
  id?: number;
  /**
   * Index of the step
   */
  stepIndex: number;
  /**
   * Step description
   */
  description: string;
  /**
   * Expression of the step
   */
  expression: string;
  /**
   * Saved LaTeX epxression
   */
  latex?: string;
}
