/**
 * Represents the data requiered to create a math problem step
 */
export interface CreateMathProblemStep {
  /**
   * Id of the problem
   */
  problemId: number;
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
