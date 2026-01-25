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
  /*
   * Expression before the step
   */
  stepBefore?: string;
  /*
   * Expression after the step
   */
  stepAfter?: string;
}
