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
  /*
   * Expression before the step
   */
  stepBefore?: string;
  /*
   * Expression after the step
   */
  stepAfter?: string;
}
