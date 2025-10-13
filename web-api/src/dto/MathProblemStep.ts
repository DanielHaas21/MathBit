/**
 * Represents a Math problem step in the system
 */
export interface MathProblemStep {
  /**
   * Id of the math problem step
   */
  id?: number;
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
  /**
   * Created timestamp
   */
  created?: Date;
}
