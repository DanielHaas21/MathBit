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
  /*
   * Expression before the step
   */
  stepBefore?: string;
  /*
   * Expression after the step
   */
  stepAfter?: string;
  /**
   * Created timestamp
   */
  created?: Date;
}
