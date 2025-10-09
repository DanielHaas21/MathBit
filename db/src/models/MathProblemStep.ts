export interface MathProblemStep {
  id?: number;
  problemId: number;
  stepIndex: number;
  description: string;
  expression: string;
  latex?: string;
  created?: Date;
}
