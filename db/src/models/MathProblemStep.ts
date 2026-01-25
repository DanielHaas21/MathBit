export interface MathProblemStep {
  id?: number;
  problemId: number;
  stepIndex: number;
  description: string;
  stepBefore?: string;
  stepAfter?: string;
  created?: Date;
}
