export interface MathProblem {
  id?: number;
  userId: number;
  originalExpression: string;
  latex?: string;
  name?: string;
  simplifiedExpression?: string;
  bookmark?: boolean;
  created?: Date;
  updated?: Date;
}
