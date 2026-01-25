export interface MathProblem {
  id?: number;
  userId: number;
  originalExpression: string;
  name?: string;
  description?: string;
  simplifiedExpression?: string;
  bookmark?: boolean;
  created?: Date;
  updated?: Date;
}
