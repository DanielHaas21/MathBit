import { CreateMathProblemStep } from '../CreateMathProblemStep';
import { UpdateMathProblemStep } from '../UpdateMathProblemStep';

export interface CreateMathProblemStepRequest {
  problemId: number;
  step: CreateMathProblemStep;
}

export interface UpdateMathProblemStepRequest {
  id: number;
  step: UpdateMathProblemStep;
}
