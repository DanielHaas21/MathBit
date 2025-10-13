import { CreateMathProblem } from '../CreateMathProblem';
import { UpdateMathProblem } from '../UpdateMathProblem';

export interface CreateMathProblemRequest {
  userId: number;
  problem: CreateMathProblem;
}

export interface UpdateMathProblemRequest {
  id: number;
  problem: UpdateMathProblem;
}
