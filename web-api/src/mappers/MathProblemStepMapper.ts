import { CreateMathProblemStep } from '../dto/CreateMathProblemStep';
import { MathProblemStep } from '../dto/MathProblemStep';
import { UpdateMathProblemStep } from '../dto/UpdateMathProblemStep';
import { MathProblemStep as MathProblemStepModel } from 'db';

export class MathProblemStepMapper {
  static toDto(model: MathProblemStepModel): MathProblemStep | null {
    if (!model) {
      return null;
    }
    return {
      id: model.id,
      problemId: model.problemId,
      stepIndex: model.stepIndex,
      description: model.description,
      expression: model.expression,
      latex: model.latex,
      created: model.created,
    };
  }

  static toCreateModel(dto: CreateMathProblemStep): MathProblemStepModel | null {
    if (!dto) {
      return null;
    }
    return {
      problemId: dto.problemId,
      stepIndex: dto.stepIndex,
      description: dto.description,
      expression: dto.expression,
      latex: dto.latex,
    };
  }

  static toUpdateModel(dto: UpdateMathProblemStep): Partial<MathProblemStepModel> | null {
    if (!dto) {
      return null;
    }
    return {
      id: dto.id,
      stepIndex: dto.stepIndex,
      description: dto.description,
      expression: dto.expression,
      latex: dto.latex,
    };
  }
}
