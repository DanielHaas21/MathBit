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
      stepBefore: model.stepBefore,
      stepAfter: model.stepAfter,
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
      stepBefore: dto.stepBefore,
      stepAfter: dto.stepAfter,
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
      stepBefore: dto.stepBefore,
      stepAfter: dto.stepAfter,
    };
  }
}
