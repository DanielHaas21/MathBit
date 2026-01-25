import { CreateMathProblem } from '../dto/CreateMathProblem';
import { MathProblem } from '../dto/MathProblem';
import { UpdateMathProblem } from '../dto/UpdateMathProblem';
import { MathProblem as MathProblemModel } from 'db';

export class MathProblemMapper {
  static toDto(model: MathProblemModel): MathProblem | null {
    if (!model) {
      return null;
    }
    return {
      id: model.id,
      userId: model.userId,
      originalExpression: model.originalExpression,
      description: model.description,
      name: model.name,
      simplifiedExpression: model.simplifiedExpression,
      bookmark: model.bookmark,
      created: model.created,
      updated: model.updated,
    };
  }

  static toCreateModel(dto: CreateMathProblem): Partial<MathProblemModel> | null {
    if (!dto) {
      return null;
    }
    return {
      originalExpression: dto.originalExpression,
      description: dto.description,
      name: dto.name,
      simplifiedExpression: dto.simplifiedExpression,
      bookmark: dto.bookmark,
    };
  }

  static toUpdateModel(dto: UpdateMathProblem): Partial<MathProblemModel> | null {
    if (!dto) {
      return null;
    }
    return {
      id: dto.id,
      originalExpression: dto.originalExpression,
      description: dto.description,
      name: dto.name,
      simplifiedExpression: dto.simplifiedExpression,
      bookmark: dto.bookmark,
      updated: dto.updated,
    };
  }
}
