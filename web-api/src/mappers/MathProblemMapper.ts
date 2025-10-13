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
      latex: model.latex,
      simplifiedExpression: model.simplifiedExpression,
      bookmark: model.bookmark,
      created: model.created,
      updated: model.updated,
    };
  }

  static toCreateModel(dto: CreateMathProblem): MathProblemModel | null {
    if (!dto) {
      return null;
    }
    return {
      originalExpression: dto.originalExpression,
      latex: dto.latex,
      simplifiedExpression: dto.simplifiedExpression,
      bookmark: dto.bookmark,
    } as MathProblemModel;
  }

  static toUpdateModel(dto: UpdateMathProblem): MathProblemModel | null {
    if (!dto) {
      return null;
    }
    return {
      id: dto.id,
      originalExpression: dto.originalExpression,
      latex: dto.latex,
      simplifiedExpression: dto.simplifiedExpression,
      bookmark: dto.bookmark,
      updated: dto.updated,
    } as MathProblemModel;
  }
}
