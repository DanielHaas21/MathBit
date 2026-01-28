import { User as UserModel } from 'db';
import { User } from '../dto/User';
import { CreateUser } from '../dto/CreateUser';

export class UserMapper {
  static toDto(model: UserModel): User | null {
    if (!model) {
      return null;
    }
    return {
      id: model.id,
      username: model.username,
      email: model.email,
      created: model.created,
    };
  }

  static toCreateModel(dto: CreateUser): UserModel | null {
    if (!dto) {
      return null;
    }
    return {
      username: dto.username,
      email: dto.email,
      password: dto.password,
    };
  }
}
