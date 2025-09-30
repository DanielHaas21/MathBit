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
      firstName: model.firstName,
      lastName: model.lastName,
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
      firstName: dto.firstName,
      lastName: dto.lastName,
      email: dto.email,
      password: dto.password,
    } as UserModel;
  }
}
