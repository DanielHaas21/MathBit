import {
  Controller,
  Post,
  Route,
  Body,
  Response,
  SuccessResponse,
  Tags,
  Request,
  Security,
} from 'tsoa';
import jwt from 'jsonwebtoken';
import bcrypt from 'bcrypt';

import { type LoginRequest } from '../dto/LoginRequest';
import { type LoginResponse } from '../dto/LoginResponse';
import { getAppConfig } from '../appConfig';
import { UserRepository } from 'db';
import { type ExpressRequest } from '../types/expressRequest';
import { UserProfile } from '@/types/userProfile';

const JWT_EXPIRATION = '24h';

@Route('auth')
@Tags('Auth')
export class AuthController extends Controller {
  private userRepository: UserRepository;

  constructor() {
    super();
    this.userRepository = new UserRepository();
  }

  @Post('login')
  @SuccessResponse('200', 'Login successful')
  @Response<Error>('400', 'Invalid username or password')
  public async login(@Body() body: LoginRequest): Promise<LoginResponse> {
    const user = await this.userRepository.getUserByEmail(body.email);
    if (!user) {
      return { errorCode: 'Invalid credentials' };
    }

    const isValid = await bcrypt.compare(body.password, user.password);
    if (!isValid) {
      return { errorCode: 'Invalid credentials' };
    }

    const payload: UserProfile = { id: user.id, username: user.username, email: user.email };
    const config = getAppConfig();
    const token = jwt.sign(payload, config.app.secret, { expiresIn: JWT_EXPIRATION });

    return { token };
  }

  @Post('logout')
  @Security('jwt')
  @SuccessResponse('200', 'Logout successful')
  public async logout(@Request() _req: ExpressRequest): Promise<void> {
    // Stateless JWT logout = client just deletes token
    return;
  }
}
