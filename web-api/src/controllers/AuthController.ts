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
  Get,
} from 'tsoa';
import jwt from 'jsonwebtoken';
import bcrypt from 'bcrypt';

import { type LoginRequest } from '../dto/LoginRequest';
import { type LoginResponse } from '../dto/LoginResponse';
import { getAppConfig } from '../appConfig';
import { UserRepository } from 'db';
import { type ExpressRequest } from '../types/expressRequest';
import { type UserProfile } from '../types/userProfile';
import { UserMapper } from '../mappers/UserMapper';
import { type User } from '../dto/User';

const ACCESS_TOKEN_EXPIRATION = '4h';
const REFRESH_TOKEN_EXPIRATION = '7d';

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
  public async login(
    @Body() body: LoginRequest,
    @Request() req: ExpressRequest
  ): Promise<LoginResponse> {
    const user = await this.userRepository.getUserByEmail(body.email);
    if (!user) {
      return { errorCode: 'Invalid credentials' };
    }

    const isValid = await bcrypt.compare(body.password, user.password);
    if (!isValid) {
      return { errorCode: 'Invalid credentials' };
    }

    const payload: UserProfile = {
      id: user.id,
      username: user.username,
      email: user.email,
      firstName: user.firstName,
      lastName: user.lastName,
    };
    const config = getAppConfig();

    const accessToken = jwt.sign(payload, config.app.secret, {
      expiresIn: ACCESS_TOKEN_EXPIRATION,
    });
    const refreshToken = jwt.sign(payload, config.app.secret, {
      expiresIn: REFRESH_TOKEN_EXPIRATION,
    });

    req.res?.cookie('refreshToken', refreshToken, {
      httpOnly: true,
      secure: false, // dev-only switch to secure in prod
      sameSite: 'lax', // dev-only switch to none in prod
      maxAge: 24 * 60 * 60 * 1000 * 7, // 7 days in ms (match REFRESH_TOKEN_EXPIRATION)
    });

    return { accessToken: accessToken, userProfile: payload };
  }

  @Post('refresh')
  @Response<Error>('400', 'Invalid refresh token')
  public async refresh(@Request() req: ExpressRequest) {
    const config = getAppConfig();

    const token = req.cookies?.refreshToken;

    if (!token) {
      this.setStatus(400);
      return { errorCode: 'No refresh token provided' };
    }

    try {
      const decoded = jwt.verify(token, config.app.secret);

      const { iat, exp, ...payload } = decoded as UserProfile & jwt.JwtPayload; // jwt replaces iat and exp by itself

      const newAccessToken = jwt.sign(payload, config.app.secret, {
        expiresIn: ACCESS_TOKEN_EXPIRATION,
      });

      return { accessToken: newAccessToken };
    } catch (err) {
      this.setStatus(400);
      console.log(err);
      return { errorCode: 'Invalid refresh token' };
    }
  }

  /**
   * Returns the currently authenticated user
   */
  @Get('me')
  @Security('jwt')
  public async me(@Request() request: ExpressRequest): Promise<User | null> {
    const profile = request.userProfile;

    if (!profile?.id) {
      this.setStatus(401);
      throw new Error('Unauthorized');
    }

    const user = await this.userRepository.getUserById(profile.id);

    if (!user) {
      this.setStatus(404);
      throw new Error('User not found');
    }

    return UserMapper.toDto(user);
  }

  @Post('logout')
  @Security('jwt')
  @SuccessResponse('200', 'Logout successful')
  public async logout(@Request() _req: ExpressRequest): Promise<void> {
    // Stateless JWT logout = client just deletes token
    return;
  }
}
