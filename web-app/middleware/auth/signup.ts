import { store } from '@/store/store';
import {
  type LoginResponse,
  type LoginRequest,
  login as loginAPI,
  CreateUser,
  createUser,
  CreateUser201,
} from 'web-api-client';
import { login as loginState } from '@/store/slices/UserState';
import getApiConfig from '@/apiConfig';
import { ActionResult } from '../types/ActionResult';

async function signup(args: CreateUser): Promise<ActionResult> {
  try {
    const user: CreateUser201 = await createUser(args, getApiConfig(true));

    if (user && user.id) {
      // Auto-login after successful signup
      const loginArgs: LoginRequest = {
        email: args.email,
        password: args.password,
      };
      const response: LoginResponse = await loginAPI(loginArgs, getApiConfig(false));

      if (!response.errorCode && response.accessToken && response.userProfile) {
        const userProfile = response.userProfile;

        store.dispatch(
          loginState({
            accessToken: response.accessToken,
            user: {
              id: userProfile.id,
              email: userProfile.email,
              username: userProfile.username,
            },
          })
        );

        return { ok: true };
      } else {
        return { ok: false, errorCode: response.errorCode ?? 'UNKNOWN_ERROR' };
      }
    }
    return { ok: false, errorCode: 'SIGNUP_FAILED' };
  } catch (error) {
    throw new Error('Login failed', error as ErrorOptions);
  }
}

export default signup;
