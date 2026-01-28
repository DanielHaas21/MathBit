import { store } from '@/store/store';
import { type LoginResponse, type LoginRequest, login as loginAPI } from 'web-api-client';
import { login as loginState } from '@/store/slices/UserState';
import getApiConfig from '@/apiConfig';

type LoginResult = { ok: true } | { ok: false; errorCode: string };

async function login(args: LoginRequest): Promise<LoginResult> {
  try {
    const response: LoginResponse = await loginAPI(args, getApiConfig(false));

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
  } catch (error) {
    throw new Error('Login failed', error as ErrorOptions);
  }
}

export default login;
