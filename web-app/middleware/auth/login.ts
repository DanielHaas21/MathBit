import { store } from '@/store/store';
import { type LoginResponse, type LoginRequest, login as loginAPI } from 'web-api-client';
import { login as loginState } from '@/store/slices/UserState';

async function login(args: LoginRequest): Promise<boolean | { errorCode?: string } | undefined> {
  try {
    const response: LoginResponse = await loginAPI(args, {
      baseURL: import.meta.env.VITE_API_URL,
      withCredentials: true,
    } as any); // Typescript somehow complains, yet the refreshToken cookie is set correctly

    if (!response.errorCode && response.accessToken && response.userProfile) {
      const userProfile = response.userProfile;

      store.dispatch(
        loginState({
          accessToken: response.accessToken,
          user: {
            id: userProfile.id,
            firstName: userProfile.firstName,
            lastName: userProfile.lastName,
            email: userProfile.email,
            username: userProfile.username,
          },
        })
      );

      return true;
    } else {
      return { errorCode: response.errorCode };
    }
  } catch (error) {
    throw new Error('Login failed', error as ErrorOptions);
  }
}

export default login;
