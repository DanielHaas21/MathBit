import { describe, it, expect, beforeAll } from 'vitest';
import { login, refresh, me, logout, type LoginResponse, Refresh200 } from 'web-api-client';
import * as dotenv from 'dotenv';
import axios from 'axios';
import { CookieJar } from 'tough-cookie';
import { wrapper } from 'axios-cookiejar-support';
import { getApiConfig } from 'src/utils/getApiConfig';

dotenv.config();

let accessToken: string;
const testEmail: string = process.env.TEST_USER_EMAIL ?? 'test.email@com';
const testPassword: string = process.env.TEST_USER_PASSWORD ?? 'Password';

describe('Auth integration', () => {
  it('logs in and returns access token + cookie', async () => {
    const response: LoginResponse = await login(
      {
        email: testEmail, 
        password: testPassword, 
      },
      getApiConfig(false, accessToken) 
    );

    expect(response.accessToken).toBeDefined();
    expect(response.userProfile?.email).toBe(testEmail);

    accessToken = response.accessToken!;
  });

  it('returns current user via /me', async () => {
    const user = await me(
      getApiConfig(false, accessToken) // Bearer token here
    );

    expect(user).not.toBeNull();
    expect(user?.email).toBe(testEmail);
  });

  it('refreshes access token using refresh cookie', async () => {
    const response: Refresh200 = await refresh(
      getApiConfig(false, accessToken) // cookie is sent automatically
    );

    expect(response.accessToken).toBeDefined();
    accessToken = response.accessToken!;
  });

  it('logs out and clears refresh cookie', async () => {
    await logout(getApiConfig(false, accessToken));

    // refresh should now fail
    await expect(refresh(getApiConfig(false, accessToken))).rejects.toBeDefined();
  });
});
