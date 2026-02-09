import axios, { AxiosRequestConfig } from 'axios';
import { CookieJar } from 'tough-cookie';
import { wrapper } from 'axios-cookiejar-support';
import * as dotenv from 'dotenv';

dotenv.config();

const baseURL = process.env.WEB_API_URL;

// Set up an axios instance with cookie jar support for handling refresh tokens via cookies in tests
// Since the refresh token is stored in an HttpOnly cookie, we need to use a cookie jar to persist it across requests in our tests.
// This allows us to test the full authentication flow, including token refresh, as it would work in a real client environment.
// The kubbClient is a wrapper around this axios instance that we can pass to web-api-client functions to ensure they use the same cookie jar for authentication.
const jar = new CookieJar();
const axiosClient = wrapper(
  axios.create({
    baseURL,
    withCredentials: true,
  })
);
axiosClient.defaults.jar = jar;

// Kubb-generated client functions accept a `client` override.
// Their default client is NOT the global `axios` instance, so our CookieJar
// must be used via this custom client.
const kubbClient = <TData = unknown, TError = unknown, TVariables = unknown>(
  config: AxiosRequestConfig
) => axiosClient.request(config);

/**
 * Api config for web-api-client requests, use only on auth-dependent requests
 * This is a test-used version of the one in web-app/src/apiConfig.ts
 * Which is not used in tests since it imports store and interceptors which are not needed for testing and would cause issues due to circular dependencies with the test files
 * This version also doesnt use the interceptor
 * @param {boolean} auth - Whether to include Authorization headers with Bearer token and API interceptors, defaults to false
 * @returns {Partial<RequestConfig<LoginMutationRequest>>} Api config object; typed as that since web-api-client expects its own type, which either doesnt contain all fields or contains some discrepencies with actual axios types yet it accepts them
 */
export function getApiConfig<T = unknown>(auth: boolean = false, token?: string): T {
  const config = (auth
    ? {
        baseURL,
        /**
         * Allows cross-origin requests to include credentials such as cookies
         */
        withCredentials: true,
        client: kubbClient,
      }
    : {
        /**
         * Base URL for API requests
         */
        baseURL,
        /**
         * Allows cross-origin requests to include credentials such as cookies
         */
        withCredentials: true,

        /**
         * Special client function that uses the axios instance with cookie jar support, ensuring that cookies (like refresh tokens) are properly handled in tests. This is necessary for testing authenticated endpoints that rely on cookies for session management.
         * Note: This client is only necessary for authenticated requests that require cookie handling. For unauthenticated requests, the default client can be used without issues.
         */
        client: kubbClient,
        /**
         * Authorization header with Bearer token if available
         */
        headers: token ? { Authorization: `Bearer ${token}` } : {},
      }) as unknown as T; // type has to be asserted since unkown conversions cant be annotated directly

  return config;
}
