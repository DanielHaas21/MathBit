import * as dotenv from 'dotenv';

dotenv.config();

/**
 * Api config for web-api-client requests, use only on auth-dependent requests
 * This is a test-used version of the one in web-app/src/apiConfig.ts
 * Which is not used in tests since it imports store and interceptors which are not needed for testing and would cause issues due to circular dependencies with the test files
 * This version also doesnt use the interceptor
 * @param {boolean} auth - Whether to include Authorization headers with Bearer token and API interceptors, defaults to false
 * @returns {Partial<RequestConfig<LoginMutationRequest>>} Api config object; typed as that since web-api-client expects its own type, which either doesnt contain all fields or contains some discrepencies with actual axios types yet it accepts them
 */
function getApiConfig<T = unknown>(auth: boolean = false, token?: string): T {
  const config = (auth
    ? {
        baseURL: process.env.WEB_API_URL || 'http://localhost:4001',
        /**
         * Allows cross-origin requests to include credentials such as cookies
         */
        withCredentials: true,
      }
    : {
        /**
         * Base URL for API requests
         */
        baseURL: process.env.WEB_API_URL || 'http://localhost:4001',
        /**
         * Allows cross-origin requests to include credentials such as cookies
         */
        withCredentials: true,
        /**
         * Authorization header with Bearer token if available
         */
        headers: token ? { Authorization: `Bearer ${token}` } : {},
      }) as unknown as T; // type has to be asserted since unkown conversions cant be annotated directly

  return config;
}

export default getApiConfig;
