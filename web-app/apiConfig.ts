import { LoginMutationRequest, RequestConfig } from 'web-api-client';
import api from './interceptor';
import { store } from './store/store';

/**
 * Api config for web-api-client requests, use only on auth-dependent requests
 * @param {boolean} auth - Whether to include Authorization headers with Bearer token and API interceptors, defaults to false
 * @returns {Partial<RequestConfig<LoginMutationRequest>>} Api config object; typed as that since web-api-client expects its own type, which either doesnt contain all fields or contains some discrepencies with actual axios types yet it accepts them
 */
function getApiConfig(auth: boolean = false): Partial<RequestConfig<LoginMutationRequest>> {
  const token = store.getState().User.accessToken;

  const config = (auth
    ? {
        baseURL: import.meta.env.VITE_API_URL || 'http://localhost:4001',
        /**
         * Allows cross-origin requests to include credentials such as cookies
         */
        withCredentials: true,
      }
    : {
        /**
         * Base URL for API requests
         */
        baseURL: import.meta.env.VITE_API_URL || 'http://localhost:4001',
        /**
         * Allows cross-origin requests to include credentials such as cookies
         */
        withCredentials: true,
        /**
         * Authorization header with Bearer token if available
         */
        headers: token ? { Authorization: `Bearer ${token}` } : {},
        /**
         * Custom axios instance to be used for requests
         */
        client: api,
      }) as unknown as Partial<RequestConfig<LoginMutationRequest>>; // type has to be asserted since unkown conversions cant be annotated directly

  return config;
}

export default getApiConfig;
