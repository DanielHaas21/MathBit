import api from './interceptor';
import { store } from './store/store';

/**
 * Api config for web-api-client requests, use only on auth-dependent requests
 * @returns {any} Api config object; typed as any since web-api-client expects its own type, which either doesnt contain all fields or contains some discrepencies with axios types
 */
function getApiConfig(): any {
  const token = store.getState().User.accessToken;

  return {
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
  };
}
export default getApiConfig;
