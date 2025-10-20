import { store } from './store/store';

/**
 * Api config for web-api-client requests, use only on auth-dependent requests
 * @returns {any} Api config object; typed as any since web-api-client expects its own type, which doesnt contain all fields
 */
function getApiConfig(): any {
  const token = store.getState().User.accessToken;

  return {
    baseURL: import.meta.env.VITE_API_URL || 'http://localhost:4001',
    withCredentials: true,
    headers: token ? { Authorization: `Bearer ${token}` } : {},
  };
}
export default getApiConfig;
