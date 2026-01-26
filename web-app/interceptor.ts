import axios, { AxiosError, AxiosRequestConfig } from 'axios';
import { refresh } from 'web-api-client';
import { store } from '@/store/store';
import { login as loginState, logout as logoutState } from '@/store/slices/UserState';
import getApiConfig from './apiConfig';

/**
 * This file sets up an axios instance with an interceptor to handle token refresh on 401 responses.
 *
 * This also:
 * - Ensures persistent login sessions by automatically refreshing access tokens when they expire or are deleted
 * - This axios instance is injected into web-api-client requests via apiConfig.ts
 */

const API_BASE = import.meta.env.VITE_API_URL as string;

export const api = axios.create({
  baseURL: API_BASE,
  withCredentials: true,
});
// track refresh in progress to avoid duplicate requests
let refreshPromise: Promise<string | null> | null = null;

async function performRefresh(): Promise<string | null> {
  if (refreshPromise) return refreshPromise;

  refreshPromise = (async () => {
    try {
      const response = await refresh(getApiConfig(false));

      const newToken = response?.accessToken;
      if (newToken) {
        // current user from store
        const currentUser = store.getState().User?.user ?? null;

        // update store and axios defaults
        store.dispatch(loginState({ accessToken: newToken, user: currentUser }));
        api.defaults.headers.common['Authorization'] = `Bearer ${newToken}`;
        return newToken;
      }
      return null;
    } catch (err) {
      console.error('Token refresh failed:', err);
      store.dispatch(logoutState());
      return null;
    } finally {
      refreshPromise = null;
    }
  })();

  return refreshPromise;
}

// if we get 401 = try refresh
api.interceptors.response.use(
  (res) => res,
  async (error: AxiosError) => {
    const originalRequest = error.config as AxiosRequestConfig & { _retry?: boolean };

    if (
      error.response?.status === 401 &&
      !originalRequest._retry &&
      !String(originalRequest.url).includes('/auth/login') &&
      !String(originalRequest.url).includes('/auth/refresh')
    ) {
      // we perform refresh and retry the failed original request; this will unnoticeably extend user sessions
      originalRequest._retry = true;

      const newToken = await performRefresh();
      if (newToken) {
        originalRequest.headers = originalRequest.headers || {};
        originalRequest.headers.Authorization = `Bearer ${newToken}`;
        return api(originalRequest);
      }
    }

    return Promise.reject(error);
  }
);

export default api;
