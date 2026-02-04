import { useEffect } from 'react';
import { Outlet, Link } from 'react-router-dom';
import { me, refresh } from 'web-api-client';
import { useDispatch } from 'react-redux';
import { AppDispatch } from '@/store/store';
import getApiConfig from '@/apiConfig';
import { login, logout } from '@/store/slices/UserState';
export default function Layout() {
  // Redux dispatch
  const Dispatch = useDispatch<AppDispatch>();

  // On mount, attempt to refresh token and fetch user info
  useEffect(() => {
    const refreshToken = async () => {
      try {
        const accessToken = await refresh(getApiConfig(true));
 
        const user = await me(getApiConfig(false));

        // Update Redux store with new token and user info
        Dispatch(
          login({ accessToken: accessToken.accessToken, user: user, authStatus: 'authenticated' })
        );
      } catch (error) {
        Dispatch(logout());
        console.error('Error refreshing token:', error);
      } finally {
      }
    };
    refreshToken();
  }, []);
  return (
    <div>
      <Outlet />
    </div>
  );
}
