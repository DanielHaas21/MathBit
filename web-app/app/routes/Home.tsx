import { createMathProblem, getAllUsers, refresh } from 'web-api-client';
import login from '../../middleware/auth/login';
import { useEffect } from 'react';
import getApiConfig from '@/apiConfig';
import api from '@/interceptor';

export default function Home() {
  // useEffect(() => {
  //   async function d() {
  //     const auth = await login({ email: 'e.procházková4@example.com', password: 'test' });
  //   }

  //   d();
  // }, []);

  const test = async () => {
    async function testRefresh() {
      try {
        const response = await refresh(getApiConfig());

        console.log('Refresh response:', response);
      } catch (err) {
        console.error('Refresh request failed:', err);
      }
    }

    testRefresh();
  };
  const test2 = async () => {
    console.log(await getAllUsers(getApiConfig()));
  };
  return (
    <>
      <button onClick={test}>test ref</button>
      <button onClick={test2}>test users</button>
    </>
  );
}
