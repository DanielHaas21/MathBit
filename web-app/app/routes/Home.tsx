
import { useEffect } from 'react';
import { login } from 'web-api-client';

export default function Home() {
  useEffect(() => {
    async function d() {
      console.log(
        await login(
          { email: 'e.procházková4@example.com', password: 'test' },
          { baseURL: 'http://localhost:3001' }
        )
      );
    }

    d();
  }, []);

  return <h1>Home Page</h1>;
}
