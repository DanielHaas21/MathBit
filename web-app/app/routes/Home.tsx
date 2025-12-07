import { createMathProblem, getAllUsers, refresh } from 'web-api-client';
import login from '../../middleware/auth/login';
import { useEffect, useRef, useState } from 'react';
import getApiConfig from '@/apiConfig';
import api from '@/interceptor';
import MathInput, { KeyProps } from 'react-math-keyboard';
import { MathJax, MathJaxContext } from 'better-react-mathjax';
import { Button, InputBase, MathField } from '@/libs/ui/components';


export default function Home() {
  // useEffect(() => {
  //   async function d() {
  //     const auth = await login({ email: 'e.procházková4@example.com', password: 'test' });
  //   }

  //   d();
  // }, []);

  const [latex, setLatex] = useState('');
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

  return (
    <>
      <button onClick={test}>test ref</button>
      <Button >test users</Button>

      <MathField initialLatex={latex} onChange={(newLatex) => setLatex(newLatex)} />
      <MathJaxContext>
        <MathJax dynamic>{`\\(${latex}\\)`}</MathJax>
      </MathJaxContext>
      <p>{latex}</p>
    </>
  );
}
