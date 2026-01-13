import { createMathProblem, getAllUsers, MathEngineSolveStep, refresh } from 'web-api-client';
import login from '../../middleware/auth/login';
import { useEffect, useRef, useState } from 'react';
import getApiConfig from '@/apiConfig';
import { MathJax, MathJaxContext } from 'better-react-mathjax';
import {
  Button,
  ClipBoardCopy,
  FunctionPlot,
  InputBase,
  KeyValuePair,
  MathField,
} from '@/libs/ui/components';
import { ComputeEngine } from '@cortex-js/compute-engine';
import { Paper } from '@/libs/ui/layouts';
import { latexToMathJson } from '@/libs/math';
import { evaluateLatexNumeric } from '@/libs/math/evaluateExpression';
import solve from '@/middleware/actions/solve';

export default function Home() {
  // useEffect(() => {
  //   async function d() {
  //     const auth = await login({ email: 'e.procházková4@example.com', password: 'test' });
  //   }

  //   d();
  // }, []);
  const [latex, setLatex] = useState('');
  const [final, setFinal] = useState('');
  const [finalSteps, setFinalSteps] = useState<MathEngineSolveStep[]>([]);
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
    // console.log(await getAllUsers(getApiConfig()));

    console.log(latexToMathJson(latex));
    const solved = await solve(latex);
    setFinal(solved.finalExpression);
    setFinalSteps(solved.steps);
    console.log(solved);
  };

  const renderedSteps =
    finalSteps &&
    finalSteps.map((m, i) => (
      <>
        <h3>Step {i + 1}:</h3>
        <MathJaxContext>
          <MathJax>{`Before: \\(${m.stepBefore}\\)`}</MathJax>
        </MathJaxContext>
        <MathJaxContext>
          <MathJax>{`After: \\(${m.stepAfter}\\)`}</MathJax>
        </MathJaxContext>
        <p>Desc: {m.stepRuleDescription}</p>
      </>
    ));

  return (
    <div className="w-full flex justify-start items-center flex-col h-fit">
      <div>
        <Button onClick={test}>test ref/auth</Button>
        <Button onClick={test2}>test math</Button>
      </div>

      <Paper thickness="sm" className="border border-white-800 w-[70%]">
        <Paper.Title>Math Input Section</Paper.Title>
        <Paper.Content className="grid grid-cols-1 xl:grid-cols-2">
          <MathField initialLatex={latex} onChange={(newLatex) => setLatex(newLatex)} />

          <div className="fle gap-2 flex-col w-full">
            <KeyValuePair
              orientation={'horizontal'}
              label="LaTeX format:"
              value={
                <p>
                  {latex} {latex && <ClipBoardCopy text={latex}></ClipBoardCopy>}
                </p>
              }
            ></KeyValuePair>
            <KeyValuePair
              orientation={'horizontal'}
              label="Plain Math:"
              value={
                <MathJaxContext>
                  <MathJax>{`\\(${latex}\\)`}</MathJax>
                </MathJaxContext>
              }
            ></KeyValuePair>
            <KeyValuePair
              label="Evaluation:"
              orientation={'horizontal'}
              value={evaluateLatexNumeric(latex) === null ? 'None' : evaluateLatexNumeric(latex)}
            ></KeyValuePair>
            <MathJaxContext>
              <MathJax>{`\\(${final}\\)`}</MathJax>
            </MathJaxContext>
            {renderedSteps}
          </div>
        </Paper.Content>
      </Paper>
      <FunctionPlot latex={latex}></FunctionPlot>
    </div>
  );
}
