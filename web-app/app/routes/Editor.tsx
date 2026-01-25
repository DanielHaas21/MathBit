import {
  createMathProblem,
  CreateMathProblemRequest,
  getAllUsers,
  MathEngineSolveRequest,
  MathEngineSolveResponse,
  MathEngineSolveStep,
  refresh,
} from 'web-api-client';
import login from '../../middleware/auth/login';
import { useEffect, useRef, useState } from 'react';
import getApiConfig from '@/apiConfig';
import {
  Button,
  FunctionPlot,
  Icon,
  MathField,
  ResolveValue,
  SolveStep,
  InputModal,
} from '@/libs/ui/components';
import { ComputeEngine } from '@cortex-js/compute-engine';
import { BaseLayout, Paper } from '@/libs/ui/layouts';
import { latexToMathJson } from '@/libs/math';
import { evaluateLatexNumeric } from '@/libs/math/evaluateExpression';
import solve from '@/middleware/actions/solve';
import { Header } from '@/libs/ui/components/Header';

export default function Editor() {
  // useEffect(() => {
  //   async function d() {
  //     const auth = await login({ email: 'e.procházková4@example.com', password: 'test' });
  //   }

  //   d();
  // }, []);
  // const test = async () => {
  //   async function testRefresh() {
  //     try {
  //       const response = await refresh(getApiConfig());

  //       console.log('Refresh response:', response);
  //     } catch (err) {
  //       console.error('Refresh request failed:', err);
  //     }
  //   }

  //   testRefresh();
  // };
  const [latex, setLatex] = useState<string>('');
  const [final, setFinal] = useState<string>('');
  const [finalSteps, setFinalSteps] = useState<MathEngineSolveStep[]>([]);
  const [isOpen, setIsOpen] = useState<boolean>(false);

  const solveExpression = async () => {
    console.log(latexToMathJson(latex));
    const solved: MathEngineSolveResponse = await solve(latex);

    setFinal(solved.finalExpression);
    setFinalSteps(solved.steps);
    console.log(solved);
  };

  const renderedSteps =
    finalSteps &&
    finalSteps.map((m, i) => <SolveStep step={m} index={i} isFinal={false}></SolveStep>);

  const renderFinal = final && (
    <SolveStep step={final} isFinal={true} index={finalSteps.length}></SolveStep>
  );
  return (
    <>
      <InputModal
        Open={isOpen}
        title="Create New Problem"
        onResolve={async (value: ResolveValue | false) => {
          setIsOpen(false);

          if (!value) return;

          const newProblem = await createMathProblem(
            {
              userId: 1,
              problem: {
                name: value.name,
                originalExpression: latex,
                //stdescription: value.description,
              },
            },
            getApiConfig(true)
          );
        }}
      ></InputModal>
      <BaseLayout className="overflow-hidden">
        <BaseLayout.Menu>
          <Header
            route={[
              { pageTitle: 'Browser', pageRoute: '/browser' },
              { pageTitle: 'Editor', pageRoute: '/browser/editor' },
            ]}
          />
        </BaseLayout.Menu>
        <BaseLayout.Content>
          <div className="relative w-full h-full bg-white-800 overflow-hidden">
            <FunctionPlot
              latex={latex}
              xRange={[-100, 100]}
              className="absolute h-full w-full pointer-events-none"
            />
            <Paper
              thickness="sm"
              className="border border-white-800 relative left-5 top-10 w-[600px] h-[85%] p-3 flex flex-col"
            >
              <Paper.Title className="flex flex-row items-start gap-3">
                <MathField initialLatex={latex} onChange={(newLatex) => setLatex(newLatex)} />
                <Button size="lg" className="mt-5 gap-2" onClick={solveExpression}>
                  <Icon name="paper-plane"></Icon>Solve
                </Button>
              </Paper.Title>
              <Paper.Content className="flex flex-col flex-1 min-h-0">
                <div className="w-full flex-1 min-h-0 flex flex-col overflow-y-auto">
                  {renderedSteps}
                  {renderFinal}
                </div>
                <div className="w-full flex-shrink-0 pt-2 flex justify-end gap-4">
                  <Button className="gap-2">
                    <Icon name="download"></Icon>Save as new
                  </Button>
                </div>
              </Paper.Content>
            </Paper>
          </div>
        </BaseLayout.Content>
      </BaseLayout>
    </>
  );
}
