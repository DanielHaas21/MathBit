import {
  createMathProblem,
  CreateMathProblem201,
  createMathProblemStep,
  deleteMathProblemStepsByProblemId,
  getMathProblemById,
  GetMathProblemById200,
  getMathProblemStepsByProblemId,
  GetMathProblemStepsByProblemId200,
  MathEngineSolveResponse,
  MathEngineSolveStep,
  MathProblem,
  SolveMathExpression400,
  updateMathProblem,
} from 'web-api-client';
import { useEffect, useState } from 'react';
import { useLocation, useNavigate, useParams } from 'react-router-dom';
import getApiConfig from '@/apiConfig';
import {
  Button,
  FunctionPlot,
  Icon,
  MathField,
  ResolveValue,
  SolveStep,
  InputModal,
  useToast,
  Label,
} from '@/libs/ui/components';
import { BaseLayout, Paper } from '@/libs/ui/layouts';
import { evaluateLatexNumeric } from '@/libs/math/evaluateExpression';
import solve from '@/middleware/actions/solve';
import { Header } from '@/libs/ui/components/Header';
import { useSelector } from 'react-redux';
import { RootState } from '@/store/store';
import { BreadcrumbItem } from '@/libs/ui/types';
import { useTranslation } from '@/libs/ui/provider';

export default function Editor() {
  const user = useSelector((state: RootState) => state.User);
  const t = useTranslation('pages.editor');
  const ui_t = useTranslation('ui');
  // Router hooks
  const navigate = useNavigate();
  const { id: routeId } = useParams();
  const route = useLocation().pathname;
  // parsed as string turned to falsy if not present
  const hasExistingId = !!routeId;
  // Toast hook
  const { show } = useToast();
  const [currentSavedProblem, setCurrentSavedProblem] = useState<MathProblem | null>(null);
  // latex state
  const [latex, setLatex] = useState<string>('');
  // solved result state
  const [final, setFinal] = useState<string>('');
  // solved steps state
  const [finalSteps, setFinalSteps] = useState<MathEngineSolveStep[]>([]);
  // modal states
  const [isOpen, setIsOpen] = useState<boolean>(false);
  const [isOpenUpdate, setIsOpenUpdate] = useState<boolean>(false);
  // error state for fetch problem, if error occurs we redirect to 404 or editor depending on error, so we need to track it to avoid showing toast twice on auth error (once here and once in layout on refresh)
  const [error, setError] = useState<string | null>(null);
  useEffect(() => {
    if (route === '/browser/editor') {
      setCurrentSavedProblem(null);
      setLatex('');
      setFinal('');
      setFinalSteps([]);
      return;
    }
  }, [route]);

  // Redirect to /editor if not logged in
  useEffect(() => {
    // Avoid preemptive redirect before refresh completes.
    // If viewing a specific problem, attempt fetch and redirect only on failure.
    if (routeId) {
      (async () => {
        try {
          const problem: GetMathProblemById200 = await getMathProblemById(
            parseInt(routeId),
            getApiConfig()
          );
          if (!problem?.id) {
            navigate('/*');
          }
          if (!problem) return;

          const steps: GetMathProblemStepsByProblemId200 = await getMathProblemStepsByProblemId(
            problem.id as number,
            getApiConfig(false)
          );
          setFinalSteps(
            (steps?.data ?? [])
              .slice()
              .sort((a, b) => a.stepIndex - b.stepIndex)
              .map((s) => ({
                stepBefore: s.stepBefore ?? '',
                stepAfter: s.stepAfter ?? '',
                stepRuleDescription: s.description,
              }))
          );

          setCurrentSavedProblem(problem);
          setLatex(problem.originalExpression);
          setFinal(problem.simplifiedExpression ?? '');
        } catch (err: any) {
          const status = err?.status ?? err?.response?.status;

          if (status === 401) {
            navigate('/browser/editor'); // return to editor without id and auth

            show({
              icon: 'triangle-exclamation',
              variant: 'error',
              title: t('messages.notAuthenticated'),
              description: t('messages.notAuthenticatedDescription'),
            });
          } else {
            navigate('/*');
          }
        }
      })();
    }
  }, [routeId, navigate, show]);

  // used for responsive design
  const [width, setWidth] = useState<number | null>(null);
  useEffect(() => {
    const handleResize = () => setWidth(window.innerWidth);

    handleResize();

    window.addEventListener('resize', handleResize);
    return () => window.removeEventListener('resize', handleResize);
  }, []);
  // solve expression handler
  const solveExpression = async () => {
    const evaluate = evaluateLatexNumeric(latex);

    // if its a numeric expression, skip processing via engine and return final directly
    if (evaluate !== null) {
      setFinal(evaluate.toString());
      setFinalSteps([]);
      return;
    }

    setError(null);
    try {
      const solved: MathEngineSolveResponse = await solve(latex);
      setError(null);
      setFinal(solved.finalExpression);
      setFinalSteps(solved.steps);
    } catch (error) {
      setError(error instanceof Error ? t(`errors.${error.message}`) : t('errors.other'));
      show({
        icon: 'triangle-exclamation',
        variant: 'error',
        title: t('messages.solveError'),
        description: t('messages.solveErrorDescription'),
      });
      return;
    }
  };

  // render solved steps and final
  const renderedSteps =
    finalSteps &&
    finalSteps.map((m, i) => <SolveStep step={m} key={i} index={i} isFinal={false}></SolveStep>);

  const renderFinal = final && (
    <SolveStep step={final} key="final" isFinal={true} index={finalSteps.length}></SolveStep>
  );

  const renderError = error && (
    <div className="w-full flex p-4 items-center justify-center">
      <Label className="text-error-text">{error}</Label>
    </div>
  );
  // Handle if  we are editing an existing problem or creating a new one, cant be annotated due to length
  const BreadcrumbProblem =
    currentSavedProblem &&
    route &&
    ({
      pageTitle: currentSavedProblem.name ? currentSavedProblem.name : t('breadcrumb.unnamed'),
      pageRoute: '/browser/editor/' + currentSavedProblem.id,
    } as BreadcrumbItem);

  return (
    <>
      <InputModal
        Open={isOpen}
        title={ui_t('inputModal.editor.create.title')}
        onResolve={async (value: ResolveValue | false) => {
          setIsOpen(false);
          if (!value || !user.user?.id) return; // Cancelled action

          try {
            const newProblem: CreateMathProblem201 = await createMathProblem(
              {
                userId: user.user?.id, // Assumes user is logged in, he would be already redirected if not
                problem: {
                  name: value.name,
                  originalExpression: latex,
                  simplifiedExpression: final,
                  description: value.description,
                },
              },
              getApiConfig()
            );
            setCurrentSavedProblem(newProblem as MathProblem);

            const createdId = newProblem?.id;

            if (createdId) {
              for (const step of finalSteps) {
                await createMathProblemStep(
                  {
                    problemId: createdId,
                    step: {
                      stepBefore: step.stepBefore,
                      stepAfter: step.stepAfter,
                      description: step.stepRuleDescription,
                      problemId: createdId,
                      stepIndex: finalSteps.indexOf(step),
                    },
                  },
                  getApiConfig(false)
                );
              }

              show({
                icon: 'circle-check',
                variant: 'success',
                title: t('messages.saveSuccess'),
              });

              navigate(`/browser/editor/${createdId}`);
            }
          } catch (error) {
            show({
              icon: 'triangle-exclamation',
              variant: 'error',
              title: t('messages.saveError'),
            });
          }
        }}
      ></InputModal>
      {currentSavedProblem && routeId && (
        <InputModal
          Open={isOpenUpdate}
          title={ui_t('inputModal.editor.update.title')}
          data={{
            name: currentSavedProblem?.name ?? '',
            description: currentSavedProblem?.description ?? '',
          }}
          onResolve={async (value: ResolveValue | false) => {
            setIsOpenUpdate(false);
            if (!value || !user.user?.id || !currentSavedProblem.id) return; // Cancelled action

            try {
              await updateMathProblem(
                {
                  id: currentSavedProblem.id,
                  problem: {
                    name: value.name,
                    originalExpression: latex,
                    simplifiedExpression: final,
                    description: value.description,
                  },
                },
                getApiConfig()
              );

              await deleteMathProblemStepsByProblemId(currentSavedProblem.id, getApiConfig(false));

              for (const step of finalSteps) {
                await createMathProblemStep(
                  {
                    problemId: currentSavedProblem.id,
                    step: {
                      stepBefore: step.stepBefore,
                      stepAfter: step.stepAfter,
                      description: step.stepRuleDescription,
                      problemId: currentSavedProblem.id,
                      stepIndex: finalSteps.indexOf(step),
                    },
                  },
                  getApiConfig(false)
                );
              }

              setCurrentSavedProblem({
                ...currentSavedProblem,
                name: value.name,
                description: value.description,
                originalExpression: latex,
                simplifiedExpression: final,
              });

              show({
                icon: 'circle-check',
                variant: 'success',
                title: t('messages.updateSuccess'),
              });
            } catch (error) {
              show({
                icon: 'triangle-exclamation',
                variant: 'error',
                title: t('messages.updateError'),
              });
            }
          }}
        ></InputModal>
      )}

      <BaseLayout className="overflow-hidden">
        <BaseLayout.Menu>
          <Header
            route={
              BreadcrumbProblem
                ? [
                    { locKey: 'browser', pageRoute: '/browser' },
                    { locKey: 'editor', pageRoute: '/browser/editor' },
                    BreadcrumbProblem,
                  ]
                : [
                    { locKey: 'browser', pageRoute: '/browser' },
                    { locKey: 'editor', pageRoute: '/browser/editor' },
                  ]
            }
          />
        </BaseLayout.Menu>
        <BaseLayout.Content>
          <div className="relative w-full h-full bg-white-800 overflow-scroll md:overflow-hidden">
            {width && width >= 1024 && (
              <FunctionPlot
                latex={latex}
                xRange={[-50, 50]}
                className="absolute h-full w-full pointer-events-none"
              />
            )}

            <Paper
              thickness="sm"
              className="border border-white-800 ms-2 mt-2 md:mt-0 md:ms-0 md:relative md:left-5 md:top-10  md:w-[600px] h-[85%] p-3 flex flex-col"
            >
              <Paper.Title className="flex flex-col md:flex-row items-start gap-3">
                <MathField initialLatex={latex} onChange={(newLatex) => setLatex(newLatex)} />
                <Button size="lg" className="mt-5 gap-2" onClick={solveExpression}>
                  <Icon name="paper-plane"></Icon>
                  {t('solve')}
                </Button>
              </Paper.Title>
              <Paper.Content className="flex flex-col flex-1 min-h-0">
                <div className="w-full flex-1 min-h-0 flex flex-col overflow-y-auto">
                  {error ? (
                    <> {renderError}</>
                  ) : (
                    <>
                      {renderedSteps}
                      {renderFinal}
                    </>
                  )}
                </div>
                <div className="w-full flex-shrink-0 pt-2 flex justify-end gap-4">
                  <Button
                    className="gap-2"
                    onClick={() => {
                      if (!user.user?.id) {
                        show({
                          icon: 'triangle-exclamation',
                          variant: 'error',
                          title: t('messages.notAuthenticated'),
                          description: t('messages.notAuthenticatedDescription'),
                        });
                        return;
                      }
                      setIsOpen(true);
                    }}
                  >
                    <Icon name="download"></Icon>
                    {t('save')}
                  </Button>
                  {hasExistingId && (
                    <Button
                      className="gap-2"
                      onClick={() => {
                        setIsOpenUpdate(true);
                      }}
                    >
                      <Icon name="arrows-rotate"></Icon>
                      {t('update')}
                    </Button>
                  )}
                </div>
              </Paper.Content>
            </Paper>
            {width && width < 1024 && (
              <FunctionPlot
                latex={latex}
                xRange={[-50, 50]}
                className="h-full w-full pointer-events-none mt-10"
              />
            )}
          </div>
        </BaseLayout.Content>
      </BaseLayout>
    </>
  );
}
