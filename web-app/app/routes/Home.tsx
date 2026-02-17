import { Button, Header, Icon, Label } from '@/libs/ui/components';
import { BaseLayout } from '@/libs/ui/layouts';
import { Paper } from '@/libs/ui/layouts';
import { RootState } from '@/store/store';
import { useSelector } from 'react-redux';
import { useNavigate } from 'react-router-dom';

export default function Home() {
  const user = useSelector((state: RootState) => state.User);

  const navigate = useNavigate();
  return (
    <BaseLayout>
      <BaseLayout.Menu>
        <Header route={[]} />
      </BaseLayout.Menu>
      <BaseLayout.Content className="bg-white-800 overflow-y-auto flex-col">
        <div className="flex flex-col w-full pt-[60px] ps-[120px] mb-[100px] gap-10">
          <Label className="text-[32px] text-text-black font-semibold">Welcome to MathBit</Label>
          <Label size="md" className="text-text-grey ms-2">
            A math problem solver that shows you step by step solutions. Get started by opening the
            editor and entering a math problem. You can also browse saved problems if you have an
            account.
          </Label>
          <div className="flex flex-col sm:flex-row gap-3 pt-2">
            <Button
              size="lg"
              className="gap-2"
              onClick={() => {
                navigate('/browser/editor');
              }}
            >
              <Icon name="pen-to-square" />
              Begin solving
            </Button>
            {user.authStatus === 'authenticated' && (
              <Button
                size="lg"
                outline="primary"
                className="gap-2"
                onClick={() => {
                  navigate('/browser');
                }}
              >
                <Icon name="magnifying-glass" />
                Browse saved problems
              </Button>
            )}
          </div>
        </div>
        <Label className="text-[32px] text-text-black font-semibold ms-[6%] pb-8 w-[90%] text-left">
          Features
        </Label>
        <div className="w-full flex flex-col items-center p-6 gap-2">
          <Paper className="bg-white-50 border border-white-800 rounded-xl !p-6 w-full max-w-[90%]">
            <Paper.Content className="grid grid-cols-1 lg:grid-cols-2 gap-6 items-center">
              <div className="flex flex-col gap-4">
                <div className="flex items-start gap-3">
                  <div className="mt-1 text-primary-base">
                    <Icon name="keyboard" />
                  </div>
                  <div className="flex flex-col">
                    <Label size="lg" className="text-text-black font-medium">
                      Input is LaTeX-like
                    </Label>
                    <Label size="sm" className="text-text-grey">
                      The editor produces LaTeX-style math (e.g.{' '}
                      <code>
                        \\frac{}
                        {}{' '}
                      </code>
                      ,<code> \\sqrt{} </code>, <code>\\sin</code>). You can type normally or use
                      the function tabs.
                    </Label>
                  </div>
                </div>
              </div>
            </Paper.Content>
          </Paper>

          <Paper className="bg-white-50 border border-white-800 rounded-xl !p-6 w-full max-w-[90%]">
            <Paper.Content className="grid grid-cols-1 lg:grid-cols-2 gap-6 items-center">
              <div className="flex flex-col gap-4">
                <div className="flex items-start gap-3">
                  <div className="flex items-start gap-3">
                    <div className="mt-1 text-primary-base">
                      <Icon name="paper-plane" />
                    </div>
                    <div className="flex flex-col">
                      <Label size="lg" className="text-text-black font-medium">
                        One click to solve
                      </Label>
                      <Label size="sm" className="text-text-grey">
                        Press <span className="font-semibold">Solve</span> to send your expression
                        to the engine and see each transformation as a step.
                      </Label>
                    </div>
                  </div>
                </div>
              </div>
            </Paper.Content>
          </Paper>

          <Paper className="bg-white-50 border border-white-800 rounded-xl !p-6 w-full max-w-[90%]">
            <Paper.Content className="grid grid-cols-1 lg:grid-cols-2 gap-6 items-center">
              <div className="flex flex-col gap-4">
                <div className="flex items-start gap-3">
                  <div className="flex items-start gap-3">
                    <div className="mt-1 text-primary-base">
                      <Icon name="download" />
                    </div>
                    <div className="flex flex-col">
                      <Label size="lg" className="text-text-black font-medium">
                        Save when logged in
                      </Label>
                      <Label size="sm" className="text-text-grey">
                        If you create an account, you can save solutions and find them later in the
                        browser.
                      </Label>
                    </div>
                  </div>
                </div>
              </div>
            </Paper.Content>
          </Paper>

          <Paper className="bg-white-50 border border-white-800 rounded-xl !p-6">
            <Paper.Title className="text-text-black font-semibold">Quick tutorial</Paper.Title>
            <Paper.Content className="flex flex-col gap-4">
              <div className="grid grid-cols-1 lg:grid-cols-3 gap-4">
                <div className="flex flex-col gap-2">
                  <Label size="lg" className="text-text-black font-medium">
                    1) Enter your problem
                  </Label>
                  <Label size="sm" className="text-text-grey">
                    Open the editor and build your expression. The on-screen keyboard helps with
                    fractions, roots, trig and combinatorics.
                  </Label>
                </div>
                <div className="flex flex-col gap-2">
                  <Label size="lg" className="text-text-black font-medium">
                    2) Click Solve
                  </Label>
                  <Label size="sm" className="text-text-grey">
                    You’ll get a step list explaining what changed. If something looks wrong, adjust
                    the input and solve again.
                  </Label>
                </div>
                <div className="flex flex-col gap-2">
                  <Label size="lg" className="text-text-black font-medium">
                    3) Save (optional)
                  </Label>
                  <Label size="sm" className="text-text-grey">
                    Log in to save problems and revisit them later from the browser.
                  </Label>
                </div>
              </div>

              <div className="flex flex-col gap-2">
                <Label size="lg" className="text-text-black font-medium">
                  Examples to try
                </Label>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-2">
                  <div className="border border-white-800 rounded-lg bg-white-50 p-3">
                    <Label size="sm" className="text-text-grey">
                      <span className="font-semibold text-text-black">Fraction:</span>{' '}
                      <code>
                        \\frac{1}
                        {2} + \\frac{3}
                        {4}
                      </code>
                    </Label>
                  </div>
                  <div className="border border-white-800 rounded-lg bg-white-50 p-3">
                    <Label size="sm" className="text-text-grey">
                      <span className="font-semibold text-text-black">Root:</span>{' '}
                      <code>
                        \\sqrt{2} + \\sqrt[3]{8}
                      </code>
                    </Label>
                  </div>
                  <div className="border border-white-800 rounded-lg bg-white-50 p-3">
                    <Label size="sm" className="text-text-grey">
                      <span className="font-semibold text-text-black">Trig:</span>{' '}
                      <code>\\sin(\\pi/2) + \\cos(0)</code>
                    </Label>
                  </div>
                  <div className="border border-white-800 rounded-lg bg-white-50 p-3">
                    <Label size="sm" className="text-text-grey">
                      <span className="font-semibold text-text-black">Combinatorics:</span>{' '}
                      <code>C(n,k) + P(n,k)</code>
                    </Label>
                  </div>
                </div>
              </div>
            </Paper.Content>
          </Paper>
        </div>
      </BaseLayout.Content>
    </BaseLayout>
  );
}
