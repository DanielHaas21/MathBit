import { Button, Header, Icon, Label } from '@/libs/ui/components';
import { BaseLayout } from '@/libs/ui/layouts';
import { Paper } from '@/libs/ui/layouts';
import { useTranslation } from '@/libs/ui/provider/UiProvider';
import { RootState } from '@/store/store';
import { MathJaxContext, MathJax } from 'better-react-mathjax';
import { useSelector } from 'react-redux';
import { useNavigate } from 'react-router-dom';

export default function Home() {
  const user = useSelector((state: RootState) => state.User);

  const t = useTranslation('pages.home');

  const navigate = useNavigate();
  return (
    <BaseLayout>
      <BaseLayout.Menu>
        <Header route={[]} />
      </BaseLayout.Menu>
      <BaseLayout.Content className="bg-white-800 overflow-y-auto flex-col">
        <div className="flex flex-col justify-center w-full min-h-[500px]  p-2 md:p-0 pt-[60px] md:ps-[120px] mb-[100px] gap-10">
          <Label className="text-2xl md:text-[32px] text-text-black font-semibold break-words">
            {t('hero.title')}
          </Label>
          <Label size="md" className="text-text-grey ms-2 break-words leading-relaxed">
            {t('hero.description')}
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
              {t('hero.beginSolving')}
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
                {t('hero.browseSavedProblems')}
              </Button>
            )}
          </div>
        </div>
        <Label className="text-2xl md:text-[32px] text-text-black font-semibold ms-[6%] pb-8 w-[90%] text-left break-words">
          {t('sections.features')}
        </Label>
        <div className="w-full flex flex-col items-center p-6 gap-2">
          <Paper className="bg-white-50 border border-white-800 rounded-xl !p-6 w-full  md:max-w-[90%]">
            <Paper.Content className="grid grid-cols-1 lg:grid-cols-2 gap-6 items-center">
              <div className="flex flex-col gap-4">
                <div className="flex items-start gap-3">
                  <div className="mt-1 text-primary-base">
                    <Icon name="keyboard" />
                  </div>
                  <div className="flex flex-col">
                    <Label size="lg" className="text-text-black font-medium">
                      {t('features.latex.title')}
                    </Label>
                    <Label
                      size="sm"
                      className="text-text-grey pb-3 flex flex-col sm:flex-row sm:flex-wrap gap-2 break-words leading-relaxed"
                    >
                      <span>{t('features.latex.descriptionBeforeExamples')}</span>
                      <MathJaxContext>
                        <div className="flex flex-wrap gap-2 max-w-full overflow-x-auto">
                          <MathJax dynamic>{`\\(\\frac{1}{2}\\)`}</MathJax>
                          <MathJax dynamic>{'\\(\\sqrt{2}\\)'}</MathJax>
                          <MathJax dynamic>{'\\(\\sin(\\theta)\\)'}</MathJax>
                        </div>
                      </MathJaxContext>
                      <span>{t('features.latex.descriptionAfterExamples')}</span>
                    </Label>
                  </div>
                </div>
              </div>
            </Paper.Content>
          </Paper>

          <Paper className="bg-white-50 border border-white-800 rounded-xl !p-6 w-full  md:max-w-[90%]">
            <Paper.Content className="grid grid-cols-1 lg:grid-cols-2 gap-6 items-center">
              <div className="flex flex-col gap-4">
                <div className="flex items-start gap-3">
                  <div className="flex items-start gap-3">
                    <div className="mt-1 text-primary-base">
                      <Icon name="paper-plane" />
                    </div>
                    <div className="flex flex-col">
                      <Label size="lg" className="text-text-black font-medium">
                        {t('features.oneClick.title')}
                      </Label>
                      <Label size="sm" className="text-text-grey pb-3 break-words leading-relaxed">
                        {t('features.oneClick.descriptionBeforeSolve')}
                        <span className="font-semibold text-clip">
                          {t('features.oneClick.solveWord')}
                        </span>
                        {t('features.oneClick.descriptionAfterSolve')}
                      </Label>
                    </div>
                  </div>
                </div>
              </div>
            </Paper.Content>
          </Paper>

          <Paper className="bg-white-50 border border-white-800 rounded-xl !p-6 w-full  md:max-w-[90%]">
            <Paper.Content className="grid grid-cols-1 lg:grid-cols-2 gap-6 items-center">
              <div className="flex flex-col gap-4">
                <div className="flex items-start gap-3">
                  <div className="flex items-start gap-3">
                    <div className="mt-1 text-primary-base">
                      <Icon name="download" />
                    </div>
                    <div className="flex flex-col">
                      <Label size="lg" className="text-text-black font-medium">
                        {t('features.saveLoggedIn.title')}
                      </Label>
                      <Label size="sm" className="text-text-grey pb-3 break-words leading-relaxed">
                        {t('features.saveLoggedIn.description')}
                      </Label>
                    </div>
                  </div>
                </div>
              </div>
            </Paper.Content>
          </Paper>
        </div>
        <Label className="text-2xl md:text-[32px] text-text-black font-semibold ms-[6%] pb-8 w-[90%] text-left break-words">
          {t('sections.quickTutorial')}
        </Label>
        <div className="w-full flex flex-col items-center p-6 gap-2">
          <Paper className="bg-white-50 border border-white-800 rounded-xl !p-6 w-full  md:max-w-[90%]">
            <Paper.Content className="flex flex-col gap-4">
              <div className="grid grid-cols-1 lg:grid-cols-3 gap-[56px] md:gap-4">
                <div className="flex flex-col gap-2">
                  <Label size="lg" className="text-text-black font-medium">
                    {t('tutorial.steps.step1Title')}
                  </Label>
                  <Label size="sm" className="text-text-grey break-words leading-relaxed">
                    {t('tutorial.steps.step1Description')}
                  </Label>
                </div>
                <div className="flex flex-col gap-2">
                  <Label size="lg" className="text-text-black font-medium">
                    {t('tutorial.steps.step2Title')}
                  </Label>
                  <Label size="sm" className="text-text-grey break-words leading-relaxed">
                    {t('tutorial.steps.step2Description')}
                  </Label>
                </div>
                <div className="flex flex-col gap-2">
                  <Label size="lg" className="text-text-black font-medium">
                    {t('tutorial.steps.step3Title')}
                  </Label>
                  <Label size="sm" className="text-text-grey break-words leading-relaxed">
                    {t('tutorial.steps.step3Description')}
                  </Label>
                </div>
              </div>

              <div className="flex flex-col gap-2">
                <Label size="lg" className="text-text-black font-medium">
                  {t('tutorial.examples.title')}
                </Label>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-2">
                  <div className="border border-white-800 rounded-lg bg-white-50 p-3">
                    <Label size="sm" className="text-text-grey break-words leading-relaxed">
                      <span className="font-semibold text-text-black">
                        {t('tutorial.examples.fraction')}
                      </span>{' '}
                      <MathJaxContext>
                        <div className="flex flex-wrap gap-2 max-w-full overflow-x-auto">
                          <MathJax dynamic>{`\\(\\frac{x}{2} + \\frac{3}{4x}\\)`}</MathJax>
                        </div>
                      </MathJaxContext>
                    </Label>
                  </div>
                  <div className="border border-white-800 rounded-lg bg-white-50 p-3">
                    <Label size="sm" className="text-text-grey break-words leading-relaxed">
                      <span className="font-semibold text-text-black">
                        {t('tutorial.examples.root')}
                      </span>{' '}
                      <MathJaxContext>
                        <div className="flex flex-wrap gap-2 max-w-full overflow-x-auto">
                          <MathJax dynamic>{`\\(\\sqrt{2} + \\sqrt[3]{8}\\)`}</MathJax>
                        </div>
                      </MathJaxContext>
                    </Label>
                  </div>
                  <div className="border border-white-800 rounded-lg bg-white-50 p-3">
                    <Label size="sm" className="text-text-grey break-words leading-relaxed">
                      <span className="font-semibold text-text-black">
                        {t('tutorial.examples.trig')}
                      </span>{' '}
                      <MathJaxContext>
                        <div className="flex flex-wrap gap-2 max-w-full overflow-x-auto">
                          <MathJax dynamic>{`\\(\\sin(\\pi/2) + \\cos(0)\\)`}</MathJax>
                        </div>
                      </MathJaxContext>
                    </Label>
                  </div>
                  <div className="border border-white-800 rounded-lg bg-white-50 p-3">
                    <Label size="sm" className="text-text-grey break-words leading-relaxed">
                      <span className="font-semibold text-text-black">
                        {t('tutorial.examples.functions')}
                      </span>{' '}
                      <MathJaxContext>
                        <div className="flex flex-wrap gap-2 max-w-full overflow-x-auto">
                          <MathJax dynamic>{`\\(\\log(x) \\times \\ln(x)\\)`}</MathJax>
                        </div>
                      </MathJaxContext>
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
