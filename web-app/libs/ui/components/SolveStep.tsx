import { MathEngineSolveStep } from 'web-api-client';
import { Label } from './Label';
import { Icon } from './Icon';
import { Checkbox } from './Checkbox';
import { useState } from 'react';
import { MathJax, MathJaxContext } from 'better-react-mathjax';
import { cn } from '../utils';
import { AnimatePresence, motion } from 'framer-motion';
import { ClipBoardCopy } from './ClipboardCopy';

export interface SolveStepProps {
  step: MathEngineSolveStep | string;
  index: number;
  isFinal: boolean;
}
//5x\times \left(5-d\right)
/**
 * Component to display a single solve step with toggleable LaTeX view and collapsible content.
 */
export const SolveStep: React.FC<SolveStepProps> = ({ step, isFinal, index, ...props }) => {
  const [showAsLatex, setShowAsLatex] = useState<boolean>(false);
  const [isOpen, setIsOpen] = useState<boolean>(true);
  const handleToggle = () => setIsOpen((prev) => !prev);

  // Determine the content to display based on whether it's a final step or not
  const displayedContent = !showAsLatex ? (
    <div className="flex w-full flex-row items-center justify-center gap-3 py-2">
      <MathJaxContext>
        <div className="flex items-center gap-2">
          <MathJax>{`\\(${(step as MathEngineSolveStep).stepBefore}\\)`}</MathJax>
          <ClipBoardCopy text={`\\(${(step as MathEngineSolveStep).stepBefore}\\)`} />
        </div>
        <MathJax>{`\\(\\longrightarrow\\)`}</MathJax>
        <div className="flex items-center gap-2">
          <MathJax>{`\\(${(step as MathEngineSolveStep).stepAfter}\\)`}</MathJax>
          <ClipBoardCopy text={`\\(${(step as MathEngineSolveStep).stepAfter}\\)`} />
        </div>
      </MathJaxContext>
    </div>
  ) : (
    <div className="flex w-full flex-row items-center justify-center gap-3 py-2">
      <div className="flex items-center gap-2">
        <Label>{(step as MathEngineSolveStep).stepBefore}</Label>
        <ClipBoardCopy text={(step as MathEngineSolveStep).stepBefore} />
      </div>
      <Label>→</Label>
      <div className="flex items-center gap-2">
        <Label>{(step as MathEngineSolveStep).stepAfter}</Label>
        <ClipBoardCopy text={(step as MathEngineSolveStep).stepAfter} />
      </div>
    </div>
  );

  const displayedFinalContent = !showAsLatex ? (
    <div className="flex w-full flex-row items-center justify-center gap-2 py-2">
      <MathJaxContext>
        <MathJax>{`\\(${step}\\)`}</MathJax>
      </MathJaxContext>
      <ClipBoardCopy text={`\\(${step as string}\\)`} />
    </div>
  ) : (
    <div className="flex w-full flex-row items-center justify-center gap-2 py-2">
      <Label>{step as string}</Label>
      <ClipBoardCopy text={step as string} />
    </div>
  );

  return (
    <div className="w-full  border-b border-b-white-800 flex flex-col pt-4 pb-4" {...props}>
      <div className="w-full">
        <Icon
          name="chevron-down"
          size="xl"
          className={cn(
            'px-2 text-black-500 cursor-pointer transition-transform duration-200 ease-in-out',
            !isOpen && 'rotate-[-90deg]'
          )}
          onClick={handleToggle}
        ></Icon>
        <Label size="xl">{isFinal ? 'Final expression:' : `Step ${index + 1}:`}</Label>
      </div>
      <AnimatePresence initial={false}>
        {isOpen && (
          <motion.div
            initial={{ opacity: 0, y: -8, height: 0 }}
            animate={{ opacity: 1, y: 0, height: 'auto' }}
            exit={{ opacity: 0, y: -8, height: 0 }}
            transition={{ duration: 0.25, ease: 'easeInOut' }}
            className="overflow-hidden"
          >
            {isFinal ? displayedFinalContent : displayedContent}
            <div
              className={cn(
                'w-full flex flex-row gap-4 px-4 pb-2 justify-between pt-4',
                isFinal && 'justify-end'
              )}
            >
              {!isFinal && (
                <Label className="whitespace-normal h-fit">
                  {(step as MathEngineSolveStep).stepRuleDescription}
                </Label>
              )}
              <Checkbox
                checked={showAsLatex}
                onChange={() => setShowAsLatex(!showAsLatex)}
                label="LateX"
              ></Checkbox>
              {!isFinal && (
                <ClipBoardCopy
                  text={`\\(${(step as MathEngineSolveStep).stepBefore}\\;\\longrightarrow\\;${(step as MathEngineSolveStep).stepAfter}\\)`}
                />
              )}
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  );
};
