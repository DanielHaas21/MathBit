import MathInput, { KeyId, KeyProps } from 'react-math-keyboard';
import { useEffect, useRef, useState } from 'react';
import { Select } from './Select';
import { KeyGroupIds, SelectInputOption } from '../types';

export interface MathFieldProps {
  initialLatex?: string;
  onChange?: (latex: string) => void;
}

export const MathField: React.FC<MathFieldProps> = ({ initialLatex, onChange }) => {
  const [tab, setTab] = useState<KeyGroupIds>('functions');
  const [latex, setLatex] = useState<string>(initialLatex || '');
  const [isMathKeyboardReady, setIsMathKeyboardReady] = useState(false);

  // Dynamically import math keyboard and its dependencies to avoid SSR issues and reduce initial bundle size
  // this is ai generated code for an issue that would normally be hard to trace
  useEffect(() => {
    let isMounted = true;

    const initializeMathKeyboard = async () => {
      const browserWindow = window as Window & {
        require?: (moduleId: string) => unknown;
        jQuery?: unknown;
        $?: unknown;
      };

      const jqueryModule = await import('jquery');
      const jQueryInstance = jqueryModule.default;

      browserWindow.jQuery = jQueryInstance;
      browserWindow.$ = jQueryInstance;

      await import('mathquill4keyboard/build/mathquill.css');
      await import('mathquill4keyboard/build/mathquill');

      if (!browserWindow.require) {
        browserWindow.require = (moduleId: string) => {
          if (
            moduleId === 'mathquill4keyboard/build/mathquill.css' ||
            moduleId === 'mathquill4keyboard/build/mathquill'
          ) {
            return true;
          }

          throw new Error(`Unsupported require call in browser: ${moduleId}`);
        };
      }

      if (isMounted) {
        setIsMathKeyboardReady(true);
      }
    };

    initializeMathKeyboard();

    return () => {
      isMounted = false;
    };
  }, []);

  // Keep internal state in sync with parent-provided latex
  useEffect(() => {
    setLatex(initialLatex || '');
  }, [initialLatex]);

  const options: SelectInputOption[] = [
    { label: 'Functions', value: 'functions' },
    { label: 'Trigonometry', value: 'trigo' },
    { label: 'Combinatorics', value: 'combinatorics' },
  ];

  const allKeys = [
    // Functions tab
    {
      id: 'sqrt',
      label: '\\sqrt{}',
      labelType: 'tex',
      groups: ['functions'],
      mathfieldInstructions: { content: '\\sqrt{}', method: 'write' },
    },
    {
      id: 'cube',
      label: '\\sqrt[3]{}',
      labelType: 'tex',
      groups: ['functions'],
      mathfieldInstructions: { content: '\\sqrt[3]{}', method: 'write' },
    },
    {
      id: 'abs',
      label: '\\left| \\right|',
      labelType: 'tex',
      groups: ['functions'],
      mathfieldInstructions: { content: '\\left|\\right|', method: 'write' },
    },
    {
      id: 'log',
      label: '\\log',
      labelType: 'tex',
      groups: ['functions'],
      mathfieldInstructions: { content: '\\log', method: 'write' },
    },
    {
      id: 'ln',
      label: '\\ln',
      labelType: 'tex',
      groups: ['functions'],
      mathfieldInstructions: { content: '\\ln', method: 'write' },
    },
    {
      id: 'exp',
      label: 'e^{}',
      labelType: 'tex',
      groups: ['functions'],
      mathfieldInstructions: { content: 'e^{}', method: 'write' },
    },
    {
      id: 'factorial',
      label: '!',
      labelType: 'tex',
      groups: ['functions'],
      mathfieldInstructions: { content: '!', method: 'write' },
    },
    // Algebra tab
    {
      id: 'frac',
      label: '\\frac{}{}',
      labelType: 'tex',
      groups: ['functions'],
      mathfieldInstructions: { content: '\\frac{}{}', method: 'write' },
    },
    {
      id: 'sin',
      label: '\\sin',
      labelType: 'tex',
      groups: ['trigo'],
      mathfieldInstructions: { content: '\\sin', method: 'write' },
    },
    {
      id: 'cos',
      label: '\\cos',
      labelType: 'tex',
      groups: ['trigo'],
      mathfieldInstructions: { content: '\\cos', method: 'write' },
    },
    {
      id: 'tan',
      label: '\\tan',
      labelType: 'tex',
      groups: ['trigo'],
      mathfieldInstructions: { content: '\\tan', method: 'write' },
    },
    {
      id: 'cotan',
      label: '\\cot',
      labelType: 'tex',
      groups: ['trigo'],
      mathfieldInstructions: { content: '\\cot', method: 'write' },
    },
    {
      id: 'arcsin',
      label: '\\arcsin',
      labelType: 'tex',
      groups: ['trigo'],
      mathfieldInstructions: { content: '\\arcsin', method: 'write' },
    },
    {
      id: 'arccos',
      label: '\\arccos',
      labelType: 'tex',
      groups: ['trigo'],
      mathfieldInstructions: { content: '\\arccos', method: 'write' },
    },
    {
      id: 'arctan',
      label: '\\arctan',
      labelType: 'tex',
      groups: ['trigo'],
      mathfieldInstructions: { content: '\\arctan', method: 'write' },
    },
    // Geometry tab
    {
      id: 'pi',
      label: '\\pi',
      labelType: 'tex',
      groups: ['functions'],
      mathfieldInstructions: { content: '\\pi', method: 'write' },
    },
    {
      id: 'combination',
      label: 'C(n, k)',
      labelType: 'tex',
      groups: ['combinatorics'],
      mathfieldInstructions: { content: 'C(n,k)', method: 'write' },
    },
    {
      id: 'permutation',
      label: 'P(n, k)',
      labelType: 'tex',
      groups: ['combinatorics'],
      mathfieldInstructions: { content: 'P(n,k)', method: 'write' },
    },
    {
      id: 'variation',
      label: 'V(n, k)',
      labelType: 'tex',
      groups: ['combinatorics'],
      mathfieldInstructions: { content: 'V(n,k)', method: 'write' },
    },
  ];

  const keysForTab = allKeys.filter((k) => k.groups?.includes(tab));

  if (!isMathKeyboardReady) {
    return null;
  }

  return (
    <div className="flex flex-col justify-between p-4 w-fit gap-2 [&>div:first-child>span]:!border-xl ">
      <MathInput
        style={{
          minWidth: '300px',
          borderColor: '#E3E5EA',
          backgroundColor: '#F4F5F5',
        }}
        setValue={(val) => {
          setLatex(val);
          onChange?.(val);
        }}
        initialLatex={latex}
        numericToolbarKeys={keysForTab as KeyProps[]}
      />
      <div>
        <Select
          size="md"
          options={options}
          value={tab}
          onChange={(value) => {
            setTab(value as KeyGroupIds);
          }}
        ></Select>
      </div>
    </div>
  );
};

MathField.displayName = 'MathField';
