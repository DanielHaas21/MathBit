import MathInput, { KeyId, KeyProps } from 'react-math-keyboard';
import { MathJax, MathJaxContext } from 'better-react-mathjax';
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

  const options: SelectInputOption[] = [
    { label: 'Functions', value: 'functions' },
    { label: 'Algebra', value: 'algebra' },
    { label: 'Calculus', value: 'sequences' },
    { label: 'Trigonometry', value: 'trigo' },
    { label: 'Geometry', value: 'geometry' },
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
      groups: ['algebra'],
      mathfieldInstructions: { content: '\\frac{}{}', method: 'write' },
    },
    {
      id: 'sigma',
      label: '\\sum',
      labelType: 'tex',
      groups: ['algebra'],
      mathfieldInstructions: { content: '\\sum', method: 'write' },
    },
    {
      id: 'product',
      label: '\\prod',
      labelType: 'tex',
      groups: ['algebra'],
      mathfieldInstructions: { content: '\\prod', method: 'write' },
    },
    // Calculus tab (sequences)
    {
      id: 'integral',
      label: '\\int',
      labelType: 'tex',
      groups: ['sequences'],
      mathfieldInstructions: { content: '\\int', method: 'write' },
    },
    {
      id: 'definite_integral',
      label: '\\int_{}^{}',
      labelType: 'tex',
      groups: ['sequences'],
      mathfieldInstructions: { content: '\\int_{}^{}', method: 'write' },
    },
    {
      id: 'derivative_dx',
      label: '\\frac{d}{dx}',
      labelType: 'tex',
      groups: ['sequences'],
      mathfieldInstructions: { content: '\\frac{d}{dx}', method: 'write' },
    },
    {
      id: 'derivative_dy',
      label: '\\frac{dy}{dx}',
      labelType: 'tex',
      groups: ['sequences'],
      mathfieldInstructions: { content: '\\frac{dy}{dx}', method: 'write' },
    },
    {
      id: 'partial_derivative',
      label: '\\frac{\\partial}{\\partial x}',
      labelType: 'tex',
      groups: ['sequences'],
      mathfieldInstructions: { content: '\\frac{\\partial}{\\partial x}', method: 'write' },
    },
    {
      id: 'limit',
      label: '\\lim_{x \\to 0}',
      labelType: 'tex',
      groups: ['sequences'],
      mathfieldInstructions: { content: '\\lim_{x \\to 0}', method: 'write' },
    },
    {
      id: 'prime_y1',
      label: "y'",
      labelType: 'tex',
      groups: ['sequences'],
      mathfieldInstructions: { content: "y'", method: 'write' },
    },
    {
      id: 'prime_y2',
      label: "y''",
      labelType: 'tex',
      groups: ['sequences'],
      mathfieldInstructions: { content: "y''", method: 'write' },
    },
    {
      id: 'prime_y3',
      label: "y'''",
      labelType: 'tex',
      groups: ['sequences'],
      mathfieldInstructions: { content: "y'''", method: 'write' },
    },
    {
      id: 'prime_fx',
      label: "f'(x)",
      labelType: 'tex',
      groups: ['sequences'],
      mathfieldInstructions: { content: "f'(x)", method: 'write' },
    },
    // Trigonometry tab
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
      groups: ['geometry'],
      mathfieldInstructions: { content: '\\pi', method: 'write' },
    },
    {
      id: 'angle',
      label: '\\angle',
      labelType: 'tex',
      groups: ['geometry'],
      mathfieldInstructions: { content: '\\angle', method: 'write' },
    },
    {
      id: 'degree',
      label: '^\\circ',
      labelType: 'tex',
      groups: ['geometry'],
      mathfieldInstructions: { content: '^\\circ', method: 'write' },
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

  return (
    <div className="flex flex-row justify-between p-4 w-fit gap-2 [&>div:first-child>span]:!border-xl ">
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
