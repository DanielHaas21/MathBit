import * as React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../../utils';
import { InputWrapper, InputWrapperProps } from '../InputWrapper';
import { Digits, CodeInputVariant } from '../../types';

type BaseCodeInputAttributes = Pick<
  React.InputHTMLAttributes<HTMLInputElement>,
  'id' | 'className' | 'onFocus' | 'onClick' | 'onInput' | 'value' | 'defaultValue'
>;

const CodeInputFieldVariants = cva(
  ` text-3xl text-center transition-all placeholder:text-black-300 font-semibold border-2 rounded-lg focus:outline-none focus:ring-4 focus:ring-brand-blue-100`,
  {
    variants: {
      color: {
        default: 'bg-white-50',
        grey: 'bg-background-inputs'
      },
      hasError: {
        true: '!border-error-text focus:[box-shadow:0_0_0_4px_theme(colors.error.bg)]',
        false: '!border-white-800 focus:!border-brand-blue-400'
      },
      size: {
        sm: 'w-12 h-12',
        md: 'w-14 h-14',
        lg: 'w-16 h-16 text-4xl'
      },
      disabled: {
        true: 'bg-white-50 placeholder:text-black-50 cursor-not-allowed !border-white-200'
      }
    },
    defaultVariants: {
      color: 'default',
      size: 'md'
    }
  }
);

type CodeInputFieldVariantProps = VariantProps<typeof CodeInputFieldVariants>;

export interface CodeInputFieldProps
  extends Omit<InputWrapperProps, 'children'>,
    CodeInputFieldVariantProps,
    BaseCodeInputAttributes {
  variant?: CodeInputVariant;
  onComplete?: (code: string) => void;
  onChange?: (code: string) => void;
  placeholder?: Digits;
}

export const CodeInputField = React.forwardRef<HTMLInputElement, CodeInputFieldProps>((props) => {
  const {
    LabelSize,
    htmlFor,
    isError = false,
    errorText,
    label,
    hint,
    size,
    disabled = false,
    color,
    variant = '4Digit',
    placeholder = variant === '4Digit' ? '0000' : '000000',
    value,
    defaultValue,
    onChange,
    onComplete,
    className
  } = props;

  const VariantLengths = {
    '4Digit': 4,
    '6Digit': 6
  }[variant];

  React.useEffect(() => {
    const initialValues = (value as string)?.split('') || (defaultValue as string)?.split('') || [];
    const filled = Array(VariantLengths)
      .fill('')
      .map((_, i) => initialValues[i] || '');
    setValues(filled);
  }, [value, defaultValue, VariantLengths]);

  const [values, setValues] = React.useState<string[]>(Array(VariantLengths).fill(''));
  const inputsRef = React.useRef<(HTMLInputElement | null)[]>([]);

  const handleChange = (value: string, index: number) => {
    if (!/^[0-9]?$/.test(value)) return; // only numbers

    const newValues = [...values];
    newValues[index] = value;
    setValues(newValues);

    if (value && index < VariantLengths - 1) {
      if (inputsRef.current[index + 1]?.value != '') {
        inputsRef.current[index + 2]?.focus(); // skip a line if its already filled in
      } else {
        inputsRef.current[index + 1]?.focus();
      }
    }

    if (onChange) {
      onChange(newValues.join(''));
    }
    if (newValues.every((val) => val !== '')) {
      onComplete?.(newValues.join(''));
    }
  };

  //Removing
  const handleKeyDown = (e: React.KeyboardEvent, index: number) => {
    if (e.key === 'Backspace' && !values[index] && index > 0) {
      inputsRef.current[index - 1]?.focus();
    }
    if (e.key === 'ArrowLeft') {
      inputsRef.current[index - 1]?.focus();
    }
    if (e.key === 'ArrowRight') {
      inputsRef.current[index + 1]?.focus();
    }
  };

  return (
    <InputWrapper
      LabelSize={LabelSize}
      label={label ?? 'Secure Code'}
      hint={hint}
      isError={isError}
      errorText={errorText}
      htmlFor={htmlFor}
    >
      <div className="flex gap-2">
        {values.map((v, index) => (
          <>
            <input
              id={htmlFor}
              key={index}
              ref={(el) => {
                inputsRef.current[index] = el;
              }}
              type="text"
              placeholder={placeholder && placeholder.slice(0, VariantLengths)[index]}
              maxLength={1}
              value={v}
              onChange={(e) => handleChange(e.target.value, index)}
              onKeyDown={(e) => handleKeyDown(e, index)}
              className={cn(
                CodeInputFieldVariants({ size, color, hasError: isError, disabled }),
                className
              )}
              disabled={disabled ?? true}
            />
            {VariantLengths === 6 && index === 2 && (
              <p className={cn('text-4xl mt-2', disabled ? 'text-black-50' : 'text-black-300')}>
                -
              </p>
            )}
          </>
        ))}
      </div>
    </InputWrapper>
  );
});

CodeInputField.displayName = 'CodeInputField';
