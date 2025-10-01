import * as React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../utils';

type BaseTextareaAtrributes = Pick<
  React.TextareaHTMLAttributes<HTMLTextAreaElement>,
  'id' | 'className' | 'onInput' | 'onFocus' | 'value' | 'placeholder' | 'disabled' | 'onBlur'
>;

const TextareaVariants = cva(
  `focus:ring-transparent text-sm focus:outline-none transition-all rounded-lg border focus:ring-4 focus:ring-brand-blue-100 p-2`,
  {
    variants: {
      hasError: {
        true: '!border-error-text focus:ring-error-bg',
        false: '!border-white-800 focus:!border-brand-blue-400'
      },
      variant: {
        light: 'bg-white-50',
        grey: 'bg-background-inputs'
      },
      size: {
        sm: 'w-[240px] h-24',
        md: 'w-[320px] h-28',
        lg: 'w-[400px] h-32',
        full: 'w-full h-32'
      },
      resizable: {
        true: '!resize',
        x: '!resize-x',
        y: '!resize-y',
        false: '!resize-none'
      }
    },
    defaultVariants: {
      resizable: 'y',
      hasError: false,
      variant: 'grey',
      size: 'md'
    }
  }
);

type TextareaVariantProps = VariantProps<typeof TextareaVariants>;

export interface TextareaProps extends TextareaVariantProps, BaseTextareaAtrributes {}

export const Textarea = React.forwardRef<HTMLTextAreaElement, TextareaProps>((props, ref) => {
  const {
    size,
    resizable,
    variant,
    id,
    hasError,
    placeholder,
    value,
    className,
    disabled,
    onBlur
  } = props;

  return (
    <textarea
      id={id}
      ref={ref}
      disabled={disabled}
      placeholder={placeholder ?? 'Enter a description'}
      defaultValue={value}
      onBlur={onBlur}
      className={cn(TextareaVariants({ hasError, resizable, size, variant }), className)}
    />
  );
});

Textarea.displayName = 'Textarea';
